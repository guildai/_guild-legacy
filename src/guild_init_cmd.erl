%% Copyright 2016-2017 TensorHub, Inc.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(guild_init_cmd).

-export([parser/0, main/2]).

-behavior(erlydtl_library).

-export([version/0, inventory/1, warn_if_empty/2, required/2,
         latest_package/1]).

-record(template, {pkg, src, project}).

parser() ->
    cli:parser(
      "guild init",
      "[OPTION]... [DIR] [VAR=VALUE]...",
      "Initialize a Guild project.\n"
      "\n"
      "By default Guild creates an annotated project file in DIR (defaults "
      "to current directory). Specify a template to create a full configured "
      "project from a source package. TEMPLATE must be the name of an "
      "installed source package.\n"
      "\n"
      "When specifying a template, specify variables using VAR=VALUE "
      "arguments. To list variables available for a template, use "
      "--print-vars. Some variables are required and are indicated as such "
      "in the list. If a required variable is not specified, the command "
      "will fail with an error message.",
      init_opts(),
      [{pos_args, {0, any}}]).

init_opts() ->
    [{template, "--template",
      "propject template", [{metavar, "TEMPLATE"}]},
     {print_vars, "--print-vars", "print variables used by TEMPLATE", [flag]}].

%% ===================================================================
%% Main
%% ===================================================================

main(Opts, Args) ->
    Template = resolve_template(Opts),
    case print_vars_flag(Opts) of
        true -> print_vars(Template);
        false -> init_project(Template, Args)
    end.

resolve_template(Opts) ->
    case proplists:get_value(template, Opts) of
        undefined -> annotated_project_template();
        Pkg -> resolve_package_template(Pkg)
    end.

annotated_project_template() ->
    #template{project=annotated_project()}.

annotated_project() ->
    Path = filename:join(guild_app:priv_dir("projects"), "annotated"),
    {ok, Project} = guild_project:from_file(Path),
    Project.

resolve_package_template(Pkg) ->
    case guild_package_util:latest_package_path(Pkg) of
        {ok, Path} -> try_package_template_from_path(Path, Pkg);
        error -> missing_package_error(Pkg)
    end.

try_package_template_from_path(PkgDir, Pkg) ->
    TemplatePath = filename:join(PkgDir, "Guild.in"),
    case guild_project:from_file(TemplatePath) of
        {ok, Project} ->
            #template{pkg=Pkg, src=PkgDir, project=Project};
        {error, {missing_project_file, _}} ->
            missing_guild_in_error(PkgDir)
    end.

missing_package_error(Name) ->
    guild_cli:cli_error(
      io_lib:format(
        "~s is not installed~n"
        "Try 'guild list-packages' for a list",
        [Name])).

missing_guild_in_error(PkgDir) ->
    guild_cli:cli_error(
      io_lib:format(
        "~s is not a source package (missing Guild.in)",
        [PkgDir])).

print_vars_flag(Opts) ->
    proplists:get_bool(print_vars, Opts).

print_vars(Template) ->
    lists:foreach(fun print_var/1, var_defs(Template)).

var_defs(#template{project=Project}) ->
    guild_project:sections(Project, ["var"]).

-define(help_inset, 20).

print_var({["var", Name], Attrs}) ->
    print_var_name(Name),
    print_var_help(Attrs, ?help_inset - length(Name)),
    io:format("~n").

print_var_name(Name) ->
    io:format(Name).

print_var_help(Attrs, Indent) ->
    case var_help(Attrs) of
        undefined -> ok;
        Help ->
            print_spaces(Indent),
            io:format(Help)
    end.

print_spaces(N) when N > 0 ->
    io:format(" "),
    print_spaces(N - 1);
print_spaces(_) ->
    ok.

var_help(Attrs) ->
    proplists:get_value("help", Attrs).

init_project(Template, Args) ->
    ProjectDir = project_dir_from_args(Args),
    assert_project_dir_empty(ProjectDir),
    Vars = validated_template_vars(Args, ProjectDir, Template),
    maybe_copy_template_src(Template, ProjectDir),
    write_template_as_guild_file(Template, Vars, ProjectDir).

project_dir_from_args([Arg|Rest]) ->
    case lists:member($=, Arg) of
        true -> project_dir_from_args(Rest);
        false -> Arg
    end;
project_dir_from_args([]) ->
    filename:absname("").

assert_project_dir_empty(Dir) ->
    case file:list_dir(Dir) of
        {error, enoent} -> ok;
        {ok, []} -> ok;
        {ok, _} -> project_dir_not_empty_error(Dir)
    end.

project_dir_not_empty_error(Dir) ->
    guild_cli:cli_error(
      io_lib:format(
        "'~s' is not an empty directory\n"
        "Try 'guild init' with a new or empty directory.",
        [Dir])).

validated_template_vars(Args, ProjectDir, Template) ->
    BaseVars = base_vars(ProjectDir),
    UserVars = vars_from_args(Args),
    validate_user_vars(UserVars, Template),
    UserAndDefaultVars = apply_default_vars(UserVars, Template, BaseVars),
    UserAndDefaultVars ++ BaseVars.

base_vars(ProjectDir) ->
    [{project_dir, dir_basename(ProjectDir)}].

dir_basename(".") -> dir_basename("");
dir_basename(Dir) -> filename:basename(filename:absname(Dir)).

vars_from_args(Args) ->
    lists:foldl(fun acc_vars/2, [], Args).

acc_vars(Arg, Acc) ->
    case re:split(Arg, "=", [{parts, 2}, {return, list}]) of
        [Name, Val] -> [{Name, Val}|Acc];
        [_] -> Acc
    end.

validate_user_vars(Vars, Template) ->
    VarDefs = var_defs(Template),
    Validate = fun(Def) -> check_required_var(Def, Vars, Template) end,
    lists:foreach(Validate, VarDefs).

check_required_var({["var", Name], Attrs}, Vars, Template) ->
    case is_var_required(Attrs) andalso is_var_missing(Name, Vars) of
        true -> missing_required_var_error(Name, Template);
        false -> ok
    end.

is_var_required(Attrs) ->
    proplists:get_value("required", Attrs) == "yes".

is_var_missing(Name, Vars) ->
    proplists:get_value(Name, Vars, undefined) == undefined.

missing_required_var_error(Name, #template{pkg=Pkg}) ->
    guild_cli:cli_error(
      io_lib:format(
        "project template for ~s requires '~s' variable~n"
        "Try 'guild init --template=~s --print-vars' for help",
        [Pkg, Name, Pkg])).

apply_default_vars(UserVars, Template, BaseVars) ->
    VarDefs = var_defs(Template),
    ApplyMissing = fun(Def, Vars) -> apply_missing_var(Vars, Def, BaseVars) end,
    lists:foldl(ApplyMissing, UserVars, VarDefs).

apply_missing_var(Vars, {["var", Name], _}=Def, BaseVars) ->
    case is_var_missing(Name, Vars) of
        true -> maybe_apply_default(Vars, Def, BaseVars);
        false -> Vars
    end.

maybe_apply_default(Vars, {["var", Name], Attrs}, BaseVars) ->
    case var_default(Attrs) of
        undefined -> Vars;
        Default -> [{Name, render_default(Default, BaseVars)}|Vars]
    end.

var_default(Attrs) ->
    proplists:get_value("default", Attrs).

render_default(Val, Vars) ->
    Mod = guild_init_var_template,
    Opts = [return, {out_dir, false}],
    case erlydtl:compile_template(Val, Mod, Opts) of
        {ok, _, []} ->
            handle_default_render(Mod:render(Vars), Val);
        {error, Err} ->
            handle_default_compile_error(Err, Val)
    end.

handle_default_compile_error(Err, Orig) ->
    guild_cli:warn("~p", [Err]),
    Orig.

handle_default_render({ok, Rendered}, _) ->
    Rendered;
handle_default_render({error, Err}, Orig) ->
    guild_cli:warn("~p~n", [Err]),
    Orig.

maybe_copy_template_src(#template{src=undefined}, _Dest) -> ok;
maybe_copy_template_src(#template{src=Src}, Dest) ->
    guild_app:init_support([exec]),
    Bin = guild_app:priv_bin("guild-init"),
    Args = [Bin, Src, Dest],
    guild_cmd_support:exec_run(Args, []).

write_template_as_guild_file(#template{project=Project}, Vars, ProjectDir) ->
    Rendered = render_project_template(Project, Vars),
    Stripped = strip_vars(Rendered),
    write_guild_file(ProjectDir, Stripped).

render_project_template(Project, Vars) ->
    ProjectSrc = guild_project:project_file(Project),
    Mod = guild_init_project_template,
    Opts = [{default_libraries, [?MODULE]}],
    try guild_dtl_util:compile_template(ProjectSrc, Mod, Opts) of
        ok -> handle_render(Mod:render(Vars))
    catch
        error:{template_compile, _} -> bad_template_error()
    end.

handle_render({ok, Bin}) ->
    Bin;
handle_render({error, Msg}) ->
    guild_cli:cli_error(Msg).

bad_template_error() ->
    guild_cli:cli_error(
      "unable to initialize project - template contains errors").

strip_vars(Bin) ->
    guild_project_util:strip_sections(Bin, ["var"]).

write_guild_file(Dir, Bin) ->
    Path = filename:join(Dir, "Guild"),
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, Bin).

%% ===================================================================
%% Template support
%% ===================================================================

version() -> 1.

inventory(filters) ->
    [warn_if_empty,
     required,
     latest_package];
inventory(tags) ->
    [].

warn_if_empty(Val, Msg) when Val == ""; Val == undefined ->
    guild_cli:warn("WARNING: ~s~n", [Msg]),
    Val;
warn_if_empty(Val, _Msg) ->
    Val.

required(Val, Msg) when Val == ""; Val == undefined ->
    throw(Msg);
required(Val, _Msg) ->
    Val.

latest_package(Val) ->
    case guild_package_util:latest_package_path(Val) of
        {ok, Path} -> filename:basename(Path);
        error -> missing_package_msg(Val)
    end.

missing_package_msg(Name) ->
    io_lib:format(
      "there are no installed packages matching '~s'",
      [Name]).
