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
         required_latest_package/1]).

parser() ->
    cli:parser(
      "guild init",
      "[OPTION]... [DIR] [VAR=VALUE]...",
      "Initialize a Guild project.\n"
      "\n"
      "By default Guild creates an annotated project file in DIR (defaults "
      "to current directory) or prints to standard output if --print is "
      "specified. Specify a template to create a full configured project "
      "from a source package. TEMPLATE must be the name of an installed source "
      "package.\n"
      "\n"
      "When specifying a template, specify variables using VAR=VALUE "
      "arguments. To list variables available for a template, use "
      "--list-variables. Some variables are required and are indicated as such "
      "in the list. If a required variable is not specified, the command "
      "will fail with an error message.",
      init_opts(),
      [{pos_args, {0, 1}}]).

init_opts() ->
    [{template, "--template",
      "propject template", [{metavar, "TEMPLATE"}]}].

%% ===================================================================
%% Main
%% ===================================================================

main(Opts, Args) ->
    ProjectDir = project_dir_from_args(Args),
    assert_project_dir_empty(ProjectDir),
    Template = template_option(Opts),
    Vars = apply_cmd_vars(vars_from_args(Args), ProjectDir),
    handle_template_option(Template, ProjectDir, Vars).

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

template_option(Opts) ->
    proplists:get_value(template, Opts).

vars_from_args(Args) ->
    lists:foldl(fun acc_vars/2, [], Args).

apply_cmd_vars(Base, ProjectDir) ->
    [{project_dir, dir_basename(ProjectDir)}|Base].

dir_basename(".") -> dir_basename("");
dir_basename(Dir) -> filename:basename(filename:absname(Dir)).

acc_vars(Arg, Acc) ->
    case re:split(Arg, "=", [{parts, 2}, {return, list}]) of
        [Name, Val] -> [{Name, Val}|Acc];
        [] -> Acc
    end.

handle_template_option(undefined, ProjectDir, Vars) ->
    init_annotated_project(ProjectDir, Vars);
handle_template_option(Template, ProjectDir, Vars) ->
    init_project_from_template(ProjectDir, Template, Vars).

init_annotated_project(ProjectDir, Vars) ->
    Template = annotated_template(),
    Rendered = render_template(Template, Vars),
    write_guild_file(ProjectDir, Rendered).

annotated_template() ->
    filename:join(guild_app:priv_dir("projects"), "annotated").

render_template(Template, Vars) ->
    Mod = guild_init_project_template,
    Opts = [{default_libraries, [?MODULE]}],
    try guild_dtl_util:compile_template(Template, Mod, Opts) of
        ok ->
            handle_render(Mod:render(Vars))
    catch
        error:{template_compile, _} ->
            bad_template_error()
    end.

handle_render({ok, Bin}) ->
    Bin;
handle_render({error, Msg}) ->
    guild_cli:cli_error(Msg).

bad_template_error() ->
    guild_cli:cli_error(
      "unable to initialize project - template contains errors").

write_guild_file(Dir, Bin) ->
    Path = guild_project:project_file_for_dir(Dir),
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, Bin).

init_project_from_template(ProjectDir, Template, Vars) ->
    Source = installed_package_path(Template),
    GuildIn = validated_source_template(Source, Template),
    Rendered = render_template(GuildIn, Vars),
    guild_app:init_support([exec]),
    Bin = guild_app:priv_bin("guild-init"),
    Args = [Bin, Source, ProjectDir],
    Env = init_cmd_env(Rendered),
    guild_cmd_support:exec_run(Args, [{env, Env}]).

installed_package_path(Template) ->
    filename:join([os:getenv("HOME"), ".guild/packages", Template]).

validated_source_template(Path, Template) ->
    GuildIn = filename:join(Path, "Guild.in"),
    assert_source_dir(Path, Template),
    assert_guild_in(GuildIn, Template),
    GuildIn.

assert_source_dir(Path, Template) ->
    case filelib:is_dir(Path) of
        true -> ok;
        false -> missing_package_error(Template)
    end.

missing_package_error(Name) ->
    guild_cli:cli_error(
      io_lib:format(
        "~s is not installed~n"
        "Try 'guild install ~s'",
        [Name, Name])).

assert_guild_in(Path, Template) ->
    case filelib:is_file(Path) of
        true -> ok;
        false -> missing_guild_in_error(Template)
    end.

missing_guild_in_error(Name) ->
    guild_cli:cli_error(
      io_lib:format(
        "~s is not a source package (missing Guild.in)",
        [Name])).

init_cmd_env(GuildProject) ->
    [{"GUILD_PROJECT", binary_to_list(iolist_to_binary(GuildProject))}].

%% ===================================================================
%% Template support
%% ===================================================================

version() -> 1.

inventory(filters) ->
    [warn_if_empty,
     required,
     required_latest_package];
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

required_latest_package(Val) ->
    PkgHome = guild_app:user_dir("packages"),
    Glob = filename:join(PkgHome, [Val, "-*"]),
    case filelib:wildcard(Glob) of
        [] -> throw(missing_package_msg(Val));
        Matches -> latest_package_from_paths(Matches)
    end.

missing_package_msg(Name) ->
    io_lib:format(
      "there are no installed packages matching '~s'",
      [Name]).

latest_package_from_paths(Paths) ->
    Names = [filename:basename(Path) || Path <- Paths],
    [Latest|_] = lists:reverse(lists:sort(Names)),
    Latest.
