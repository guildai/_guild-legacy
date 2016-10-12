%% Copyright 2106 TensorHub, Inc.
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

-define(default_template, "annotated").

-behavior(erlydtl_library).

-export([version/0, inventory/1]).
-export([warn_if_empty/2]).

parser() ->
    cli:parser(
      "guild init",
      "[OPTION]... [DIR]",
      "Initialize a Guild project.\n"
      "\n"
      "By default Guild creates a commented project stub in DIR. Specify "
      "a template to create a specific project type. Guild provides "
      "preconfigured templates that can be referenced by name (see list below) "
      "or you may specify a full path to your own Guild template.\n"
      "\n"
      "Available templates:\n"
      "\n"
      "!!  tensorflow     use for typical TensorFlow project\n",
      init_opts(),
      [{pos_args, {0, 1}}]).

init_opts() ->
    [{name, "--name",
     "project name (defaults to directory name)"},
     {template, "--template",
      "template Guild propject", [{metaval, "GUILD_FILE"}]},
     {train_cmd, "--train-cmd",
      "script used to train the model", []}].

%% ===================================================================
%% Main
%% ===================================================================

main(Opts, Args) ->
    Dir = project_dir_from_args(Args),
    assert_not_project(Dir),
    Template = template_source(Opts),
    Vars = template_vars(Dir, Opts),
    Rendered = render_template(Template, Vars),
    write_guild_file(Dir, Rendered),
    guild_cli:out("Initialized Guild project at '~s'~n", [Dir]).

project_dir_from_args([])    -> filename:absname("");
project_dir_from_args([Dir]) -> filename:absname(Dir).

assert_not_project(Dir) ->
    case filelib:is_file(guild_project:project_file_for_dir(Dir)) of
        true -> existing_project_error(Dir);
        false -> ok
    end.

existing_project_error(Dir) ->
    guild_cli:cli_error(
      io_lib:format(
        "project exists at '~s'\n"
        "Move or rename the Guild project there and try 'guild init' again.",
        [Dir])).

template_source(Opts) ->
    resolve_template(template_opt(Opts)).

template_opt(Opts) ->
    Template = proplists:get_value(template, Opts, ?default_template),
    case filelib:is_file(Template) of
        true -> {path, Template};
        false -> {name, Template}
    end.

resolve_template({path, Path}) -> Path;
resolve_template({name, Name}) ->
    Path = filename:join(project_templates_dir(), Name),
    case filelib:is_file(Path) of
        true -> Path;
        false -> bad_named_template_error(Name)
    end.

project_templates_dir() ->
    guild_app:priv_dir("projects").

bad_named_template_error(Name) ->
    guild_cli:cli_error(
      io_lib:format(
        "unknown template '~s'\n"
        "Try 'guild init --help' for more information.", [Name])).

template_vars(Dir, Opts) ->
    [{project_name, project_name_var(Dir, Opts)},
     {train_cmd, train_cmd_var(Dir, Opts)}].

project_name_var(Dir, Opts) ->
    case proplists:get_value(name, Opts) of
        undefined -> filename:basename(Dir);
        Name -> Name
    end.

train_cmd_var(_Dir, Opts) ->
    proplists:get_value(train_cmd, Opts, "").

render_template(Template, Vars) ->
    Mod = guild_init_project_template,
    Opts = [{default_libraries, [?MODULE]}],
    try guild_dtl_util:compile_template(Template, Mod, Opts) of
        ok -> handle_render(Mod:render(Vars))
    catch
        error:{template_compile, _} -> bad_template_error()
    end.

handle_render({ok, Bin}) -> Bin.

bad_template_error() ->
    guild_cli:cli_error(
      "unable to initialize project - template contains errors").

write_guild_file(Dir, Bin) ->
    Path = guild_project:project_file_for_dir(Dir),
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, Bin).

%% ===================================================================
%% Template support
%% ===================================================================

version() -> 1.

inventory(filters) -> [warn_if_empty];
inventory(tags)    -> [].

warn_if_empty(Val, Msg) when Val == ""; Val == undefined ->
    guild_cli:warn("WARNING: ~s~n", [Msg]),
    Val;
warn_if_empty(Val, _Msg) ->
    Val.
