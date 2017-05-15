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

-module(guild_package_cmd).

-export([parser/0, main/2]).

parser() ->
    cli:parser(
      "guild package",
      "[OPTION]...",
      "Creates a Guild package.",
      package_opts() ++ guild_cmd_support:project_options([flag_support])).

package_opts() ->
    [{clean, "-c, --clean", "Removes sources before building", [flag]}].

%% ===================================================================
%% Main
%% ===================================================================

main(Opts, _Args) ->
    Project = guild_cmd_support:project_from_opts(Opts),
    guild_app:init_support([exec]),
    Bin = guild_app:priv_bin("guild-package"),
    Args = [Bin, guild_project:project_dir(Project)],
    Env = package_env(Project, Opts),
    handle_exit(guild_exec:run(Args, [{env, Env}])).

package_env(Project, Opts) ->
    project_env(Project) ++ cmd_env(Opts).

project_env(Project) ->
    Attrs = guild_project:section_attrs(Project, ["package"]),
    [{"package_" ++ Name, Val} || {Name, Val} <- Attrs].

cmd_env(Opts) ->
    OptsEnv = [{clean, "CLEAN", "1"}],
    {_, Env} = lists:foldl(fun apply_cmd_env/2, {Opts, []}, OptsEnv),
    Env.

apply_cmd_env({Flag, Name, Val}, {Opts, Env}) ->
    case proplists:get_bool(Flag, Opts) of
        true -> {Opts, [{Name, Val}|Env]};
        false -> {Opts, Env}
    end.

handle_exit({ok, []}) ->
    ok;
handle_exit({error, [{exit_status, Status}]}) ->
    {error, exec:status(Status)}.
