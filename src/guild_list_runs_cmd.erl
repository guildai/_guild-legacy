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

-module(guild_list_runs_cmd).

-export([parser/0, main/2]).

%% ===================================================================
%% Parser
%% ===================================================================

parser() ->
    cli:parser(
      "guild list-runs",
      "[OPTION]...",
      "List project runs.",
      guild_cmd_support:project_options(),
      [{pos_args, 0}]).

%% ===================================================================
%% Main
%% ===================================================================

main(Opts, []) ->
    Project = guild_cmd_support:project_from_opts(Opts),
    print_runs(guild_run:runs_for_project(Project)).

print_runs(Runs) ->
    lists:foreach(fun print_run/1, Runs).

print_run(R) ->
    Dir = guild_run:dir(R),
    guild_cli:out(io_lib:format("~s~n", [Dir])).
