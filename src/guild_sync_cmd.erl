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

-module(guild_sync_cmd).

-export([parser/0, main/2]).

-define(default_port, 6555).

%% ===================================================================
%% Parser
%% ===================================================================

parser() ->
    cli:parser(
      "guild sync",
      "[OPTION]... DEPOT",
      "Sync a project to a depot.\n"
      "\n"
      "NOTE: This is an experimental feature.\n"
      "\n"
      "DEPOT must be the path to a local Guild depot.",
      sync_options() ++ guild_cmd_support:project_options(),
      [{pos_args, 1}]).

sync_options() -> [].

%% ===================================================================
%% Main
%% ===================================================================

main(Opts, [DepotDir]) ->
    Project = guild_cmd_support:project_from_opts(Opts),
    io:format(
      "TODO: use rsync to sync Guild and runs to ~s~n", [DepotDir]).
