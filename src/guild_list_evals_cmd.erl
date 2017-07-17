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

-module(guild_list_evals_cmd).

-export([parser/0, main/2]).

%% ===================================================================
%% Parser
%% ===================================================================

parser() ->
    cli:parser(
      "guild list-evals",
      "[OPTION]... [RUNDIR]",
      "List evaluations for a run in RUNIDR or the latest using --latest-run.\n"
      "\n"
      "Use 'guild list-runs' to list runs that can be used for RUNDIR.\n"
      "\n"
      "To list the evalutions for all runs, use --all-runs.",
      list_eval_options() ++ guild_cmd_support:project_options([latest_run]),
      [{pos_args, {0, 1}}]).

list_eval_options() ->
    [{all_runs, "-A, --all-runs", "list evaluations for all runs", [flag]}].

%% ===================================================================
%% Main
%% ===================================================================

main(Opts, Args) ->
    RunDirs = rundirs_for_args(Opts, Args),
    print_paths(eval_paths(RunDirs)).

rundirs_for_args(Opts, Args) ->
    case proplists:get_bool(all_runs, Opts) of
        true ->
            all_eval_dirs(Opts);
        false ->
            [guild_cmd_support:run_db_for_args(Opts, Args)]
    end.

all_eval_dirs(Opts) ->
    Project = guild_cmd_support:project_from_opts(Opts),
    [guild_run:dir(Run) || Run <- guild_run:runs_for_project(Project)].

eval_paths(RunDirs) ->
    Paths = lists:foldl(fun eval_paths_acc/2, [], RunDirs),
    lists:sort(Paths).

eval_paths_acc(RunDir, Acc0) ->
    lists:sort(
      lists:foldl(
        fun(Path, Acc) -> [Path|Acc] end,
        Acc0,
        filelib:wildcard(filename:join(RunDir, "eval-*")))).

print_paths(Paths) ->
    lists:foreach(fun(P) -> io:format("~s~n", [P]) end, Paths).
