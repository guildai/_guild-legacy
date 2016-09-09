-module(hubc_list_runs_cmd).

-export([parser/0, main/2]).

%% ===================================================================
%% Parser
%% ===================================================================

parser() ->
    cli:parser(
      "guild list-runs",
      "[OPTION]...",
      "List project runs.",
      hubc_cmd_support:project_options(),
      [{pos_args, 0}]).

%% ===================================================================
%% Main
%% ===================================================================

main(Opts, []) ->
    Project = hubc_cmd_support:project_from_opts(Opts),
    print_runs(hubc_run:runs_for_project(Project)).

print_runs(Runs) ->
    lists:foreach(fun print_run/1, Runs).

print_run(R) ->
    Dir = hubc_run:dir(R),
    hubc_cli:out(io_lib:format("~s~n", [Dir])).
