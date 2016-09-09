-module(hubc_list_run_stats_cmd).

-export([parser/0, main/2]).

%% ===================================================================
%% Parser
%% ===================================================================

parser() ->
    cli:parser(
      "guild list-run-stats",
      "[OPTION]... [LOGDIR]",
      "List stats for a run in LOGIDR or the latest using --latest-run.\n"
      "\n"
      "Use 'list-runs' to list runs that can be used for LOGDIR.",
      list_opts() ++ hubc_cmd_support:project_options(),
      [{pos_args, {0, 1}}]).

list_opts() ->
    [{latest, "--latest-run", "use the most recent run", [flag]}].

%% ===================================================================
%% Main
%% ===================================================================

main(Opts, Args) ->
    LogDir = logdir(Opts, Args),
    case hubc_run_db:open(LogDir) of
        ok -> print_stat_names(hubc_run_db:series_keys(LogDir));
        {error, missing} -> hubc_cli:cli_error(missing_db_error(LogDir))
    end.

logdir(Opts, [LOGDIR]) ->
    assert_not_latest_flag(Opts),
    LOGDIR;
logdir(Opts, []) ->
    assert_latest_flag(Opts),
    Project = hubc_cmd_support:project_from_opts(Opts),
    hubc_cmd_support:latest_logdir(Project).

assert_not_latest_flag(Opts) ->
    case proplists:get_bool(latest, Opts) of
        false -> ok;
        true ->
            hubc_cli:cli_error(
              "--latest-run cannot be used with LOGDIR\n"
              "Try 'guild list-run-stats --help' for more information.")
    end.

assert_latest_flag(Opts) ->
    case proplists:get_bool(latest, Opts) of
        true -> ok;
        false ->
            hubc_cli:cli_error(
              "either LOGDIR or --latest-run is required\n"
              "Try 'guild list-run-stats --help' for more information.")
    end.

print_stat_names({ok, Names}) ->
    Print = fun(N) -> io:format("~s~n", [N]) end,
    lists:foreach(Print, Names).

missing_db_error(LogDir) ->
    io_lib:format("~s does not contain run data", [LogDir]).
