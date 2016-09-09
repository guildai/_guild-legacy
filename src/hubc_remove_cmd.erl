-module(hubc_remove_cmd).

-export([parser/0, main/2]).

%% ===================================================================
%% Parser
%% ===================================================================

parser() ->
    cli:parser(
      "guild remove",
      "[OPTION]... LOGDIR",
      "Remove a training run by deleting its log directory.",
      [],
      [{pos_args, 1}]).

%% ===================================================================
%% Main
%% ===================================================================

main(_Opts, [Dir]) ->
    maybe_remove_rundir(hubc_run:is_run(Dir), Dir).

maybe_remove_rundir(true, Dir) ->
    remove_dir(Dir);
maybe_remove_rundir(false, Dir) ->
    print_invalid_rundir(Dir).

remove_dir(Dir) ->
    "" = os:cmd(["rm -rf ", quote_dir(Dir)]),
    ok.

quote_dir(Dir) ->
    Escaped = re:replace(Dir, "\"", "\\\\\"", [global, {return, list}]),
    "\"" ++ Escaped ++ "\"".

print_invalid_rundir(Dir) ->
    hubc_cli:cli_error(
      io_lib:format(
        "'~s' is not a TensorHub run log directory~nn"
        "Try 'guild list runs' for a list of runs.",
        [Dir])).
