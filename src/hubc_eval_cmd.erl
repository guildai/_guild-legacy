-module(hubc_eval_cmd).

-export([parser/0, main/2]).

parser() ->
    cli:parser(
      "guild evaluate",
      "[OPTION]... [LOGDIR]",
      "Evaluate a trained model in LOGIDR or the latest using --latest-run.\n"
      "\n"
      "Use 'list-runs' to list runs that can be used for LOGDIR.\n"
      "\n"
      "The model applicable to the run must have an evaluate operation "
      "operation defined.",
      eval_opts() ++ hubc_cmd_support:project_options([flag_support]),
      [{pos_args, {0, 1}}]).

eval_opts() ->
    [{latest, "--latest-run", "use the most recent run", [flag]}].

main(Opts, Args) ->
    eval(init_op(Opts, Args)).

init_op(Opts, Args) ->
    Project = hubc_cmd_support:project_from_opts(Opts),
    LogDir = logdir_from_args(Args, Opts, Project),
    Run = run_for_logdir(LogDir),
    Model = model_for_run(Run, Project),
    MaybeRuntime = hubc_runtime:for_section(Model, Project),
    try_init_op(MaybeRuntime, Run, Model, Project).

logdir_from_args([LogDir], Opts, _Project) ->
    assert_not_latest_flag(Opts),
    LogDir;
logdir_from_args([], Opts, Project) ->
    assert_latest_flag(Opts),
    LogDir = hubc_cmd_support:latest_logdir(Project),
    hubc_cli:out("Evaluating run ~s~n", [LogDir]),
    LogDir.

assert_not_latest_flag(Opts) ->
    case proplists:get_bool(latest, Opts) of
        false -> ok;
        true ->
            hubc_cli:cli_error(
              "--latest-run cannot be used with LOGDIR\n"
              "Try 'guild evaluate --help' for more information.")
    end.

assert_latest_flag(Opts) ->
    case proplists:get_bool(latest, Opts) of
        true -> ok;
        false ->
            hubc_cli:cli_error(
              "either LOGDIR or --latest-run is required\n"
              "Try 'guild evaluate --help' for more information.")
    end.

run_for_logdir(LogDir) ->
    case hubc_run:run_for_logdir(LogDir) of
        {ok, Run} -> Run;
        error -> bad_logdir_error(LogDir)
    end.

bad_logdir_error(LogDir) ->
    hubc_cli:cli_error(io_lib:format("cannot a find run at ~s", [LogDir])).

model_for_run(Run, Project) ->
    hubc_cmd_support:model_section_for_name(run_model_name(Run), Project).

run_model_name(Run) ->
    case hubc_run:attr(Run, "model") of
        {ok, <<>>} -> undefined;
        {ok, Name} -> Name;
        error      -> undefined
    end.

try_init_op({ok, Runtime}, LogDir, Section, Project) ->
    hubc_runtime:init_eval_op(Runtime, LogDir, Section, Project);
try_init_op({error, Err}, _LogDir, _Section, _Project) ->
    {error, Err}.

eval({ok, Op}) ->
    hubc_cmd_support:exec_operation(hubc_eval_op, Op, [exec]);
eval({error, Err}) ->
    init_op_error(Err).

init_op_error(evaluatable) ->
    hubc_cli:cli_error(
      "model does not support an eval operation\n"
      "Try 'guild evaluate --help' for more information.");
init_op_error(Err) ->
    hubc_cli:cli_error(hubc_cmd_support:runtime_error_msg(Err)).
