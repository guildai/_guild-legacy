-module(hubc_train_cmd).

-export([parser/0, main/2]).

parser() ->
    cli:parser(
      "guild train",
      "[OPTION]... [MODEL]",
      "Trains MODEL if specified, otherwise trains the default model.\n"
      "\n"
      "The default model is the first model defined in the project config.\n"
      "\n"
      "Models are trained by their configured 'train' operation, which must"
      "be specified.\n"
      "\n"
      "If the specified model does not exist or cannot be trained because "
      "it does not define a train operation, the command exits with an error.",
      hubc_cmd_support:project_options([flag_support]) ++ train_options(),
      [{pos_args, {0, 1}}]).

train_options() ->
    [{preview, "--preview", "print training details but do not train", [flag]},
     {debug, "--debug", "log errors", [hidden, flag]}].

main(Opts, Args) ->
    Project = hubc_cmd_support:project_from_opts(Opts),
    Model = model_arg(Args),
    train_or_preview(init_op(Model, Project), Opts).

model_arg([]) -> undefined;
model_arg([Name]) -> Name.

init_op(ModelName, Project) ->
    ModelSection = hubc_cmd_support:model_section_for_name(ModelName, Project),
    MaybeRuntime = hubc_runtime:for_section(ModelSection, Project),
    init_op(MaybeRuntime, ModelSection, Project).

init_op({ok, Runtime}, Model, Project) ->
    hubc_runtime:init_train_op(Runtime, Model, Project);
init_op({error, Err}, _Model, _Project) ->
    {error, Err}.

train_or_preview({ok, Op}, Opts) ->
    case proplists:get_bool(preview, Opts) of
        false -> train(Op, Opts);
        true -> preview(Op)
    end;
train_or_preview({error, Err}, _Opts) ->
    init_op_error(Err).

train(Op, Opts) ->
    init_error_tty(Opts),
    hubc_cmd_support:exec_operation(hubc_train_op, Op, [exec, json]).

init_error_tty(Opts) ->
    %% The train operation directs the error logger to a file as a
    %% matter of record. We typicall disable tty here to insulate the
    %% user from the chatty gen_server logging (gen_server logs errors
    %% whenever a process exits abnormally - and we use abnormal exits
    %% to track when our monitored operation exits with non zero). To
    %% disable this behavior we will turn tty on when the debug option
    %% is provided.
    error_logger:tty(proplists:get_bool(debug, Opts)).

preview(Op) ->
    #{logdir:=LogDir,
      cmd:={CmdArgs, CmdOpts}} = hubc_operation:info(Op),
    hubc_cli:out_par(
      "This command will use the settings below. Note that the log "
      "directory contains a time stamp and will be different when "
      "the command is run.~n~n"),
    hubc_cli:out("Log directory (will be created):~n~n  ~s~n~n", [LogDir]),
    hubc_cli:out("Command:~n~n  ~s~n~n", [hubc_util:format_cmd_args(CmdArgs)]),
    print_cmd_env(proplists:get_value(env, CmdOpts)).

print_cmd_env(undefined) -> ok;
print_cmd_env(Env) ->
    hubc_cli:out("Environment:~n~n"),
    lists:foreach(
      fun({Name, Val}) -> hubc_cli:out("  ~s=~s~n~n", [Name, Val]) end,
      Env).

init_op_error(trainable) ->
    hubc_cli:cli_error(
      "model does not support a train operation\n"
      "Try 'guild train --help' for more information.");
init_op_error(Err) ->
    hubc_cli:cli_error(hubc_cmd_support:runtime_error_msg(Err)).
