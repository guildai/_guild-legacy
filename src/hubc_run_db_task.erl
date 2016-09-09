-module(hubc_run_db_task).

-behavior(e2_task).

-export([start_link/4]).

-export([init/1, handle_task/1]).

-record(state, {logdir, flags, cmd}).

start_link(_Op, LogDir, Flags, Cmd) ->
    e2_task:start_link(?MODULE, [LogDir, Flags, Cmd], []).

init([LogDir, Flags, Cmd]) ->
    hubc_proc:reg(optask, self()),
    {ok, #state{logdir=LogDir, flags=Flags, cmd=Cmd}}.

handle_task(State) ->
    open_db(State),
    log_initial_state(State),
    {stop, normal}.

open_db(#state{logdir=LogDir}) ->
    ok = hubc_run_db:open(LogDir, [create_if_missing]).

log_initial_state(State) ->
    log_flags(State),
    log_cmd(State).

log_flags(#state{logdir=LogDir, flags=Flags}) ->
    ok = hubc_run_db:log_flags(LogDir, Flags).

log_cmd(#state{logdir=LogDir, cmd=Cmd}) ->
    ok = hubc_run_db:log_attrs(LogDir, cmd_attrs(Cmd)).

cmd_attrs({Args, Opts}) ->
    lists:foldl(fun cmd_attr_acc/2, [], [{args, Args}|Opts]).

cmd_attr_acc({args, Args}, Acc) ->
    Formatted = string:join(Args, "\n"),
    [{"cmd_args", Formatted}|Acc];
cmd_attr_acc({cwd, Dir}, Acc) ->
    [{"cmd_working_dir", Dir}|Acc];
cmd_attr_acc({env, Env}, Acc) ->
    Formatted = string:join([[Name, "=", Val] || {Name, Val} <- Env], "\n"),
    [{"cmd_env", Formatted}|Acc].
