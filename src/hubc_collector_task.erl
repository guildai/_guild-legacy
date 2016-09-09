-module(hubc_collector_task).

-behavior(e2_task).

-export([start_link/4, start_builtin/4]).

-export([init/1, handle_task/1, handle_msg/3]).

-record(state, {op, cmd, logdir, op_exec_ospid, exec_pid, exec_ospid,
                buf, waiting, stopping}).

-define(default_repeat, 5000).
-define(stop_exec_timeout, 1000).
-define(stop_task_timeout, 5000).

%% ===================================================================
%% Start / init
%% ===================================================================

start_link(Op, Cmd, LogDir, TaskOpts) ->
    SafeOpts = ensure_repeat(TaskOpts),
    e2_task:start_link(?MODULE, [Op, Cmd, LogDir], SafeOpts).

start_builtin(Op, LocalExe, LogDir, TaskOpts) ->
    start_link(Op, local_exe_cmd(LocalExe), LogDir, TaskOpts).

local_exe_cmd(Exe) ->
    {[hubc_app:priv_bin(Exe)], []}.

ensure_repeat(Opts) ->
    case proplists:get_value(repeat, Opts) of
        undefined -> [{repeat, ?default_repeat}|Opts];
        _ -> Opts
    end.

init([Op, Cmd, LogDir]) ->
    process_flag(trap_exit, true),
    monitor(process, Op),
    hubc_proc:reg(optask, self()),
    {ok, init_state(Op, Cmd, LogDir)}.

init_state(Op, Cmd, LogDir) ->
    #state{
       op=Op,
       cmd=Cmd,
       logdir=LogDir,
       buf=hubc_collector_protocol:new_input_buffer(),
       waiting=false}.

%% ===================================================================
%% Task
%% ===================================================================

handle_task(#state{exec_pid=undefined}=State) ->
    Next = start_exec(wait_for_op(State)),
    handle_task(Next);
handle_task(State) ->
    Next = maybe_send_request(State),
    {repeat, Next}.

wait_for_op(#state{op=Op}=S) ->
    Pid = hubc_operation:wait_for_exec_start(Op),
    S#state{op_exec_ospid=Pid}.

start_exec(#state{cmd={Args, UserOpts}}=State) ->
    Opts = exec_opts(UserOpts, State),
    {ok, Pid, OSPid} = hubc_exec:run_link(Args, Opts),
    State#state{exec_pid=Pid, exec_ospid=OSPid}.

exec_opts(UserOpts, State) ->
    {BaseEnv, RestUserOpts} = split_env(UserOpts),
    Env = apply_task_env(BaseEnv, State),
    hubc_exec:apply_user_opts(
      RestUserOpts,
      [stdout, stderr, stdin, {env, Env}]).

split_env(Opts) ->
    TakeEnv = fun({env, _}) -> true; (_) -> false end,
    case lists:splitwith(TakeEnv, Opts) of
        {[], Rest} -> {[], Rest};
        {[{env, Env}|_], Rest} -> {Env, Rest}
    end.

apply_task_env(Env0, #state{logdir=LogDir, op_exec_ospid=Pid}) ->
    [{"LOGDIR", LogDir},
     {"OP_PID", integer_to_list(Pid)}|Env0].

maybe_send_request(#state{waiting=false, exec_pid=Pid}=S) ->
    hubc_exec:send(Pid, <<"\n">>),
    S#state{waiting=true};
maybe_send_request(#state{waiting=true}=State) ->
    State.

%% ===================================================================
%% Message dispatch
%% ===================================================================

handle_msg({stdout, OSPid, Bin}, noreply, #state{exec_ospid=OSPid}=State) ->
    handle_stdout(Bin, State);
handle_msg({stderr, OSPid, Bin}, noreply, #state{exec_ospid=OSPid}=State) ->
    handle_stderr(Bin, State);
handle_msg({'DOWN', _, process, Op, _}, noreply, #state{op=Op}=State) ->
    handle_op_exit(State);
handle_msg({'EXIT', Pid, Reason}, noreply, #state{exec_pid=Pid}=State) ->
    handle_exec_exit(Reason, State).

%% ===================================================================
%% Stdout
%% ===================================================================

handle_stdout(Bin, State) ->
    handle_decoded(decode_input(Bin, State), State).

decode_input(Bin, #state{buf=Buf}) ->
    hubc_collector_protocol:input(Buf, Bin).

handle_decoded({Decoded, Buf}, State) ->
    handle_decoded_(Decoded, State#state{buf=Buf}).

handle_decoded_([{_Time, eof}|Rest], State) ->
    handle_eof(Rest, State);
handle_decoded_([Decoded|Rest], State) ->
    log_decoded(Decoded, State),
    handle_decoded_(Rest, State);
handle_decoded_([], State) ->
    {noreply, State}.

handle_eof(_RestDecoded, #state{stopping=true, exec_pid=Pid}=State) ->
    hubc_exec:stop_and_wait(Pid, ?stop_exec_timeout),
    {stop, normal, State};
handle_eof(RestDecoded, State) ->
    handle_decoded_(RestDecoded, State#state{waiting=false}).

%% ===================================================================
%% Log decoded
%% ===================================================================

log_decoded({Time, {kv, KVs}}, State) ->
    log_keyvals(Time, KVs, State);
log_decoded({_Time, {ktsv, KTSVs}}, State) ->
    log_keytsvs(KTSVs, State);
log_decoded({_, {other, Term}}, _State) ->
    hubc_log:internal("Invalid collector response: ~p~n", [Term]);
log_decoded({_, {invalid, Bin}}, _State) ->
    hubc_log:internal("Invalid collector output: ~p~n", [Bin]).

log_keyvals(Time, KVs, #state{logdir=LogDir}) ->
    KTSVs = [{Key, [[Time, 0, Val]]} || {Key, Val} <- KVs],
    handle_db_result(hubc_run_db:log_series_values(LogDir, KTSVs)).

log_keytsvs(KTSVs, #state{logdir=LogDir}) ->
    handle_db_result(hubc_run_db:log_series_values(LogDir, KTSVs)).

handle_db_result(ok) -> ok;
handle_db_result({error, Err}) ->
    hubc_log:internal("Error logging series values: ~p~n", [Err]).

%% ===================================================================
%% Stderr
%% ===================================================================

handle_stderr(Bin, State) ->
    hubc_log:internal(Bin),
    {noreply, State}.

%% ===================================================================
%% Operation exited - run a final time
%% ===================================================================

handle_op_exit(State) ->
    e2_task:run_once(self()),
    timer:kill_after(?stop_task_timeout),
    {noreply, State#state{stopping=true}}.

%% ===================================================================
%% Exec exited
%% ===================================================================

handle_exec_exit(normal, State) ->
    {stop, normal, State};
handle_exec_exit({exit_status, Status}, State) ->
    {stop, {exec_exit, exec:status(Status)}, State}.
