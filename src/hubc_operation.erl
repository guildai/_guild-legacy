-module(hubc_operation).

-behavior(e2_task).

-export([new/4, start_link/2, info/1, wait_for_exec_start/1, stop/2]).

-export([init/1, handle_task/1, handle_msg/3]).

-record(op, {cmd, logdir, tasks, stream_handlers}).
-record(state, {op, exec_pid, exec_ospid, stdout_buf, stderr_buf}).

%% ===================================================================
%% New
%% ===================================================================

new(Cmd, LogDir, Tasks, StreamHandlers) ->
    #op{
       cmd=Cmd,
       logdir=LogDir,
       tasks=Tasks,
       stream_handlers=StreamHandlers}.

%% ===================================================================
%% Start / init
%% ===================================================================

start_link(Name, Op) ->
    e2_task:start_link(?MODULE, Op, [{registered, Name}]).

init(Op) ->
    hubc_proc:reg(operation, self()),
    process_flag(trap_exit, true),
    {ok, init_state(Op)}.

%% ===================================================================
%% API
%% ===================================================================

info(#op{cmd=Cmd, logdir=LogDir, tasks=Tasks, stream_handlers=SHs}) ->
    #{cmd => Cmd,
      logdir => LogDir,
      tasks => Tasks,
      stream_handlers => SHs}.

wait_for_exec_start(Op) ->
    gproc:reg({p, l, {exec_start, Op}}),
    receive
        {exec_started, Op, OSPid} -> OSPid
    end.

stop(Op, Timeout) ->
    e2_task:call(Op, {stop, Timeout}).

%% ===================================================================
%% Task impl
%% ===================================================================

handle_task(State) ->
    init_logdir(State),
    init_error_logger(State),
    start_tasks(State),
    Next = start_exec(State),
    {wait_for_msg, Next}.

init_logdir(State) ->
    ok = filelib:ensure_dir(logdir(State) ++ "/").

init_error_logger(State) ->
    error_logger:logfile({open, errors_log_path(State)}).

errors_log_path(State) ->
    filename:join(logdir(State), "errors.log").

start_tasks(State) ->
    lists:foreach(fun(Task) -> start_task(Task) end, tasks(State)).

start_task(TaskSpec) ->
    {ok, _} = hubc_optask_sup:start_task(TaskSpec, self()).

start_exec(State) ->
    {Args, UserOpts} = cmd(State),
    Opts = hubc_exec:apply_user_opts(UserOpts, [stdout, stderr]),
    {ok, Pid, OSPid} = hubc_exec:run_link(Args, Opts),
    notify_exec_started(OSPid),
    State#state{exec_pid=Pid, exec_ospid=OSPid}.

%% ===================================================================
%% Messages
%% ===================================================================

handle_msg({Stream, OSPid, Bin}, _From, #state{exec_ospid=OSPid}=State) ->
    handle_input(Stream, Bin, State);
handle_msg({'EXIT', Pid, Reason}, _From, #state{exec_pid=Pid}) ->
    handle_exec_exit(Reason);
handle_msg({stop, Timeout}, _From, State) ->
    handle_stop(Timeout, State).

handle_input(Stream, Bin, State) ->
    {Lines, Next} = stream_input(Stream, Bin, State),
    handle_stream_lines(Stream, Lines, State),
    {noreply, Next}.

stream_input(stdout, Bin, #state{stdout_buf=Buf}=S) ->
    {Lines, NextBuf} = hubc_util:input(Buf, Bin),
    {Lines, S#state{stdout_buf=NextBuf}};
stream_input(stderr, Bin, #state{stderr_buf=Buf}=S) ->
    {Lines, NextBuf} = hubc_util:input(Buf, Bin),
    {Lines, S#state{stderr_buf=NextBuf}}.

handle_stream_lines(Stream, Lines, State) ->
    dispatch_to_stream_handlers({Stream, Lines}, stream_handlers(State)).

dispatch_to_stream_handlers(Msg, Handlers) ->
    lists:foreach(fun(H) -> call_stream_handler(H, Msg) end, Handlers).

call_stream_handler(F, Msg) when is_function(F) -> F(Msg);
call_stream_handler({M, F, A}, Msg) -> M:F([Msg|A]).

handle_exec_exit(Reason) ->
    {stop, Reason}.

handle_stop(Timeout, #state{exec_pid=Pid}=State) ->
    exec:stop_and_wait(Pid, Timeout),
    {stop, normal, ok, State}.

%% ===================================================================
%% State
%% ===================================================================

init_state(Op) ->
    #state{
       op=Op,
       stdout_buf=hubc_util:new_input_buffer(),
       stderr_buf=hubc_util:new_input_buffer()}.

logdir(#state{op=#op{logdir=Dir}}) -> Dir.

tasks(#state{op=#op{tasks=Tasks}}) -> Tasks.

cmd(#state{op=#op{cmd=Cmd}}) -> Cmd.

stream_handlers(#state{op=#op{stream_handlers=Handlers}}) -> Handlers.

%% ===================================================================
%% Misc
%% ===================================================================

notify_exec_started(OSPid) ->
    gproc:send({p, l, {exec_start, self()}}, {exec_started, self(), OSPid}).
