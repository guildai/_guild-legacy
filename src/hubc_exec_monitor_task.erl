-module(hubc_exec_monitor_task).

-behavior(e2_service).

-export([start_link/3]).

-export([init/1, handle_task/1, handle_msg/3]).

-record(state, {op, op_stop_timeout, exec_pid, exec_ospid}).

start_link(Op, OSPid, OpStopTimeout) ->
    e2_task:start_link(?MODULE, [Op, OSPid, OpStopTimeout]).

init([Op, OSPid, OpStopTimeout]) ->
    process_flag(trap_exit, true),
    hubc_proc:reg(optask, self()),
    {ok, #state{op=Op, op_stop_timeout=OpStopTimeout, exec_ospid=OSPid}}.

handle_task(#state{exec_ospid=OSPid}=State) ->
    hubc_cli:out("TensorHub monitoring process ~b~n", [OSPid]),
    Next = monitor_ospid(State),
    {wait_for_msg, Next}.

monitor_ospid(#state{exec_ospid=OSPid}=S) ->
    {ok, Pid, OSPid} = exec:manage(OSPid, [monitor]),
    S#state{exec_pid=Pid}.

handle_msg({'DOWN', _, process, Pid, _}, noreply, #state{exec_pid=Pid}=State) ->
    handle_exec_exit(State).

handle_exec_exit(#state{op=Op, op_stop_timeout=Timeout, exec_ospid=OSPid}) ->
    hubc_cli:out("TensorHub stopping operation (process ~b exited)~n", [OSPid]),
    hubc_operation:stop(Op, Timeout),
    {stop, normal}.
