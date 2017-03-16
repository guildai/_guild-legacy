-module(guild_tf_tensorboard).

-behavior(e2_task).

-export([start_link/2, stop/1]).

-export([handle_task/1, handle_msg/3]).

-record(state, {logdir, port, ospid}).

start_link(LogDir, Port) ->
    State = #state{logdir=LogDir, port=Port},
    e2_task:start_link(?MODULE, State, []).

stop(TB) ->
    e2_task:call(TB, stop).

handle_task(S) ->
    Next = start_tensorboard(S),
    {wait_for_msg, Next}.

handle_msg(stop, _From, #state{ospid=OSPid}=S) ->
    exec:stop(OSPid),
    {stop, normal, ok, S}.

start_tensorboard(#state{logdir=LogDir, port=Port}=S) ->
    Args =
        [tensorboard_bin(),
         "--logdir", LogDir,
         "--port",
         integer_to_list(Port)],
    Opts = [],
    {ok, _Pid, OSPid} = exec:run_link(Args, Opts),
    S#state{ospid=OSPid}.

tensorboard_bin() ->
    case os:find_executable("tensorboard") of
        false -> error(tensorboard_not_found);
        Path -> Path
    end.
