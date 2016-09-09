-module(hubc_test_task).

-behavior(e2_task).

-export([start_link/3]).

-export([init/1, handle_task/1, handle_msg/3]).

-record(file, {dir, n}).
-record(buf, {buf, n}).

start_link(OpPid, InitArgs, Repeat) ->
    e2_task:start_link(?MODULE, [OpPid, InitArgs], [{repeat, Repeat}]).

init([Op, InitArgs]) ->
    erlang:monitor(process, Op),
    hubc_proc:reg(optask, self()),
    init_for_args(InitArgs).

init_for_args({write_files, Dir})  -> {ok, #file{dir=Dir, n=0}};
init_for_args({write_to_buf, Buf}) -> {ok, #buf{buf=Buf, n=0}}.

handle_task(#file{}=State) -> handle_write_file(State);
handle_task(#buf{}=State)  -> handle_write_to_buffer(State).

handle_write_file(#file{dir=Dir, n=N}=S) when N < 100 ->
    ok = file:write_file(test_file_path(Dir, N), <<>>),
    {repeat, S#file{n=N+1}};
handle_write_file(_) ->
    {stop, normal}.

test_file_path(Dir, N) ->
    Name = io_lib:format("test-~2..0b", [N]),
    filename:join(Dir, Name).

handle_write_to_buffer(#buf{buf=Buf, n=N}=S) when N < 100 ->
    ok = hubc_test_buffer:add(Buf, N),
    {repeat, S#buf{n=N+1}}.

handle_msg({'DOWN', _Ref, process, _OpPid, _Reason}, _From, _State) ->
    {stop, normal}.
