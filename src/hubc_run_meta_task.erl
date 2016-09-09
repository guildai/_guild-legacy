-module(hubc_run_meta_task).

-behavior(e2_task).

-export([start_link/3]).

-export([init/1, handle_task/1, handle_msg/3]).

-record(state, {op, dir, attrs}).

start_link(Op, LogDir, RunAttrs) ->
    e2_task:start_link(?MODULE, [Op, LogDir, RunAttrs], []).

init([Op, LogDir, RunAttrs]) ->
    monitor(process, Op),
    hubc_proc:reg(optask, self()),
    {ok, init_state(Op, LogDir, RunAttrs)}.

init_state(Op, LogDir, RunAttrs) ->
    #state{
       op=Op,
       dir=hubc_run:run_meta_dir(LogDir),
       attrs=RunAttrs}.

handle_task(#state{dir=Dir, attrs=Attrs}=State) ->
    ensure_dir(Dir),
    write_lock(Dir),
    write_timestamp(Dir, "start"),
    write_attrs(Dir, Attrs),
    {wait_for_msg, State}.

ensure_dir(Dir) ->
    ok = filelib:ensure_dir(Dir ++ "/").

write_lock(Dir) ->
    ok = file:write_file(lock_file(Dir), os:getpid()).

lock_file(Dir) -> filename:join(Dir, "LOCK").

write_timestamp(Dir, Name) ->
    write_attrs(Dir, [{Name, hubc_run:timestamp()}]).

write_attrs(Dir, Attrs) ->
    lists:foreach(fun({Name, Val}) -> write_attr(Dir, Name, Val) end, Attrs).

write_attr(Dir, Name, Val) ->
    Path = filename:join(Dir, Name),
    ok = file:write_file(Path, val_to_string(Val)).

val_to_string(undefined)                       -> "";
val_to_string(A) when is_atom(A)               -> atom_to_list(A);
val_to_string(S) when is_list(S); is_binary(S) -> S;
val_to_string(I) when is_integer(I)            -> integer_to_list(I);
val_to_string(F) when is_float(F)              -> float_to_list(F).

handle_msg({'DOWN', _, process, Op, Reason}, noreply, #state{op=Op, dir=Dir}) ->
    write_exit_status(Dir, Reason),
    write_timestamp(Dir, "stop"),
    rm_lock(Dir),
    {stop, normal}.

write_exit_status(Dir, OpExitReason) ->
    write_attrs(Dir, [{"exit_status", op_exit_status(OpExitReason)}]).

op_exit_status(normal)           -> 0;
op_exit_status({exit_status, N}) -> N.

rm_lock(Dir) ->
    ok = file:delete(lock_file(Dir)).
