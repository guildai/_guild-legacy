-module(hubc_test_buffer).

-export([start_link/0, add/2, get_all/1, stop/1]).

-export([handle_msg/3]).

start_link() -> e2_service:start_link(?MODULE, []).
add(Buf, X)  -> e2_service:call(Buf, {add, X}).
get_all(Buf) -> e2_service:call(Buf, get_all).
stop(Buf)    -> e2_service:call(Buf, stop).

handle_msg({add, X}, _From, Buf) -> {reply, ok, [X|Buf]};
handle_msg(get_all,  _From, Buf) -> {reply, lists:reverse(Buf), Buf};
handle_msg(stop,     _From, Buf) -> {stop, normal, ok, Buf}.
