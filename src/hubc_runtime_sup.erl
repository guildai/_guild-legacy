-module(hubc_runtime_sup).

-behavior(e2_supervisor).

-export([start_link/0, start_child/1]).

start_link() ->
    e2_supervisor:start_link(?MODULE, [], [registered]).

start_child(ChildSpec) ->
    e2_supervisor:start_child(?MODULE, ChildSpec).
