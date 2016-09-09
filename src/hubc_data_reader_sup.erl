-module(hubc_data_reader_sup).

-behavior(e2_task_supervisor).

-export([start_link/0, start_reader/2]).

start_link() ->
    e2_task_supervisor:start_link(?MODULE, hubc_data_reader, [registered]).

start_reader(Request, Reply) ->
    e2_task_supervisor:start_task(?MODULE, [Request, Reply, self()]).
