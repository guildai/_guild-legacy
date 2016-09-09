-module(hubc_operation_sup).

-export([start_link/0, start_op/2]).

-behavior(e2_task_supervisor).

start_link() ->
    e2_task_supervisor:start_link(?MODULE, hubc_operation, [registered]).

start_op(Name, Op) ->
    e2_task_supervisor:start_task(?MODULE, [Name, Op]).
