-module(hubc_optask_sup).

-export([start_link/0, start_task/2, tasks/0]).

-behavior(e2_task_supervisor).

start_link() ->
    e2_task_supervisor:start_link(?MODULE, {erlang, apply, []}, [registered]).

start_task({M, F, A}, OpPid) ->
    e2_task_supervisor:start_task(?MODULE, [M, F, [OpPid|A]]).

tasks() ->
    supervisor:which_children(?MODULE).
