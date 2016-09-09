-module(hubc_http_sup).

-export([start_link/0]).

-export([start_server/3, stop_server/1]).

start_link() ->
    e2_task_supervisor:start_link(?MODULE, hubc_http, [registered]).

start_server(Port, App, Opts) ->
    e2_task_supervisor:start_task(?MODULE, [Port, App, Opts]).

stop_server(Proc) ->
    exit(whereis(Proc), kill).
