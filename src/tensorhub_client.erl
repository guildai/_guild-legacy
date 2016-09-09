-module(tensorhub_client).

-export([main/1, start/0, stop/0, restart/0, version/0]).

main(Args) ->
    start(),
    handle_args(Args).

start() ->
    {ok, _} = application:ensure_all_started(?MODULE).

stop() ->
    application:stop(?MODULE).

restart() ->
    stop(),
    start().

handle_args(Args) ->
    Parser = hubc_cli:parser(),
    Exit = cli:main(Args, Parser, {hubc_cli, main, []}),
    stop(),
    erlang:halt(Exit).

version() ->
    {ok, Vsn} = application:get_key(?MODULE, vsn),
    Vsn.
