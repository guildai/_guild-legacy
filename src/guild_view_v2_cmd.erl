-module(guild_view_v2_cmd).

-export([main/2]).

-define(default_port, 6334).

main(Opts, []) ->
    View = init_project_view(Opts),
    Port = guild_cmd_support:port_opt(Opts, ?default_port),
    guild_app:init_support([exec]),
    Server = start_http_server(View, Port, Opts),
    guild_cli:out("Guild View running on port ~b~n", [Port]),
    wait_for_server_and_terminate(Server, Opts).

init_project_view(Opts) ->
    Project = guild_cmd_support:project_from_opts(Opts),
    {ok, View} = guild_view_v2:start_link(Project),
    View.

start_http_server(View, Port, Opts) ->
    case guild_view_v2_http:start_server(View, Port, Opts) of
        {ok, Server} ->
            Server;
        {error, {{listen, eaddrinuse}, _Stack}} ->
            port_in_use_error(Port)
    end.

port_in_use_error(Port) ->
    guild_cli:cli_error(
      io_lib:format(
        "port ~b is being used by another application\n"
        "Try 'guild view --port PORT' with a different port.",
        [Port])).

wait_for_server_and_terminate(Pid, MainOpts) ->
    guild_proc:reg(Pid),
    Exit = guild_proc:wait_for({proc, Pid}),
    handle_server_exit(Exit, MainOpts).

handle_server_exit({_, normal}, _MainOpts) ->
    guild_cli:out("Server stopped by user~n");
handle_server_exit({_, Other}, MainOpts) ->
    guild_log:internal("Restarting server due to error: ~p~n", [Other]),
    main(MainOpts, []).
