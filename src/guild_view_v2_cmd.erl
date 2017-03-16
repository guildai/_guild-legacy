-module(guild_view_v2_cmd).

-export([main/2]).

-define(default_port, 6334).

main(Opts, []) ->
    Project = guild_cmd_support:project_from_opts(Opts),
    guild_app:init_support([exec]),
    TBInfo = start_tensorboard(Project, Opts),
    View = init_project_view(Project, Opts, TBInfo),
    Port = guild_cmd_support:port_opt(Opts, ?default_port),
    Server = start_http_server(View, Port, Opts),
    guild_cli:out("Guild View running on port ~b~n", [Port]),
    wait_for_server_and_terminate(Server, Opts).

start_tensorboard(Project, Opts) ->
    LogDir = tensorboard_logdir(Project),
    Port = guild_util:free_port(),
    TBChild = {guild_tf_tensorboard, start_link, [LogDir, Port]},
    handle_tensorboard_start(guild_app:start_child(TBChild), Port, Opts).

tensorboard_logdir(Project) ->
    string:join(guild_project_util:all_runroots(Project), ",").

handle_tensorboard_start({ok, _}, Port, Opts) ->
    maybe_report_tensorboard_port(proplists:get_bool(debug, Opts), Port),
    tb_info(Port);
handle_tensorboard_start({error, Err}, _Port, _Opts) ->
    report_tensorboard_error(Err),
    tb_info(undefined).

maybe_report_tensorboard_port(true, Port) ->
    guild_cli:out("TensorBoard running on port ~b~n", [Port]);
maybe_report_tensorboard_port(false, _Port) ->
    ok.

tb_info(Port) ->
    #{port => Port}.

report_tensorboard_error(Err) ->
    guild_cli:out(
      io_lib:format(
        "Unable to start TensorBoard (~p)\n"
        "TensorBoard integration will be disabled\n",
        [Err])).

init_project_view(Project, Opts, TBInfo) ->
    Settings = view_settings(Opts, TBInfo),
    {ok, View} = guild_view_v2:start_link(Project, Settings),
    View.

view_settings(Opts, TBInfo) ->
    #{refreshInterval => guild_view_cmd:interval_opt(Opts),
      tensorboard => TBInfo}.

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
