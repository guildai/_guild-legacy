-module(hubc_view_cmd).

-export([parser/0, main/2]).

-define(default_port, 6333).
-define(default_interval, 5).

%% ===================================================================
%% Parser
%% ===================================================================

parser() ->
    cli:parser(
      "guild view",
      "[OPTION]...",
      "Start project viewer.",
      view_options() ++ hubc_cmd_support:project_options(),
      [{pos_args, 0}]).

view_options() ->
    [{port, "-p, --port",
      io_lib:format(
        "HTTP server port (default is ~b)",
        [?default_port]),
      [{metavar, "PORT"}]},
     {interval, "-n, --interval",
      io_lib:format(
        "refresh interval in seconds (default is ~b)",
        [?default_interval]),
      [{metavar, "SECONDS"}]},
     {logging, "-l, --logging", "enable logging", [flag]}].

%% ===================================================================
%% Main
%% ===================================================================

main(Opts, []) ->
    View = init_project_view(Opts),
    hubc_app:init_support([json]),
    Server = start_http_server(View, Opts),
    wait_for_server_and_terminate(Server).

init_project_view(Opts) ->
    Project = hubc_cmd_support:project_from_opts(Opts),
    {ok, View} = hubc_project_view_sup:start_view(Project, view_opts(Opts)),
    View.

view_opts(Opts) ->
    [{data_poll_interval, interval_opt(Opts)}].

interval_opt(Opts) ->
    validate_interval(
      cli_opt:int_val(
        interval, Opts, ?default_interval, "invalid value for --interval")).

validate_interval(I) when I > 0 -> I;
validate_interval(_) -> throw({error, "invalid value for --interval"}).

start_http_server(View, Opts) ->
    Port = port_opt(Opts),
    ServerOpts = server_opts(Opts),
    {ok, Server} = hubc_project_view_http:start_server(View, Port, ServerOpts),
    hubc_cli:out("View server running on port ~b~n", [Port]),
    Server.

port_opt(Opts) ->
    validate_port(
      cli_opt:int_val(
        port, Opts, ?default_port, "invalid value for --port")).

validate_port(P) when P > 0, P < 65535 -> P;
validate_port(_) -> throw({error, "invalid value for --port"}).

server_opts(Opts) ->
    [recompile_templates, {log, server_log_opt(Opts)}].

server_log_opt(Opts) ->
    proplists:get_value(logging, Opts).

wait_for_server_and_terminate(Pid) ->
    hubc_proc:reg(Pid),
    Exit = hubc_proc:wait_for(proc, Pid),
    handle_server_exit(Exit).

handle_server_exit({_, normal}) ->
    hubc_cli:out("Server stopped by user~n");
handle_server_exit({_, Other}) ->
    {error, io_lib:format("Unexpected server exit: ~p", [Other])}.
