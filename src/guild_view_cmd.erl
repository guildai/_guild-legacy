%% Copyright 2016-2017 TensorHub, Inc.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(guild_view_cmd).

-export([parser/0, main/2]).

-define(default_port, 6333).
-define(default_refresh_interval, 5).

%% ===================================================================
%% Parser
%% ===================================================================

parser() ->
    cli:parser(
      "guild view",
      "[OPTION]...",
      "Start a web based app to view and interact with project runs.\n"
      "\n"
      "When the server is running, open your browser on the specified port "
      "(default is " ++ integer_to_list(?default_port)
      ++ ") - e.g. http://localhost:" ++ integer_to_list(?default_port) ++ ".\n"
      "\n"
      "To log server requests, use --logging.\n"
      "\n"
      "To modify the refresh interval (default is "
      ++ integer_to_list(?default_refresh_interval)
      ++ " seconds), use --interval. This is useful for "
      "longer running operations that don't need to be refreshed often.",
      view_options() ++ guild_cmd_support:project_options(),
      [{pos_args, 0}]).

view_options() ->
    [{port, "-p, --port",
      fmt("HTTP server port (default is ~b)", [?default_port]),
      [{metavar, "PORT"}]},
     {interval, "-n, --interval",
      fmt("refresh interval in seconds (default is ~b)",
          [?default_refresh_interval]),
      [{metavar, "SECONDS"}]},
     {logging, "-l, --logging",
      "enable logging", [flag]},
     {v2, "--v2", "use Polymer based view (experimental)", [flag]}].

fmt(Msg, Data) -> io_lib:format(Msg, Data).

%% ===================================================================
%% Main
%% ===================================================================

main(Opts, []) ->
    case proplists:get_bool(v2, Opts) of
        true -> guild_view_v2_cmd:main(Opts, []);
        false -> default_view(Opts)
    end.

default_view(Opts) ->
    View = init_project_view(Opts),
    Port = guild_cmd_support:port_opt(Opts, ?default_port),
    ServerOpts = server_opts(Opts),
    guild_app:init_support([json, exec, {app_child, guild_tensorflow_port}]),
    Server = start_http_server(View, Port, ServerOpts),
    guild_cli:out("Guild View running on port ~b~n", [Port]),
    wait_for_server_and_terminate(Server, Opts).

init_project_view(Opts) ->
    Project = guild_cmd_support:project_from_opts(Opts),
    {ok, View} = guild_project_view_sup:start_view(Project, view_opts(Opts)),
    View.

view_opts(Opts) ->
    [{data_poll_interval, interval_opt(Opts)}].

interval_opt(Opts) ->
    validate_interval(
      cli_opt:int_val(
        interval, Opts, ?default_refresh_interval,
        "invalid value for --interval")).

validate_interval(I) when I > 0 -> I;
validate_interval(_) -> throw({error, "invalid value for --interval"}).

server_opts(Opts) ->
    [recompile_templates, {log, server_log_opt(Opts)}].

server_log_opt(Opts) ->
    proplists:get_value(logging, Opts).

start_http_server(View, Port, Opts) ->
    case guild_project_view_http:start_server(View, Port, Opts) of
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
