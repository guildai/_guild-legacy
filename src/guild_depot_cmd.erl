%% Copyright 2106 TensorHub, Inc.
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

-module(guild_depot_cmd).

-export([parser/0, main/2]).

-define(default_port, 6555).

%% ===================================================================
%% Parser
%% ===================================================================

parser() ->
    cli:parser(
      "guild depot",
      "[OPTION]...",
      "Start a server to store published projects.\n"
      "\n"
      "NOTE: This is an experimental feature.\n"
      "\n"
      "When the server is running, open your browser on the specified port "
      "(default is " ++ integer_to_list(?default_port)
      ++ ") - e.g. http://localhost:" ++ integer_to_list(?default_port) ++ ".\n"
      "\n"
      "To log server requests, use --logging.",
      depot_options(),
      [{pos_args, 0}]).

depot_options() ->
    [{port, "-p, --port",
      fmt("HTTP server port (default is ~b)", [?default_port]),
      [{metavar, "PORT"}]},
     {logging, "-l, --logging",
      "enable logging", [flag]},
     {depot_dir, "-D, --depot",
      "depot directory (default is current directory)",
      [{metavar, "DIR"}]}].

fmt(Msg, Data) -> io_lib:format(Msg, Data).

%% ===================================================================
%% Main
%% ===================================================================

main(Opts, []) ->
    Depot = depot_from_opts(Opts),
    View = init_depot_view(Depot),
    Port = guild_cmd_support:port_opt(Opts, ?default_port),
    ServerOpts = server_opts(Opts),
    guild_app:init_support([json, exec]),
    guild_depot_db:open(Depot, [create_if_missing]),
    Server = start_http_server(View, Port, ServerOpts),
    guild_cli:out("Guild Depot running on port ~b~n", [Port]),
    wait_for_server_and_terminate(Server, Opts).

depot_from_opts(Opts) ->
    %% Using depot dir to represent depot for now.
    proplists:get_value(depot_dir, Opts, ".").

init_depot_view(Depot) ->
    {ok, View} = guild_depot_view_sup:start_view(Depot),
    View.

server_opts(Opts) ->
    [recompile_templates, {log, server_log_opt(Opts)}].

server_log_opt(Opts) ->
    proplists:get_value(logging, Opts).

start_http_server(View, Port, Opts) ->
    case guild_depot_http:start_server(View, Port, Opts) of
        {ok, Server} ->
            Server;
        {error, {{listen, eaddrinuse}, _Stack}} ->
            port_in_use_error(Port)
    end.

port_in_use_error(Port) ->
    guild_cli:cli_error(
      io_lib:format(
        "port ~b is being used by another application\n"
        "Try 'guild depot --port PORT' with a different port.",
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
