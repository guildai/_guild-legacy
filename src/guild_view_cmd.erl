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

-module(guild_view_cmd).

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
      view_options() ++ guild_cmd_support:project_options(),
      [{pos_args, 0}]).

view_options() ->
    [{port, "-p, --port",
      fmt("HTTP server port (default is ~b)", [?default_port]),
      [{metavar, "PORT"}]},
     {interval, "-n, --interval",
      fmt("refresh interval in seconds (default is ~b)", [?default_interval]),
      [{metavar, "SECONDS"}]},
     {logging, "-l, --logging",
      "enable logging", [flag]}].

fmt(Msg, Data) -> io_lib:format(Msg, Data).

%% ===================================================================
%% Main
%% ===================================================================

main(Opts, []) ->
    View = init_project_view(Opts),
    guild_app:init_support([json, exec]),
    Server = start_http_server(View, Opts),
    wait_for_server_and_terminate(Server).

init_project_view(Opts) ->
    Project = guild_cmd_support:project_from_opts(Opts),
    {ok, View} = guild_project_view_sup:start_view(Project, view_opts(Opts)),
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
    Server = try_start_server(View, Port, ServerOpts),
    guild_cli:out("View server running on port ~b~n", [Port]),
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

try_start_server(View, Port, Opts) ->
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

wait_for_server_and_terminate(Pid) ->
    guild_proc:reg(Pid),
    Exit = guild_proc:wait_for({proc, Pid}),
    handle_server_exit(Exit).

handle_server_exit({_, normal}) ->
    guild_cli:out("Server stopped by user~n");
handle_server_exit({_, Other}) ->
    {error, io_lib:format("Unexpected server exit: ~p", [Other])}.
