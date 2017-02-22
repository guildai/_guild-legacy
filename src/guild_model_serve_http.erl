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

-module(guild_model_serve_http).

-export([start_server/3, app/5]).

-define(max_run_request, 1024 * 1024 * 1024).

start_server(Project, Run, Port) ->
    ModelInitResult = init_tensorflow_port_model(Project, Run),
    maybe_start_server(ModelInitResult, Project, Run, Port).

init_tensorflow_port_model(Project, Run) ->
    guild_tensorflow_port:load_project_model(Project, Run).

maybe_start_server(ok, Project, Run, Port) ->
    App = psycho_util:dispatch_app(?MODULE, [method, path, Project, Run, env]),
    guild_http_sup:start_server(Port, App, []);
maybe_start_server({error, Err}, _Project, _Run, _Port) ->
    {error, {port_init, Err}}.

app("POST", "/run", Project, Run, Env) ->
    Handler = {guild_project_view_model_http, handle_model_run, [Project, Run]},
    {recv_body, Handler, Env, [{recv_length, ?max_run_request}]};
app("GET", "/info", Project, Run, _Env) ->
    guild_project_view_model_http:handle_model_info(Project, Run);
app("GET", "/stats", Project, Run, _Env) ->
    guild_project_view_model_http:handle_model_stats(Project, Run);
app(_, _, _, _, _) ->
    guild_http:bad_request().
