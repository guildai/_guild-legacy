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

-module(guild_model_serve_http).

-export([start_server/3, app/5]).

start_server(Project, Run, Port) ->
    App = psycho_util:dispatch_app(?MODULE, [method, path, Project, Run, env]),
    guild_http_sup:start_server(Port, App, []).

app("POST", "/run", Project, Run, Env) ->
    Handler = {guild_project_view_model_http, handle_model_run, [Project, Run]},
    {recv_body, Handler, Env};
app(_, _, _, _, _) ->
    guild_http:bad_request().
