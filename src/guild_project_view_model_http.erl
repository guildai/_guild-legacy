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

-module(guild_project_view_model_http).

-export([app/4]).

-export([handle_model_run/4]).

-define(util, guild_project_view_http_util).

%% ===================================================================
%% App
%% ===================================================================

app("POST", {"/model/run", _, Params}, Env, View) ->
    {recv_body, {?MODULE, handle_model_run, [Params, View]}, Env};
app(_, _, _, _) ->
    guild_http:bad_request().

%% ===================================================================
%% Model run
%% ===================================================================

handle_model_run(Params, View, Body, _Env) ->
    {Project, Run} = ?util:resolve_project_run(?util:run_opt(Params), View),
    ModelPath = model_path(Run, Project),
    ?util:ensure_tensorflow_port(),
    handle_image(guild_tensorflow_port:run_model(ModelPath, Body)).

model_path(Run, _Project) ->
    filename:join(guild_run:dir(Run), "model/export").

handle_image({ok, Resp}) ->
    guild_http:ok_text(io_lib:format("~p", [Resp]));
%% handle_image({ok, JSON}) ->
%%     guild_http:ok_json(JSON);
handle_image({error, <<"not found">>}) ->
    guild_http:not_found().
