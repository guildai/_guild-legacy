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

%% ===================================================================
%% App
%% ===================================================================

app("POST", {"/model/run", _, Params}, Env, View) ->
    {Project, Run} = resolve_project_run(Params, View),
    {recv_body, {?MODULE, handle_model_run, [Project, Run]}, Env};
app(_, _, _, _) ->
    guild_http:bad_request().

resolve_project_run(Params, View) ->
    RunId = guild_project_view_http_util:run_opt(Params),
    guild_project_view_http_util:resolve_project_run(RunId, View).

%% ===================================================================
%% Model run
%% ===================================================================

handle_model_run(Project, Run, Body, _Env) ->
    ModelPath = model_path(Run, Project),
    guild_project_view_http_util:ensure_tensorflow_port(),
    Result = guild_tensorflow_port:run_model(ModelPath, Body),
    handle_model_run_result(Result).

model_path(Run, _Project) ->
    filename:join(guild_run:dir(Run), "model/export").

handle_model_run_result({ok, JSON}) ->
    guild_http:ok_json(JSON);
handle_model_run_result({error, <<"not found">>}) ->
    guild_http:not_found();
handle_model_run_result({error, Err}) ->
    guild_http:bad_request([Err, "\n"]).
