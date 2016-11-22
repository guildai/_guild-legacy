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

-export([handle_model_init/2, handle_model_run/4, handle_model_info/2,
         handle_model_stats/2]).

-define(max_run_request, 1024 * 1024 * 1024).

%% ===================================================================
%% App
%% ===================================================================

app("POST", {"/model/run", _, Params}, Env, View) ->
    {Project, Run} = resolve_project_run(Params, View),
    Handler = {?MODULE, handle_model_run, [Project, Run]},
    {recv_body, Handler, Env, [{recv_length, ?max_run_request}]};
app("POST", {"/model/init", _, Params}, _Env, View) ->
    {Project, Run} = resolve_project_run(Params, View),
    handle_model_init(Project, Run);
app("GET", {"/model/info", _, Params}, _Env, View) ->
    {Project, Run} = resolve_project_run(Params, View),
    handle_model_info(Project, Run);
app("GET", {"/model/stats", _, Params}, _Env, View) ->
    {Project, Run} = resolve_project_run(Params, View),
    handle_model_stats(Project, Run);
app(_, _, _, _) ->
    guild_http:bad_request().

resolve_project_run(Params, View) ->
    RunId = guild_project_view_http:run_opt(Params),
    guild_project_view_http:resolve_project_run(RunId, View).

%% ===================================================================
%% Handlers
%% ===================================================================

handle_model_init(Project, Run) ->
    http_result(guild_tensorflow_port:load_project_model(Project, Run)).

handle_model_run(Project, Run, Body, Env) ->
    Opts = run_project_model_opts(Env),
    http_result(
      guild_tensorflow_port:run_project_model(
        Project, Run, Body, Opts)).

run_project_model_opts(Env) ->
    {_, _, Params} = psycho:parsed_request_path(Env),
    case proplists:is_defined("withstats", Params) of
        true -> [with_stats];
        false -> []
    end.

handle_model_info(Project, Run) ->
    http_result(guild_tensorflow_port:project_model_info(Project, Run)).

handle_model_stats(Project, Run) ->
    http_result(guild_tensorflow_port:project_model_stats(Project, Run)).

http_result({ok, JSON}) ->
    guild_http:ok_json(JSON);
http_result({error, <<"not found">>}) ->
    guild_http:not_found();
http_result({error, Err}) ->
    guild_http:bad_request([Err, "\n"]).
