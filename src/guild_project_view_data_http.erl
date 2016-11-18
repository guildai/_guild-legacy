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

-module(guild_project_view_data_http).

-export([app/3]).

%% ===================================================================
%% App
%% ===================================================================

app("GET", {"/data/runs", _, _}, View) ->
    view_json(View, runs);
app("GET", {"/data/flags", _, Params}, View) ->
    view_json(View, {flags, run_opt(Params)});
app("GET", {"/data/attrs", _, Params}, View) ->
    view_json(View, {attrs, run_opt(Params)});
app("GET", {"/data/summary", _, Params}, View) ->
    view_json(View, {summary, run_opt(Params)});
app("GET", {"/data/series/" ++ Path, _, Params}, View) ->
    view_json(View, {series, decode(Path), series_opts(Params)});
app("GET", {"/data/output", _, Params}, View) ->
    view_json(View, {output, run_opt(Params)});
app("GET", {"/data/compare", _, Params}, View) ->
    view_json(View, {compare, sources_opt(Params)});
app("GET", {"/data/image", _, Params}, View) ->
    handle_image_request(Params, View);
app("GET", _, _) ->
    guild_http:not_found();
app(_, _, _) ->
    guild_http:bad_request().

run_opt(Params) -> guild_project_view_http:run_opt(Params).

view_json(V, Req) ->
    guild_http:ok_json(guild_project_view:json(V, Req)).

series_opts(Params) ->
    [{run, run_opt(Params)}] ++ max_epoch_opts(Params).

max_epoch_opts(Params) ->
    Schema = [{"max_epochs", [{any, [integer, "all"]}]}],
    Error = fun max_epoch_validate_error/1,
    case guild_http:validate_params(Params, Schema, Error) of
        [{_, "all"}] -> [{max_epochs, all}];
        [{_, Max}]   -> [{max_epochs, Max}];
        []           -> []
    end.

max_epoch_validate_error(_) ->
    throw(
      guild_http:bad_request(
        "max_epochs must be a valid integer or 'all'")).

sources_opt(Params) ->
    split_sources(proplists:get_value("sources", Params, "")).

split_sources(Sources) ->
    lists:usort(string:tokens(Sources, ",")).

decode(Part) -> http_uri:decode(Part).

%% ===================================================================
%% Images
%% ===================================================================

handle_image_request(Params, View) ->
    RunId = run_opt(Params),
    {_, Run} = guild_project_view_http:resolve_project_run(RunId, View),
    RunDir = guild_run:dir(Run),
    Index = index_opt(Params),
    handle_image(guild_tensorflow_port2:get_image(RunDir, Index)).

index_opt(Params) ->
    Schema = [{"index", [required, integer]}],
    Validated = guild_http:validate_params(Params, Schema),
    proplists:get_value("index", Validated).

handle_image({ok, #{type:=Type, bytes:=Bytes}}) ->
    MIME = ["image/", Type],
    Len = size(Bytes),
    {{200, "OK"}, [{"Content-Type", MIME}, {"Content-Length", Len}], Bytes};
handle_image({error, <<"not found">>}) ->
    guild_http:not_found().
