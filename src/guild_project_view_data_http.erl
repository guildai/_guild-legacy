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

view_json(V, Req) ->
    guild_http:ok_json(guild_project_view:json(V, Req)).

run_opt(Params) ->
    Schema = [{"run", [integer]}],
    case psycho_util:validate(Params, Schema) of
        {ok, [{_, Run}]} -> Run;
        {ok, []}         -> latest;
        {error, Err}     -> validate_error(Err)
    end.

validate_error(Err) ->
    throw(guild_http:bad_request(psycho_util:format_validate_error(Err))).

series_opts(Params) ->
    [{run, run_opt(Params)}] ++ max_epoch_opts(Params).

max_epoch_opts(Params) ->
    Schema = [{"max_epochs", [{any, [integer, "all"]}]}],
    case psycho_util:validate(Params, Schema) of
        {ok, [{_, "all"}]} -> [{max_epochs, all}];
        {ok, [{_, Max}]}   -> [{max_epochs, Max}];
        {ok, []}           -> [];
        {error, _}         -> max_epoch_validate_error()
    end.

max_epoch_validate_error() ->
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
    Run = resolve_run(run_opt(Params), View),
    RunDir = guild_run:dir(Run),
    Index = index_opt(Params),
    ensure_tensorflow_working(),
    handle_image(guild_tensorflow_port:load_image(RunDir, Index)).

resolve_run(RunId, View) ->
    case guild_project_view:project_run(View, RunId) of
        undefined -> no_such_run_error(RunId);
        Run -> Run
    end.

no_such_run_error(RunId) ->
    throw(guild_http:bad_request(io_lib:format("no such run ~s", [RunId]))).

index_opt(Params) ->
    Schema = [{"index", [required, integer]}],
    case psycho_util:validate(Params, Schema) of
        {ok, Validated} -> proplists:get_value("index", Validated);
        {error, Err} -> validate_error(Err)
    end.

ensure_tensorflow_working() ->
    %% TEMP bootstrapping of tensorflow port support - when this
    %% stabilizes we need to move long running runtime-specific
    %% services into the supervisory tree, lazily initialized.
    guild_app:init_support(exec),
    case guild_app:start_child(guild_tensorflow_port) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.

handle_image({ok, #{type:=Type, bytes:=Bytes}}) ->
    MIME = ["image/", Type],
    Len = size(Bytes),
    {{200, "OK"}, [{"Content-Type", MIME}, {"Content-Length", Len}], Bytes};
handle_image({error, <<"not found">>}) ->
    guild_http:not_found().
