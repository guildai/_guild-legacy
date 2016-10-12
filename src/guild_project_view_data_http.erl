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

-export([app/2]).

app({"/data/runs", _, _}, View) ->
    view_json(View, runs);
app({"/data/flags", _, Params}, View) ->
    view_json(View, {flags, run_opt(Params)});
app({"/data/summary", _, Params}, View) ->
    view_json(View, {summary, run_opt(Params)});
app({"/data/series/" ++ Path, _, Params}, View) ->
    view_json(View, {series, decode(Path), series_opts(Params)});
app({"/data/output", _, Params}, View) ->
    view_json(View, {output, run_opt(Params)});
app({"/data/compare", _, Params}, View) ->
    view_json(View, {compare, sources_opt(Params)});
app(_, _V) -> guild_http:not_found().

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
