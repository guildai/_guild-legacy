-module(hubc_project_view_data_http).

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
app(_, _V) -> hubc_http:not_found().

view_json(V, Req) ->
    hubc_http:ok_json(hubc_project_view:json(V, Req)).

run_opt(Params) ->
    run_for_validated(psycho_util:validate(Params, [{"run", [integer]}])).

run_for_validated({ok, Validated}) ->
    proplists:get_value("run", Validated, latest);
run_for_validated({error, Err}) ->
    throw(hubc_http:bad_request(psycho_util:format_validate_error(Err))).

series_opts(Params) ->
    [{run, run_opt(Params)}].

sources_opt(Params) ->
    split_sources(proplists:get_value("sources", Params, "")).

split_sources(Sources) ->
    lists:usort(string:tokens(Sources, ",")).

decode(Part) -> http_uri:decode(Part).
