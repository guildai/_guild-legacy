-module(guild_view_v2_data_http).

-export([app/3]).

app("GET", {"/data/runs", _, _}, View) ->
    handle_runs(View);
app("GET", {"/data/series/" ++ Path, _, Params}, View) ->
    handle_series(View, Path, Params);
app(_, _, _) ->
    guild_http:bad_request().

handle_runs(View) ->
    Runs = guild_view_v2:formatted_runs(View),
    JSON = guild_json:encode(Runs),
    guild_http:ok_json(JSON).

handle_series(View, Path, Params) ->
    Run = run_for_params(Params, View),
    Pattern = http_uri:decode(Path),
    Max = max_epoch_for_params(Params),
    JSON = guild_data_reader:series_json(Pattern, Run, Max),
    guild_http:ok_json(JSON).

run_for_params(Params, View) ->
    Id = guild_view_v2_http:run_id_for_params(Params),
    case guild_view_v2:resolve_run(View, Id) of
        undefined -> throw(guild_http:not_found());
        Run -> Run
    end.

max_epoch_for_params(Params) ->
    Schema = [{"max_epochs", [{any, [integer, "all"]}]}],
    Error = fun max_epoch_validate_error/1,
    case guild_http:validate_params(Params, Schema, Error) of
        []           -> all;
        [{_, "all"}] -> all;
        [{_, Max}]   -> Max
    end.

max_epoch_validate_error(_) ->
    throw(
      guild_http:bad_request(
        "max_epochs must be a valid integer or 'all'")).
