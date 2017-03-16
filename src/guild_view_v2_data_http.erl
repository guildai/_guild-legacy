-module(guild_view_v2_data_http).

-export([app/3]).

%% ===================================================================
%% Dispatch
%% ===================================================================

app("GET", {"/data/runs", _, _}, View) ->
    handle_runs(View);
app("GET", {"/data/series/" ++ Path, _, Params}, View) ->
    handle_series(View, Path, Params);
app("GET", {"/data/flags", _, Params}, View) ->
    handle_flags(View, Params);
app("GET", {"/data/attrs", _, Params}, View) ->
    handle_attrs(View, Params);
app("GET", {"/data/output", _, Params}, View) ->
    handle_output(View, Params);
app("GET", {"/data/tf/" ++ Path, Qs, _}, _View) ->
    handle_tf_data(Path, Qs);
app(_, _, _) ->
    guild_http:bad_request().

%% ===================================================================
%% Runs
%% ===================================================================

handle_runs(View) ->
    Runs = guild_view_v2:formatted_runs(View),
    JSON = guild_json:encode(Runs),
    guild_http:ok_json(JSON).

%% ===================================================================
%% Series
%% ===================================================================

handle_series(View, Path, Params) ->
    Run = run_for_params(Params, View),
    Pattern = http_uri:decode(Path),
    Max = max_epoch_for_params(Params),
    JSON = guild_data_reader:series_json(Run, Pattern, Max),
    guild_http:ok_json(JSON).

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

%% ===================================================================
%% Flags
%% ===================================================================

handle_flags(View, Params) ->
    Run = run_for_params(Params, View),
    JSON = guild_data_reader:flags_json(Run),
    guild_http:ok_json(JSON).

%% ===================================================================
%% Attrs
%% ===================================================================

handle_attrs(View, Params) ->
    Run = run_for_params(Params, View),
    JSON = guild_data_reader:attrs_json(Run),
    guild_http:ok_json(JSON).

%% ===================================================================
%% Attrs
%% ===================================================================

handle_output(View, Params) ->
    Run = run_for_params(Params, View),
    JSON = guild_data_reader:output_json(Run),
    guild_http:ok_json(JSON).

%% ===================================================================
%% TensorFlow data
%% ===================================================================

handle_tf_data(Path, Qs) ->
    FullPath = [Path, "?", Qs],
    case guild_tf_data_proxy:data(6006, FullPath) of
        {ok, JSON} ->
            guild_http:ok_json(JSON);
        {error, {status, Status}} ->
            {Status, [], []}
    end.

%% ===================================================================
%% Shared
%% ===================================================================

run_for_params(Params, View) ->
    guild_view_v2_http:run_for_params(Params, View).
