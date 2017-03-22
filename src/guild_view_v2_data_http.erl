-module(guild_view_v2_data_http).

-export([create_app/1, app/4]).

create_app(View) ->
    ViewSettings = guild_view_v2:settings(View),
    psycho_util:dispatch_app(
      {?MODULE, app},
      [method, parsed_path, View, ViewSettings]).


%% ===================================================================
%% Dispatch
%% ===================================================================

app("GET", {"/data/runs", _, _}, View, _) ->
    handle_runs(View);
app("GET", {"/data/series/" ++ Path, _, Params}, View, _) ->
    handle_series(View, Path, Params);
app("GET", {"/data/flags", _, Params}, View, _) ->
    handle_flags(View, Params);
app("GET", {"/data/attrs", _, Params}, View, _) ->
    handle_attrs(View, Params);
app("GET", {"/data/output", _, Params}, View, _) ->
    handle_output(View, Params);
app("GET", {"/data/compare", _, Params}, View, _) ->
    handle_compare(View, Params);
app("GET", {"/data/tf/" ++ Path, Qs, _}, _View, Settings) ->
    handle_tf_data(Path, Qs, Settings);
app(_, _, _, _) ->
    guild_http:bad_request().

%% ===================================================================
%% Runs
%% ===================================================================

handle_runs(View) ->
    Runs = guild_view_v2:formatted_runs(View),
    guild_http:ok_json(guild_json:encode(Runs)).

%% ===================================================================
%% Series
%% ===================================================================

handle_series(View, Path, Params) ->
    Run = run_for_params(Params, View),
    Pattern = http_uri:decode(Path),
    Max = max_epoch_for_params(Params),
    Series = guild_data_reader_v2:series(Run, Pattern, Max),
    guild_http:ok_json(guild_json:encode(Series)).

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
    Flags = guild_data_reader_v2:flags(Run),
    guild_http:ok_json(guild_json:encode(Flags)).

%% ===================================================================
%% Attrs
%% ===================================================================

handle_attrs(View, Params) ->
    Run = run_for_params(Params, View),
    Attrs = guild_data_reader_v2:attrs(Run),
    guild_http:ok_json(guild_json:encode(Attrs)).

%% ===================================================================
%% Output
%% ===================================================================

handle_output(View, Params) ->
    Run = run_for_params(Params, View),
    Output = guild_data_reader_v2:output(Run),
    guild_http:ok_json(guild_json:encode(Output)).

%% ===================================================================
%% Compare
%% ===================================================================

handle_compare(View, Params) ->
    Sources = sources_for_params(Params),
    Runs = guild_view_v2:all_runs(View),
    Compare = (catch guild_data_reader_v2:compare(Runs, Sources)),
    handle_compare_result(Compare).

sources_for_params(Params) ->
    Param = proplists:get_value("sources", Params, ""),
    Split = string:tokens(Param, ","),
    lists:usort(Split).

handle_compare_result({'EXIT', Err}) ->
    handle_compare_error(Err);
handle_compare_result(Compare) ->
    guild_http:ok_json(guild_json:encode(Compare)).

handle_compare_error({{run_source, Source}, _}) ->
    guild_http:bad_request(["invalid source: ", Source]);
handle_compare_error(Other) ->
    error_logger:error_report({compare_error, Other}),
    guild_http:internal_error().

%% ===================================================================
%% TensorFlow data
%% ===================================================================

handle_tf_data(Path, Qs, #{tensorboard:=#{port:=Port}})
  when is_integer(Port) ->
    handle_tf_data_(Path, Qs, Port);
handle_tf_data(_Path, _Qs, _Settings) ->
    guild_http:internal_error("TensorBoard not running").

handle_tf_data_(Path, Qs, Port) ->
    FullPath = [Path, "?", Qs],
    handle_tf_data_result(guild_tf_data_proxy:data(Port, FullPath)).

handle_tf_data_result({ok, {Status, Headers, Body}}) ->
    {Status, Headers, Body};
handle_tf_data_result({error, Err}) ->
    guild_log:internal("Error reading from tf proxy: ~p~n", [Err]),
    guild_http:internal_error().

%% ===================================================================
%% Shared
%% ===================================================================

run_for_params(Params, View) ->
    guild_view_v2_http:run_for_params(Params, View).
