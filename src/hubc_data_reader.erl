-module(hubc_data_reader).

-behavior(e2_task).

-export([start_link/3]).

-export([handle_task/1]).

-define(null_json, <<"null">>).

%% ===================================================================
%% Start / handle task
%% ===================================================================

start_link(Request, Reply, Owner) ->
    e2_task:start_link(?MODULE, [Request, Reply, Owner]).

handle_task([Request, Reply, Owner]) ->
    link(Owner),
    Reply(handle_request(Request)),
    {stop, normal}.

handle_request({runs_json, LogsRoot}) ->
    runs_json(LogsRoot);
handle_request({flags_json, Run}) ->
    flags_json(Run);
handle_request({summary_json, Run}) ->
    summary_json(Run);
handle_request({series_json, Pattern, Run, Max}) ->
    series_json(Pattern, Run, Max);
handle_request({output_json, Run}) ->
    output_json(Run);
handle_request({compare_json, Sources, Runs, View}) ->
    compare_json(Sources, Runs, View).

%% ===================================================================
%% Runs JSON
%% ===================================================================

runs_json(LogsRoot) ->
    Runs = hubc_run:runs_for_logroots(LogsRoot),
    hubc_json:encode(format_runs(Runs)).

format_runs(Runs) ->
    sort_formatted_runs([format_run(Run) || Run <- Runs]).

format_run(Run) ->
    {[
      {id, hubc_run:id(Run)},
      {dir, list_to_binary(hubc_run:dir(Run))},
      {status, hubc_run:status(Run)}
      |format_run_attrs(hubc_run:attrs(Run))
     ]}.

format_run_attrs(Attrs) ->
    [format_run_attr(Attr) || Attr <- Attrs].

format_run_attr({Name, Val}) ->
    {list_to_binary(Name), format_attr_val(Name, Val)}.

format_attr_val("start",       Bin) -> binary_to_integer(Bin);
format_attr_val("stop",        Bin) -> binary_to_integer(Bin);
format_attr_val("exit_status", Bin) -> binary_to_integer(Bin);
format_attr_val(_Name,         Bin) -> Bin.

sort_formatted_runs(Runs) ->
    Cmp = fun(A, B) -> run_start_time(A) > run_start_time(B) end,
    lists:sort(Cmp, Runs).

run_start_time({Attrs}) ->
    case lists:keyfind(<<"start">>, 1, Attrs) of
        {_, Val} -> Val;
        false -> 0
    end.

%% ===================================================================
%% Flags JSON
%% ===================================================================

flags_json(undefined) ->
    ?null_json;
flags_json(Run) ->
    try_run_db(Run, fun flags_json_for_db/1, ?null_json).

flags_json_for_db(Db) ->
    case hubc_run_db:flags(Db) of
        {ok, Flags} ->
            hubc_json:encode({Flags});
        {error, Err} ->
            hubc_log:internal("Error getting flags: ~p~n", [Err]),
            ?null_json
    end.

%% ===================================================================
%% Summary JSON
%% ===================================================================

summary_json(undefined) -> ?null_json;
summary_json(Run) -> hubc_json:encode({summary_attrs(Run)}).

summary_attrs(Run) ->
    Status = hubc_run:status(Run),
    [{status,      Status},
     {time,        format_run_time(Status, Run)},
     {exit_status, format_exit_status(Run)}].

format_run_time(Status, Run) ->
    Start = hubc_run:int_attr(Run, "start", undefined),
    Stop = hubc_run:int_attr(Run, "stop", undefined),
    format_run_time(Status, Start, Stop).

format_run_time(_Status, T0, T1) when T0 /= undefined, T1 /= undefined ->
    (T1 - T0) div 1000;
format_run_time(running, T0, undefined) ->
    (hubc_run:timestamp() - T0) div 1000;
format_run_time(_Status, _T0, _T1) ->
    null.

format_exit_status(Run) ->
    hubc_run:int_attr(Run, "exit_status", null).

%% ===================================================================
%% Series JSON
%% ===================================================================

series_json(_Pattern, undefined, _Max) ->
    ?null_json;
series_json(Pattern, Run, Max) ->
    try_run_db(Run, fun series_json_for_db/3, [Pattern, Max], ?null_json).

series_json_for_db(Db, Pattern, Max) ->
    case hubc_run_db:series(Db, Pattern) of
        {ok, Series} ->
            hubc_json:encode({reduce_series(Series, Max)});
        {error, Err} ->
            hubc_log:internal("Error getting series: ~p~n", [Err]),
            ?null_json
    end.

reduce_series(Series, Max) ->
    [{Key, hubc_util:reduce_to(Vals, Max)} || {Key, Vals} <- Series].

%% ===================================================================
%% Output JSON
%% ===================================================================

output_json(undefined) ->
    ?null_json;
output_json(Run) ->
    try_run_db(Run, fun output_json_for_db/1, ?null_json).

output_json_for_db(Db) ->
    case hubc_run_db:output(Db) of
        {ok, Output} ->
            hubc_json:encode(format_output(Output));
        {error, Err} ->
            hubc_log:internal("Error getting output: ~p~n", [Err]),
            ?null_json
    end.

format_output(Output) ->
    [[Time div 1000, stream_id(Stream), Val]
     || {Time, Stream, Val} <- Output].

stream_id(stdout) -> 0;
stream_id(stderr) -> 1;
stream_id(_) -> null.

%% ===================================================================
%% Compare JSON
%% ===================================================================

compare_json(Sources, Runs, View) ->
    RunsJSON = runs_compare_json(Runs, Sources, View),
    ["[", hubc_util:list_join(RunsJSON, ","), "]"].

runs_compare_json(Runs, Sources, View) ->
    [run_compare_json(Run, Sources, View) || Run <- Runs].

run_compare_json(Run, Sources, View) ->
    RunJSON = hubc_json:encode(format_run(Run)),
    SourcesInnerJSON = run_sources_inner_json(Run, Sources, View),
    ["{\"run\":", RunJSON, SourcesInnerJSON, "}"].

run_sources_inner_json(Run, Sources, View) ->
    [run_source_inner_json(Run, Source, View) || Source <- Sources].

run_source_inner_json(Run, Source, View) ->
    [",\"", Source, "\":", run_source_json(Run, Source, View)].

run_source_json(Run, "flags", View) ->
    view_json(View, {flags, hubc_run:id(Run)});
run_source_json(Run, "summary", View) ->
    view_json(View, {summary, hubc_run:id(Run)});
run_source_json(Run, "series/" ++ Path, View) ->
    view_json(View, {series, Path, [{run, hubc_run:id(Run)}]});
run_source_json(_Run, _Source, _View) ->
    "null".

view_json(View, Request) -> hubc_project_view:json(View, Request).

%% ===================================================================
%% Utils / support
%% ===================================================================

try_run_db(Run, Handler, MissingResult) ->
    try_run_db(Run, Handler, [], MissingResult).

try_run_db(undefined, _Handler, _Args, MissingResult) ->
    MissingResult;
try_run_db(Run, Handler, Args, MissingResult) ->
    LogDir = hubc_run:dir(Run),
    case hubc_run_db:open(LogDir) of
        ok -> erlang:apply(Handler, [LogDir] ++ Args);
        {error, missing}  -> MissingResult
    end.
