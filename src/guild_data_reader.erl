%% Copyright 2016-2017 TensorHub, Inc.
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

-module(guild_data_reader).

-behavior(e2_task).

-export([start_link/3]).

-export([handle_task/1]).

%% Working exports for guild_view_v2 and related facilities
-export([series_json/3]).

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

handle_request({runs_json, RunRoots}) ->
    runs_json(RunRoots);
handle_request({flags_json, Run}) ->
    flags_json(Run);
handle_request({attrs_json, Run}) ->
    attrs_json(Run);
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

runs_json(RunRoots) ->
    Runs = guild_run:runs_for_runroots(RunRoots),
    guild_json:encode(format_runs(Runs)).

format_runs(Runs) ->
    sort_formatted_runs([format_run(Run) || Run <- Runs]).

format_run(Run) ->
    {[
      {id, guild_run:id(Run)},
      {dir, list_to_binary(guild_run:dir(Run))},
      {status, guild_run_util:run_status(Run)}
      |format_run_attrs(guild_run:attrs(Run))
     ]}.

format_run_attrs(Attrs) ->
    [format_run_attr(Attr) || Attr <- Attrs].

format_run_attr({Name, Val}) ->
    {list_to_binary(Name), format_attr_val(Name, Val)}.

format_attr_val("started",     Bin) -> binary_to_integer(Bin);
format_attr_val("stopped",     Bin) -> binary_to_integer(Bin);
format_attr_val("exit_status", Bin) -> binary_to_integer(Bin);
format_attr_val(_Name,         Bin) -> Bin.

sort_formatted_runs(Runs) ->
    Cmp = fun(A, B) -> run_start_time(A) > run_start_time(B) end,
    lists:sort(Cmp, Runs).

run_start_time({Attrs}) ->
    case lists:keyfind(<<"started">>, 1, Attrs) of
        {_, Val} -> Val;
        false -> 0
    end.

%% ===================================================================
%% Flags JSON
%% ===================================================================

flags_json(undefined) -> ?null_json;
flags_json(Run) ->
    try_run_db(Run, fun flags_json_for_db/1, ?null_json).

flags_json_for_db(Db) ->
    case guild_run_db:flags(Db) of
        {ok, Flags} ->
            guild_json:encode({Flags});
        {error, Err} ->
            guild_log:internal("Error getting flags: ~p~n", [Err]),
            ?null_json
    end.

%% ===================================================================
%% Attrs JSON
%% ===================================================================

attrs_json(undefined) -> ?null_json;
attrs_json(Run) ->
    try_run_db(Run, fun attrs_json_for_db/1, ?null_json).

attrs_json_for_db(Db) ->
    case guild_run_db:attrs(Db) of
        {ok, Attrs} ->
            guild_json:encode({Attrs});
        {error, Err} ->
            guild_log:internal("Error getting attrs: ~p~n", [Err]),
            ?null_json
    end.

%% ===================================================================
%% Summary JSON
%% ===================================================================

summary_json(undefined) -> ?null_json;
summary_json(Run) -> guild_json:encode({summary_attrs(Run)}).

summary_attrs(Run) ->
    Status = guild_run:status(Run),
    [{status,      Status},
     {time,        format_run_time(Status, Run)},
     {exit_status, format_exit_status(Run)}].

format_run_time(Status, Run) ->
    Start = guild_run:int_attr(Run, "started", undefined),
    Stop = guild_run:int_attr(Run, "stopped", undefined),
    format_run_time(Status, Start, Stop).

format_run_time(_Status, T0, T1) when T0 /= undefined, T1 /= undefined ->
    (T1 - T0) div 1000;
format_run_time(running, T0, undefined) ->
    (guild_run:timestamp() - T0) div 1000;
format_run_time(_Status, _T0, _T1) ->
    null.

format_exit_status(Run) ->
    guild_run:int_attr(Run, "exit_status", null).

%% ===================================================================
%% Series JSON
%% ===================================================================

series_json(_Pattern, undefined, _Max) ->
    ?null_json;
series_json(Pattern, Run, Max) ->
    try_run_db(Run, fun series_json_for_db/3, [Pattern, Max], ?null_json).

series_json_for_db(Db, Pattern, Max) ->
    case guild_run_db:series(Db, Pattern) of
        {ok, Series} ->
            guild_json:encode({reduce_series(Series, Max)});
        {error, Err} ->
            guild_log:internal("Error getting series: ~p~n", [Err]),
            ?null_json
    end.

reduce_series(Series, all) ->
    Series;
reduce_series(Series, Max) ->
    [{Key, guild_util:reduce_to(Vals, Max)} || {Key, Vals} <- Series].

%% ===================================================================
%% Output JSON
%% ===================================================================

output_json(undefined) ->
    ?null_json;
output_json(Run) ->
    try_run_db(Run, fun output_json_for_db/1, ?null_json).

output_json_for_db(Db) ->
    case guild_run_db:output(Db) of
        {ok, Output} ->
            guild_json:encode(format_output(Output));
        {error, Err} ->
            guild_log:internal("Error getting output: ~p~n", [Err]),
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
    ["[", guild_util:list_join(RunsJSON, ","), "]"].

runs_compare_json(Runs, Sources, View) ->
    [run_compare_json(Run, Sources, View) || Run <- Runs].

run_compare_json(Run, Sources, View) ->
    RunJSON = guild_json:encode(format_run(Run)),
    SourcesInnerJSON = run_sources_inner_json(Run, Sources, View),
    ["{\"run\":", RunJSON, SourcesInnerJSON, "}"].

run_sources_inner_json(Run, Sources, View) ->
    [run_source_inner_json(Run, Source, View) || Source <- Sources].

run_source_inner_json(Run, Source, View) ->
    [",\"", Source, "\":", run_source_json(Run, Source, View)].

run_source_json(Run, "flags", View) ->
    view_json(View, {flags, guild_run:id(Run)});
run_source_json(Run, "summary", View) ->
    view_json(View, {summary, guild_run:id(Run)});
run_source_json(Run, "series/" ++ Path, View) ->
    view_json(View, {series, Path, [{run, guild_run:id(Run)}]});
run_source_json(_Run, _Source, _View) ->
    "null".

view_json(View, Request) -> guild_project_view:json(View, Request).

%% ===================================================================
%% Utils / support
%% ===================================================================

try_run_db(Run, Handler, MissingResult) ->
    try_run_db(Run, Handler, [], MissingResult).

try_run_db(undefined, _Handler, _Args, MissingResult) ->
    MissingResult;
try_run_db(Run, Handler, Args, MissingResult) ->
    RunDir = guild_run:dir(Run),
    case guild_run_db:open(RunDir) of
        ok -> erlang:apply(Handler, [RunDir] ++ Args);
        {error, missing}  -> MissingResult
    end.
