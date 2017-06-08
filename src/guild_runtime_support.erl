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

-module(guild_runtime_support).

-export([op_tasks/1, op_stream_handlers/1]).

%% ===================================================================
%% Op tasks
%% ===================================================================

op_tasks(Specs) ->
    [op_task(Spec) || Spec <- Specs].

op_task({collector, Exe, Repeat}) ->
    Opts = [{repeat, Repeat}],
    {guild_collector_task, start_builtin, [Exe, Opts]};
op_task({collector, Exe, Repeat, StderrHandler}) ->
    Opts = [{repeat, Repeat}, {stderr_handler, StderrHandler}],
    {guild_collector_task, start_builtin, [Exe, Opts]};
op_task({log_flags, Flags}) ->
    {guild_log_flags_task, start_link, [Flags]};
op_task(log_system_attrs) ->
    {guild_log_system_attrs_task, start_link, []}.

%% ===================================================================
%% Op stream handlers
%% ===================================================================

op_stream_handlers(Specs) ->
    [op_stream_handler(Spec) || Spec <- Specs].

op_stream_handler(console) ->
    fun(_Op) -> fun log_output_to_console/1 end;
op_stream_handler(run_db_output) ->
    fun(Op) -> run_db_output_handler(Op) end.

log_output_to_console({stdout, Lines}) ->
    lists:foreach(fun({_, L}) -> println(user, L) end, Lines);
log_output_to_console({stderr, Lines}) ->
    lists:foreach(fun({_, L}) -> println(standard_error, L) end, Lines).

println(Device, Bin) ->
    io:format(Device, "~s~n", [Bin]).

run_db_output_handler(Op) ->
    RunDir = guild_op:cwd(Op),
    fun(Out) -> log_output_to_db(RunDir, Out) end.

log_output_to_db(RunDir, {Stream, Lines}) ->
    Output = format_output_for_db(Stream, Lines),
    handle_log_to_db_result(guild_run_db:log_output(RunDir, Output)).

format_output_for_db(Stream, Lines) ->
    [{Time, Stream, Line} || {Time, Line} <- Lines, filter_line_for_db(Line)].

filter_line_for_db([<<"\e", _/binary>>|_]) -> false;
filter_line_for_db(_) -> true.

handle_log_to_db_result(ok) -> ok;
handle_log_to_db_result({error, Err}) ->
    guild_log:internal("Error writing output to db: ~p~n", [Err]).
