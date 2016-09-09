-module(hubc_runtime_support).

-export([op_tasks/1, stream_handlers/1]).

op_tasks(Specs) ->
    [op_task(Spec) || Spec <- Specs].

op_task({run_db, LogDir, Flags, Cmd}) ->
    {hubc_run_db_task, start_link, [LogDir, Flags, Cmd]};
op_task({run_meta, LogDir, ProjectSection}) ->
    Attrs = [{model, hubc_project:section_name(ProjectSection)}],
    {hubc_run_meta_task, start_link, [LogDir, Attrs]};
op_task({collector, Exe, LogDir, Repeat}) ->
    TaskOpts = [{repeat, Repeat}],
    {hubc_collector_task, start_builtin, [Exe, LogDir, TaskOpts]}.

stream_handlers(Specs) ->
    [stream_handler(Spec) || Spec <- Specs].

stream_handler({logging, LogDir}) ->
    fun(Output) -> log_output_to_db(LogDir, Output) end;
stream_handler(console) ->
    fun log_output_to_console/1.

log_output_to_console({stdout, Lines}) ->
    lists:foreach(fun({_, L}) -> println(user, L) end, Lines);
log_output_to_console({stderr, Lines}) ->
    lists:foreach(fun({_, L}) -> println(standard_error, L) end, Lines).

println(Device, Bin) -> io:format(Device, "~s~n", [Bin]).

log_output_to_db(LogDir, {Stream, Lines}) ->
    Output = format_output_for_db(Stream, Lines),
    handle_log_to_db_result(hubc_run_db:log_output(LogDir, Output)).

format_output_for_db(Stream, Lines) ->
    [{Time, Stream, Line} || {Time, Line} <- Lines].

handle_log_to_db_result(ok) -> ok;
handle_log_to_db_result({error, Err}) ->
    hubc_log:internal("Error writing output to db: ~p~n", [Err]).
