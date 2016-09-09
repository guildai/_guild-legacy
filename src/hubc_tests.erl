-module(hubc_tests).

-compile([nowarn_unused_function, export_all]).

%% ===================================================================
%% Main
%% ===================================================================

run() ->
    hubc_trace:init_from_env(os:getenv("TRACE")),
    test_inifile_parse(),
    test_project(),
    test_input_buffer(),
    test_proc_waiting(),
    test_format_value(),
    test_exec(),
    test_operation(),
    test_split_cmd(),
    test_split_keyvals(),
    test_run_db(),
    test_reduce_to(),
    test_normalize_series(),
    test_collector_protocol().

run(Test) ->
    hubc_trace:init_from_env(os:getenv("TRACE")),
    F = list_to_atom("test_" ++ Test),
    ?MODULE:F().

start(Name) ->
    Padding = lists:duplicate(max(0, 25 - length(Name)), $\s),
    io:format("~s:~s", [Name, Padding]).

ok() ->
    io:format("OK~n").

%% ===================================================================
%% Ini file parse
%% ===================================================================

test_inifile_parse() ->
    start("initfile_parse"),

    P = fun(Bin) -> inifile:parse(Bin) end,

    %% Empty
    {ok, []} = P(<<>>),

    %% Single section, no attrs
    {ok, [{["foo"], []}]} = P("[foo]\n"),

    %% Single section, one attr
    {ok, [{["foo"], [{"bar", "123"}]}]} =
        P("[foo]\n"
          "bar = 123\n"),

    %% Single section, two attrs
    {ok, [{["foo"], [{"bar", "123"}, {"baz", "456"}]}]} =
        P("[foo]\n"
          "bar = 123\n"
          "baz = 456"),

    %% Single section, two attrs, various white space
    {ok, [{["foo"], [{"bar", "123"}, {"baz", "456"}]}]} =
        P("\n"
          "[  foo  ]\n"
          "\n"
          "bar       = 123   \n"
          "baz       = 456\n"
          "   \n"),

    %% Multiple sections, multiple values
    {ok, [{["s-1"],
           [{"a-1", "123"},
            {"a-2", "456"}]},
          {["s-2"],
           [{"a-1", "789"},
            {"a-2", "012"}]}]} =
        P("[s-1]\n"
          "a-1 = 123\n"
          "a-2 = 456\n"
          "[s-2]\n"
          "a-1 = 789\n"
          "a-2 = 012\n"),

    %% Alternate attr delimiter (':')
    {ok, [{["foo"], [{"bar", "123"}, {"baz", "456"}]}]} =
        P("[foo]\n"
          "bar: 123\n"
          "baz: 456"),

    %% Typed section
    {ok, [{["foo", "bar"], []}]} = P("[foo \"bar\"]\n"),

    %% Attr without a section
    {error, {no_section_for_attr, "foo = bar", 1}} = P("foo = bar"),
    {error, {no_section_for_attr, "foo = bar", 3}} = P("\n\nfoo = bar"),

    %% Malformed section
    {error, {section_line, "[foo", 1}} = P("[foo\n"),
    {error, {section_line, "[bar", 3}} = P("[foo]\n\n[bar\n"),

    %% Malformed attr
    {error, {attr_line, "bar", 2}} = P("[foo]\nbar"),

    %% Attrs with no value
    {ok, [{["foo"], [{"bar", ""}]}]} = P("[foo]\nbar="),
    {ok, [{["foo"], [{"bar", ""}]}]} = P("[foo]\nbar:"),

    ok().

%% ===================================================================
%% Project support
%% ===================================================================

test_project() ->
    start("project"),

    M = hubc_project,

    %% Empty project
    {ok, []} = M:from_str(<<>>),

    %% Typical project
    {ok, P} =
        M:from_str(
          "[model \"mnist_softmax\"]\n"
          "runtime = tensorflow\n"
          "dataset = mnist\n"
          "train = mnist_softmax --train\n"
          "eval = mnist_softtmax --eval\n"
          "\n"
          "[model \"mnist_cnn\"]\n"
          "runtime = tensorflow\n"
          "dataset = mnist\n"
          "train = mnist_cnn_train\n"
          "eval = mnist_cnn_eval\n"
          "\n"
          "[dataset \"mnist\"]\n"
          "prepare = mnist_data"),

    %% Attr API

    {ok, "tensorflow"} = M:attr(P, ["model", "mnist_softmax"], "runtime"),
    error = M:attr(P, ["model", "mnist_softmax"], "no_match"),
    error = M:attr(P, ["no_match"], "runtime"),

    P2 = M:set_attr(P, ["model", "mnist_softmax"], "runtime", "caffe"),
    {ok, "caffe"} = M:attr(P2, ["model", "mnist_softmax"], "runtime"),

    P3 = M:set_attr(P, ["model", "mnist_softmax"], "new_attr", "skull badger"),
    {ok, "skull badger"} = M:attr(P3, ["model", "mnist_softmax"], "new_attr"),

    P4 = M:set_attr(P, ["new_section"], "new_attr", "Peruvian mongoose"),
    {ok, "Peruvian mongoose"} = M:attr(P4, ["new_section"], "new_attr"),

    %% Section API

    [{["model", "mnist_softmax"], _},
     {["model", "mnist_cnn"], _}]
        = M:sections(P, ["model"]),

    [{["model", "mnist_softmax"], _}]
        = M:sections(P, ["model", "mnist_softmax"]),

    [] = M:sections(P, ["model", "no_match"]),

    [] = M:sections(P, ["no_watch"]),

    {ok, MnistDataset} = M:section(P, ["dataset", "mnist"]),

    "mnist" = M:section_name(MnistDataset),
    undefined = M:section_name({["dataset"], []}),

    [{"prepare", "mnist_data"}] = M:section_attrs(MnistDataset),
    [{"prepare", "mnist_data"}] = M:section_attrs(P, ["dataset", "mnist"]),
    [] = M:section_attrs(P, ["no_match"]),

    {ok, "mnist_data"} = M:section_attr(MnistDataset, "prepare"),

    error = M:section(P, ["no_match"]),

    {ok, P5} =
        M:from_str(
          "[a]\n"
          "v1 = a1\n"
          "v2 = a2\n"
          "\n"
          "[b]\n"
          "v1 = b1\n"
          "v3 = b3\n"),

    P5Union = fun(Paths) -> lists:sort(M:section_attr_union(P5, Paths)) end,

    [] = P5Union([]),
    [] = P5Union(["no_match"]),
    [{"v1", "a1"}, {"v2", "a2"}] = P5Union([["a"]]),
    [{"v1", "b1"}, {"v3", "b3"}] = P5Union([["b"]]),
    [{"v1", "a1"}, {"v2", "a2"}, {"v3", "b3"}] = P5Union([["a"], ["b"]]),
    [{"v1", "b1"}, {"v2", "a2"}, {"v3", "b3"}] = P5Union([["b"], ["a"]]),

    ok().

%% ===================================================================
%% Input buffer
%% ===================================================================

test_input_buffer() ->
    start("input_buffer"),

    %% An empty input buffer is created using new_input_buffer.

    New = fun() -> hubc_util:new_input_buffer() end,

    %% The input function is used to process incoming input - it returns a tuple
    %% of finalized input lines and the next buffer state.

    Input = fun(Buf_, Bin) -> hubc_util:input(Buf_, Bin) end,

    %% Finalize is used to return any remaining buffered lines.

    Finalize = fun(Buf) -> hubc_util:finalize_input(Buf) end,

    %% Binary parts are buffered up to a new line.

    {[], Buf1} = Input(New(), <<>>),
    {[], Buf2} = Input(Buf1, <<"this is ">>),
    {[], Buf3} = Input(Buf2, <<"partial input, ">>),
    {[{_Time1, Line1}], Buf4}
        = Input(Buf3, <<"that eventually ends\n">>),

    %% %% Each line is a list of its submitted input parts

    [<<"this is ">>,
     <<"partial input, ">>,
     <<"that eventually ends">>] = Line1,

    %% In this case there's no further buffered input.

    [] = Finalize(Buf4),

    %% Multiple lines may appear in one input - each has the same timestamp.

    {[{Time2, [<<"line2">>]},
      {Time2, [<<"line3">>]},
      {Time2, [<<"line4">>]}], Buf5}
        = Input(Buf4, <<"line2\nline3\nline4\npartial">>),

    [{Time2, [<<"partial">>]}] = Finalize(Buf5),

    %% We can continue with input.

    {[{Time2, [<<"partial">>, <<", now complete">>]}], Buf6}
        = Input(Buf5, <<", now complete\nagain partial">>),

    [{Time3, [<<"again partial">>]}] = Finalize(Buf6),

    {[{Time3, [<<"again partial">>, <<", and complete">>]},
      {_Time4, [<<"line5">>]},
      {_Time5, [<<"line6">>]}], Buf7}
        = Input(Buf6, <<", and complete\nline5\nline6\n">>),

    [] = Finalize(Buf7),

    ok().

%% ===================================================================
%% Proc waiting
%% ===================================================================

test_proc_waiting() ->
    start("proc_waiting"),

    %% Overall timeout for test
    {ok, Timeout} = timer:exit_after(1000, self(), timeout),

    %% We need to start the app to make hubc_proc available.

    {ok, _} = application:ensure_all_started(tensorhub_client),

    %% Waiting on a scope or proc that doesn't exist returns immediately
    ok = hubc_proc:wait_for(scope, some_scope),
    {error, noproc} = hubc_proc:wait_for(proc, some_proc),

    %% Register a short lived process and wait for its scope
    P1 = drone(100, p1),
    hubc_proc:reg(some_scope, P1),
    ok = hubc_proc:wait_for(scope, some_scope),

    %% Register a short lived process and wait for it
    P2 = drone(100, p2),
    hubc_proc:reg(some_scope, P2),
    {P2, p2} = hubc_proc:wait_for(proc, P2),

    %% Combine waiting for proc and scope using independent threads
    P3 = drone(100, p3),
    hubc_proc:reg(another_scope, P3),
    P4 = drone(100, p4),
    hubc_proc:reg(another_scope, P4),

    %% Simulate independent threads with waiter processes
    W1 = scope_waiter(another_scope, self()),
    W2 = proc_waiter(P3, self()),
    W3 = proc_waiter(P4, self()),

    %% Selectively receive replies from our waiters (can't use sys
    %% waiting here for all threads as waits are blocking/synchronous)

    receive {W1, W1Resp} -> ok = W1Resp end,
    receive {W2, W2Resp} -> {P3, p3} = W2Resp end,
    receive {W3, W3Resp} -> {P4, p4} = W3Resp end,

    timer:cancel(Timeout),
    ok().

drone(ExitAfter, ExitReason) ->
    spawn(fun() -> timer:sleep(ExitAfter), exit(ExitReason) end).

scope_waiter(Scope, Parent) ->
    spawn(
      fun() ->
              Resp = hubc_proc:wait_for(scope, Scope),
              erlang:send(Parent, {self(), Resp})
      end).

proc_waiter(Proc, Parent) ->
    spawn(
      fun() ->
              Resp = hubc_proc:wait_for(proc, Proc),
              erlang:send(Parent, {self(), Resp})
      end).

%% ===================================================================
%% Format value
%% ===================================================================

test_format_value() ->
    start("format_value"),

    F = fun(Val, Fmt) ->
            %% Flatten string for test readabilty below
            flatten_string(hubc_template_lib:format_value(Val, Fmt))
        end,

    %% No formatting

    "hello" = F("hello", undefined),
    1       = F(1, undefined),
    1.0     = F(1.0, undefined),

    %% Number

    "1"                  = F(1,              "number"),
    "567"                = F(567,            "number"),
    "1,000"              = F(1000,           "number"),
    "1,567"              = F(1567,           "number"),
    "20,567"             = F(20567,          "number"),
    "12,345,678,901,234" = F(12345678901234, "number"),

    %% Precent

    "50.00%" = F(0.5, "percent"),

    %% Duration (seconds)

    "0s"            = F(duration(0,  0, 0,  0), "duration"),
    "1s"            = F(duration(0,  0, 0,  1), "duration"),
    "1m 0s"         = F(duration(0,  0, 1,  0), "duration"),
    "1h 0m 0s"      = F(duration(0,  1, 0,  0), "duration"),
    "1d 0h 0m 0s"   = F(duration(1,  0, 0,  0), "duration"),
    "1m 15s"        = F(duration(0,  0, 1, 15), "duration"),
    "3h 1m 15s"     = F(duration(0,  3, 1, 15), "duration"),
    "12d 3h 1m 15s" = F(duration(12, 3, 1, 15), "duration"),

    ok().

flatten_string(L) when is_list(L) -> lists:flatten(L);
flatten_string(Other) -> Other.

duration(D, H, M, S) ->
    D * 86400 + H * 3600 + M * 60 + S.

%% ===================================================================
%% Exec
%% ===================================================================

test_exec() ->
    start("exec"),

    hubc_app:init_support(exec),

    Args = [test_exec_bin(), "arg1", "arg2"],
    Opts = [stdout, stderr, {env, [{"exit", "2"}]}],

    %% We've configured the process to exit with an error code and
    %% because we're linked we need to trap the exit.

    process_flag(trap_exit, true),

    {ok, Pid, OSPid} = hubc_exec:run_link(Args, Opts),

    {stderr, OSPid, <<"test start\n">>}      = recv_msg(),
    {stdout, OSPid, <<"args: arg1 arg2\n">>} = recv_msg(),
    {stderr, OSPid, <<"test stop\n">>}       = recv_msg(),
    {'EXIT', Pid, {exit_status, Status}}     = recv_msg(),
    timeout                                  = recv_msg(100),

    %% Exit status for exec needs to be decoded.

    {status, 2} = exec:status(Status),

    %% Cleanup

    process_flag(trap_exit, false),

    ok().

recv_msg() -> recv_msg(1000).

recv_msg(Timeout) ->
    receive
        Msg -> Msg
    after
        Timeout -> timeout
    end.

test_exec_bin() ->
    "priv/bin/test-exec".

%% ===================================================================
%% Test operation
%% ===================================================================

test_operation() ->
    start("operation"),

    %% A TensorHub operation is an external command execution along
    %% with some additional support:
    %%
    %% - Support for logging to a logdir
    %% - Execution and monitoring of op tasks, which run alongside the
    %%   operation
    %% - Notification of operation stream lines (stdout and stderr) to
    %%   a list of stream handlers
    %%
    %% When we create an operation, we provide the command to execute,
    %% a logdir, a list of op tasks, and a list of stream handlers.

    %% A command is a tuple of args and command options. For this test
    %% we want our operation to take long enough to perform some
    %% background tasks (see below) - we use the "sleep" env to
    %% specify a number of seconds to sleep. We'll also tell to exit
    %% with a non-zero status to see how errors are handled.

    Args = [test_exec_bin(), "arg1", "arg2"],
    Opts = [{env, [{"sleep", "1"}]}],
    Cmd = {Args, Opts},

    %% The logdir is the location where TensorHub events, time series
    %% stats, etc. are logged.

    LogDir = init_logdir(),

    %% Op tasks are tasks that are started just before the command is
    %% executed and supervised until the command has completed or they
    %% are shutdown explicitly. By default a task is associated with
    %% the command and will be stopped when the command
    %% terminates. However, a task may be configured to remain running
    %% after the command has terminated, in which case they must be
    %% shut down elsewhere.

    %% For these tests we'll create a couple tasks - one that writes
    %% values to a file in logdir and another that maintains its own
    %% state.

    Task1 = {hubc_test_task, start_link, [{write_files, LogDir}, 100]},

    {ok, TaskBuf} = hubc_test_buffer:start_link(),
    Task2 = {hubc_test_task, start_link, [{write_to_buf, TaskBuf}, 100]},

    %% Finally we'll register a single stream handler that will
    %% capture everything from the operation command.

    {ok, Output} = hubc_test_buffer:start_link(),
    StreamHandler = fun(Msg) -> hubc_test_buffer:add(Output, Msg) end,

    %% Operations are configured separately from their start
    %% function. This is to support the separation of op configuration
    %% by a runtime specific module from the process of starting and
    %% monitoring the operation.

    Op = hubc_operation:new(Cmd, LogDir, [Task1, Task2], [StreamHandler]),

    %% Operations require various system facilities, which are best
    %% provided by the app.

    {ok, _} = application:ensure_all_started(tensorhub_client),
    hubc_app:init_support(exec),

    %% Lets confirm our state before running the operation.

    [] = hubc_optask_sup:tasks(),
    [] = list_dir_sorted(LogDir),
    [] = hubc_test_buffer:get_all(TaskBuf),
    [] = hubc_test_buffer:get_all(Output),

    %% We start the operation using the op supervisor.

    {ok, Pid} = hubc_operation_sup:start_op(test_op, Op),

    %% After a short time, both of our tasks should be running.

    timer:sleep(100),
    [_, _] = hubc_optask_sup:tasks(),

    %% To synchronize the call we'll wait for the process to terminate
    %% or timeout.

    hubc_proc:reg(Pid),
    {Pid, normal}  = hubc_proc:wait_for(proc, Pid),

    %% Here's our operation output.

    [{stderr, [{T1, [<<"test start">>]}]},
     {stdout, [{T2, [<<"args: arg1 arg2">>]}]},
     {stderr, [{T3, [<<"test stop">>]}]}] = hubc_test_buffer:get_all(Output),

    %% Each output block (one or more lines, as read together from the
    %% external process) is tagged with an epoch microsecond timestamp.

    Now = erlang:system_time(micro_seconds),
    true = T1 =< T2,
    true = T2 =< T3,
    true = T3 =< Now,

    %% Our tasks should have performed some work during the
    %% operation. As these activities aren't strictly coordinated (the
    %% tasks perform work at regular intervals) we can't assert
    %% precisely the results, but based on the timings, we should see
    %% roughly ten changes made by our tasks. We'll assert the first few.

    [0, 1, 2, 3, 4, 5|_] = hubc_test_buffer:get_all(TaskBuf),
    ["errors.log", "test-00", "test-01", "test-02", "test-03",
    "test-04", "test-05"|_] = list_dir_sorted(LogDir),

    %% Op tasks should terminate cleanly once the operation is
    %% stopped.

    [] = hubc_optask_sup:tasks(),

    %% Cleanup

    ok = hubc_test_buffer:stop(TaskBuf),
    ok = hubc_test_buffer:stop(Output),

    ok().

init_logdir() ->
    LogDir = "/tmp/hubc_test.logdir",
    [] = os:cmd("rm -rf \"" ++ LogDir ++ "\""),
    [] = os:cmd("mkdir \"" ++ LogDir ++ "\""),
    LogDir.

list_dir_sorted(Dir) ->
    {ok, L} = file:list_dir(Dir),
    lists:sort(L).

%% ===================================================================
%% Split cmd
%% ===================================================================

test_split_cmd() ->
    start("split_cmd"),

    Split = fun hubc_util:split_cmd/1,

    %% Basic usage.

    [] = Split(""),
    ["foo"] = Split("foo"),
    ["foo", "bar"] = Split("foo bar"),
    ["foo", "this is bar", "and", "this is baz"]
        = Split("foo \"this is bar\" and \"this is baz\""),


    %% Does not support escapted quotes properly.

    ["contains \\", "-", "an", "escaped", "quote"]
        = Split("\"contains \\\" - an escaped quote\""),

    ok().

%% ===================================================================
%% Split keyvals
%% ===================================================================

test_split_keyvals() ->
    start("split_keyvals"),

    Split = fun hubc_util:split_keyvals/1,

    [] = Split(""),

    %% Basic use

    [{"foo", "123"}] = Split("foo=123"),
    [{"foo", "123"}, {"bar", "456"}] = Split("foo=123 bar=456"),

    %% Double quotes can be used to include spaces

    [{"foo", "contains spaces"},
     {"bar", "doesn't"}]
        = Split("foo=\"contains spaces\" bar=doesn't"),

    %% Escaped double quotes aren't supported

    [{"foo", "contains \\"}]
        = Split("foo=\"contains \\\" - an escapted quote\""),

    ok().

%% ===================================================================
%% Run DB
%% ===================================================================

test_run_db() ->
    start("run_db"),

    M = hubc_run_db,

    {ok, _} = application:ensure_all_started(tensorhub_client),

    %% Ensure our test log dir is empty.

    LogDir = init_logdir(),

    %% If we open without the create_if_missing, we get an error.

    {error, missing} = M:open(LogDir),
    ok = M:open(LogDir, [create_if_missing]),

    %% Initial the db is empty.

    {ok, []} = M:flags(LogDir),
    {ok, []} = M:series_keys(LogDir),

    %% Let's add some flags.

    ok = M:log_flags(LogDir, [{"foo", "123"}, {"bar", "456"}]),

    %% And some series values.

    Vals =
        [{"foo", [[123, 1, 1.0], [124, 2, 1.1], [125, 3, 1.2]]},
         {"bar", [[123, 1, 2.0], [124, 2, 2.1], [125, 3, 2.2]]},
         {"baz", [[123, 1, 3.0], [124, 2, 3.1], [125, 3, 3.2]]}],
    ok = M:log_series_values(LogDir, Vals),
    ok = M:log_series_values(LogDir, []),
    ok = M:log_series_values(LogDir, [{"foo", []}]),

    %% And some output.

    ok = M:log_output(LogDir, [{1, stdout, <<"Out 1">>},
                               {2, stdout, <<"Out 2">>}]),
    ok = M:log_output(LogDir, [{3, stderr, <<"Err 1">>},
                               {4, stderr, <<"Err 2">>}]),

    %% Let's read some data back!

    {ok, [{<<"bar">>, <<"456">>},
          {<<"foo">>, <<"123">>}]} = M:flags(LogDir),

    {ok, [<<"bar">>, <<"baz">>, <<"foo">>]} = M:series_keys(LogDir),

    %% Series are read back using a regex pattern.

    {ok, []} = M:series(LogDir, "nomatch"),

    {ok, [{<<"foo">>, [[123, 1, 1.0],
                       [124, 2, 1.1],
                       [125, 3, 1.2]]}]} = M:series(LogDir, "foo"),

    {ok, [{<<"bar">>, [[123, 1, 2.0],
                       [124, 2, 2.1],
                       [125, 3, 2.2]]}]} = M:series(LogDir, "bar"),

    {ok, [{<<"baz">>, [[123, 1, 3.0],
                       [124, 2, 3.1],
                       [125, 3, 3.2]]}]} = M:series(LogDir, "baz"),

    {ok, [{<<"bar">>, [[123, 1, 2.0],
                       [124, 2, 2.1],
                       [125, 3, 2.2]]},
          {<<"baz">>, [[123, 1, 3.0],
                       [124, 2, 3.1],
                       [125, 3, 3.2]]},
          {<<"foo">>, [[123, 1, 1.0],
                       [124, 2, 1.1],
                       [125, 3, 1.2]]}]} = M:series(LogDir, ".*"),

    {ok, [{<<"bar">>, [[123, 1, 2.0],
                       [124, 2, 2.1],
                       [125, 3, 2.2]]},
          {<<"foo">>, [[123, 1, 1.0],
                       [124, 2, 1.1],
                       [125, 3, 1.2]]}]} = M:series(LogDir, "(foo|bar)"),

    %% Output comes back in the order logged.

    {ok, [{1, stdout, <<"Out 1">>},
          {2, stdout, <<"Out 2">>},
          {3, stderr, <<"Err 1">>},
          {4, stderr, <<"Err 2">>}]} = M:output(LogDir),

    %% We can update flags.

    ok = M:log_flags(LogDir, [{"foo", "456"}, {"bar", "789"}]),

    {ok, [{<<"bar">>, <<"789">>},
          {<<"foo">>, <<"456">>}]} = M:flags(LogDir),

    %% Let's close up!

    ok = hubc_run_db:close(LogDir),

    ok().

%% ===================================================================
%% Reduce to
%% ===================================================================

test_reduce_to() ->
    start("reduce_to"),

    R = fun hubc_util:reduce_to/2,

    [] = R([],         1),
    [1] = R([1],       1),
    [2] = R([1, 2],    1),
    [3] = R([1, 2, 3], 1),

    []     = R([],        2),
    [1]    = R([1],       2),
    [1, 2] = R([1, 2],    2),
    [1, 3] = R([1, 2, 3], 2),

    L1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],

    [10] = R(L1, 1),
    [4, 10] = R(L1, 2),
    [2, 6, 10] = R(L1, 3),
    [1, 4, 7, 10] = R(L1, 4),
    [1, 4, 7, 10] = R(L1, 5),
    [2, 4, 6, 8, 10] = R(L1, 6),
    [2, 4, 6, 8, 10] = R(L1, 7),
    [2, 4, 6, 8, 10] = R(L1, 8),
    [2, 4, 6, 8, 10] = R(L1, 9),
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] = R(L1, 10),
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] = R(L1, 11),

    ok().

%% ===================================================================
%% Normalize series
%% ===================================================================

test_normalize_series() ->
    start("normalize_series"),

    N = fun hubc_util:normalize_series/3,

    L1 = [[1, 1], [2, 2], [3, 3]],

    [[1, 1], [2, 2], [3, 3]] = N(L1, 3, last),

    [[1, 1], [3, 3]]         = N(L1, 2, last),
    [[1, 1], [3, 2]]         = N(L1, 2, first),
    [[1, 1], [3, 2]]         = N(L1, 2, min),
    [[1, 1], [3, 3]]         = N(L1, 2, max),

    [[3, 3]]                 = N(L1, 1, last),
    [[3, 1]]                 = N(L1, 1, first),
    [[3, 1]]                 = N(L1, 1, min),
    [[3, 3]]                 = N(L1, 1, max),

    %% Here we have a series that is trying to land every 10 units or
    %% so. We start at 1000, then miss by 2 on 1008, then overshoot by
    %% 2 on 1022. The fourth observation however skips an interval at
    %% 1045, and then another lands on 1050. This simulates a lagged
    %% observation.

    L2 = [[1000, 1], [1008, 2], [1022, 3], [1045, 4], [1050, 5]],

    %% Here we ask for five normalized observations. But because the
    %% fourth observation is so close to the fifth, it falls out,
    %% being replaced by the more recent value.

    [[1006, 1], [1017, 2], [1028, 3], [1050, 5]] = N(L2, 5, last),

    %% We can alternatively ask that the first, min, and max values be
    %% used when consolidating values that occur in the same epoch.

    [[1006, 1], [1017, 2], [1028, 3], [1050, 4]] = N(L2, 5, first),
    [[1006, 1], [1017, 2], [1028, 3], [1050, 4]] = N(L2, 5, min),
    [[1006, 1], [1017, 2], [1028, 3], [1050, 5]] = N(L2, 5, max),

    %% If we ask for enough resolution, we'll get each of our
    %% observations verbatim.

    [[1000, 1], [1008, 2], [1022, 3], [1045, 4], [1050, 5]] = N(L2, 100, last),

    ok().

%% ===================================================================
%% Collector protocol
%% ===================================================================

test_collector_protocol() ->
    start("collector_protocol"),

    hubc_app:init_support(json),

    KeyValJson = fun(KVs) -> hubc_json:encode({[{kv, {KVs}}]}) end,
    M = hubc_collector_protocol,
    Decode = fun(Bin) -> M:input(M:new_input_buffer(), Bin) end,
    EmptyBuf = {[], undefined},

    %% Empty values - nothing to handle.

    {[], EmptyBuf} = Decode(""),
    {[], EmptyBuf} = Decode(<<>>),

    %% Chunks are delimited by "\n\n". Partial chunks are buffered.

    {[], {[], {_, [<<"123">>]}}} = Decode("123"),

    %% Legal JSON that doesn't conform to the protocol is tagged as
    %% 'other'.

    {[{_, {other, 1}}], EmptyBuf} = Decode("1\n\n"),

    %% Illeal JSON is tagged as 'invalid'.

    {[{_, {invalid, [<<"foo">>]}}], EmptyBuf} = Decode("foo\n\n"),

    %% Key value JSON is tagged as 'kv'.

    JKV = KeyValJson([{foo, 123}, {bar, 456}, {baz, 789}]),
    {[{_, {kv, DKV}}], EmptyBuf} = Decode([JKV, "\n\n"]),

    123 = proplists:get_value(<<"foo">>, DKV),
    456 = proplists:get_value(<<"bar">>, DKV),
    789 = proplists:get_value(<<"baz">>, DKV),

    %% Multiple chunks can be decoded.

    {[{_, {other, 1}}, {_, {other, 2}}], EmptyBuf}
        = Decode(<<"1\n\n2\n\n">>),

    %% Input can be provided incrementally and include single '\n'
    %% chars within chunks.

    B0                                  = M:new_input_buffer(),
    {[],                   B1}          = M:input(B0, "1"),
    {[{_, {other, 1}}],    B2}          = M:input(B1, "\n\n"),
    {[{_, {other, 2}}],    B3}          = M:input(B2, "2\n\n3"),
    {[],                   B4}          = M:input(B3, "45"),
    {[{_, {other, 3456}}], B5}          = M:input(B4, "6\n\n"),
    {[{_, {other, [1,2,3]}}], EmptyBuf} = M:input(B5, "[1,\n2,\n3]\n\n"),

    %% A delimiter in isolation "\n\n" is consider an eof marker and
    %% may be used to signal an end of transmission.

    {[{_, eof}], EmptyBuf} = Decode("\n\n"),

    {[{_, {other, 1}}, {_, {other,2}}, {_, eof}], EmptyBuf}
        = Decode("1\n\n2\n\n\n\n"),

    ok().
