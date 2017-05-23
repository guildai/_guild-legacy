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

-module(guild_tests).

-compile([nowarn_unused_function, export_all]).

%% ===================================================================
%% Main
%% ===================================================================

run() ->
    guild_trace:init_from_env(os:getenv("TRACE")),
    test_inifile(),
    test_project(),
    test_input_buffer(),
    test_proc_waiting(),
    test_exec(),
    test_operation(),
    test_split_cmd(),
    test_split_keyvals(),
    test_run_db(),
    test_reduce_to(),
    test_normalize_series(),
    test_collector_protocol(),
    test_make_tmp_dir(),
    test_delete_tmp_dir(),
    test_consult_string(),
    test_port_io(),
    test_tensorflow_port_protocol(),
    test_tensorflow_read_image(),
    test_strip_project_sections().

run(Test) ->
    guild_trace:init_from_env(os:getenv("TRACE")),
    F = list_to_atom("test_" ++ Test),
    ?MODULE:F().

start(Name) ->
    Padding = lists:duplicate(max(0, 25 - length(Name)), $\s),
    io:format("~s:~s", [Name, Padding]).

ok() ->
    io:format("OK~n").

ensure_started(Mod) ->
    case Mod:start_link() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        {error, Err} -> error({start_link, Mod, Err})
    end.

%% ===================================================================
%% Ini file
%% ===================================================================

test_inifile() ->
    start("inifile"),

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

    %% Comments
    {ok, [{["foo"], [{"bar", "123"}]}]} =
        P("# This is a comment\n"
          "[foo]\n"
          "# Another comment\n"
          "; An alternative comment\n"
          "bar = 123"),

    %% Line continuation
    {ok, [{["foo"],
           [{"bar","1       2       3"},
            {"baz","4, 5, 6,   8, 9, 10"}]}]} =
        P("[foo]\n"
          "bar = 1 \\\n"
          "      2 \\\n"
          "      3\n"
          "\n"
          "baz = \\\n"
          "  4, 5, 6, \\\n"
          "  8, 9, 10\n"),

    %% Attr without a section
    {error, {no_section_for_attr, 1}} = P("foo = bar"),
    {error, {no_section_for_attr, 3}} = P("\n\nfoo = bar"),

    %% Malformed section
    {error, {section_line, 1}} = P("[foo\n"),
    {error, {section_line, 3}} = P("[foo]\n\n[bar\n"),

    %% Malformed attr
    {error, {attr_line, 2}} = P("[foo]\nbar"),
    {error, {attr_line, 3}} = P("[foo]\nbar=123\nbaz"),

    %% Line continuation at eof
    {error, {eof, 2}} = P("[foo]\nbar = 123 \\"),
    {error, {eof, 3}} = P("[foo]\nbar = 123 \\\n\\"),

    %% Attrs with no value
    {ok, [{["foo"], [{"bar", ""}]}]} = P("[foo]\nbar="),
    {ok, [{["foo"], [{"bar", ""}]}]} = P("[foo]\nbar:"),

    ok().

%% ===================================================================
%% Project support
%% ===================================================================

test_project() ->
    start("project"),

    M = guild_project,

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

    New = fun() -> guild_util:new_input_buffer() end,

    %% The input function is used to process incoming input - it returns a tuple
    %% of finalized input lines and the next buffer state.

    Input = fun(Buf_, Bin) -> guild_util:input(Buf_, Bin) end,

    %% Finalize is used to return any remaining buffered lines.

    Finalize = fun(Buf) -> guild_util:finalize_input(Buf) end,

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

    %% We need to start the app to make guild_proc available.

    {ok, _} = application:ensure_all_started(guild),

    %% Waiting on a scope or proc that doesn't exist returns immediately
    ok = guild_proc:wait_for({scope, some_scope}),
    {error, noproc} = guild_proc:wait_for({proc, some_proc}),

    %% Register a short lived process and wait for its scope
    P1 = drone(100, p1),
    guild_proc:reg(some_scope, P1),
    ok = guild_proc:wait_for({scope, some_scope}),

    %% Register a short lived process and wait for it
    P2 = drone(100, p2),
    guild_proc:reg(some_scope, P2),
    {P2, p2} = guild_proc:wait_for({proc, P2}),

    %% Combine waiting for proc and scope using independent threads
    P3 = drone(100, p3),
    guild_proc:reg(another_scope, P3),
    P4 = drone(100, p4),
    guild_proc:reg(another_scope, P4),

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
              Resp = guild_proc:wait_for({scope, Scope}),
              erlang:send(Parent, {self(), Resp})
      end).

proc_waiter(Proc, Parent) ->
    spawn(
      fun() ->
              Resp = guild_proc:wait_for({proc, Proc}),
              erlang:send(Parent, {self(), Resp})
      end).

%% ===================================================================
%% Exec
%% ===================================================================

test_exec() ->
    start("exec"),

    guild_app:init_support(exec),

    Args = [test_exec_bin(), "arg1", "arg2"],
    Opts = [stdout, stderr, {env, [{"exit", "2"}]}],

    %% We've configured the process to exit with an error code and
    %% because we're linked we need to trap the exit.

    process_flag(trap_exit, true),

    {ok, Pid, OSPid} = guild_exec:run_link(Args, Opts),

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
    filename:absname("priv/test/bin/test-exec").

%% ===================================================================
%% Operation
%% ===================================================================

test_operation() ->
    start("operation"),

    {ok, _} = application:ensure_all_started(guild),

    %% A Guild operation is an external command associated with a
    %% project section that implements a particular stage of a model's
    %% lifecycle.
    %%
    %% Operations are associated with runs. They can either create a
    %% new run or overlay an existing run.
    %%
    %% In addition to executing the external operation command, an
    %% operation supports a number of other steps:
    %%
    %% - Set up the rundir, creating a new one or using an existing
    %% - Write run metadata including the section name, start status,
    %%   exit status, etc.
    %% - Snapshot the project at the time the run was started
    %% - Configure logging of internal Guild errors to the rundir
    %% - Set up operation IO handlers (stream handlers)
    %% - Starts a number of operation tasks (op tasks)
    %%
    %% When we create an operation, we provide a rundir spec (see
    %% below), the applicable project section and project, the command
    %% arguments and addition options. These options include a command
    %% environment (env), Guild app support needed by the operation
    %% (e.g. json support), a list of op tasks, and a list of stream
    %% handlers.

    %% Let's start with the rundir spec, which specifies how the
    %% operation should handle the rundir. There are two scenarios:
    %%
    %% - Create a new rundir
    %% - Use an existing rundir
    %%
    %% When creating a new rundir, provide a spec in the form {new,
    %% Opts}. Opts may contain an a suffix option that specifies the
    %% suffix to use when creating a new rundir.
    %%
    %% If an operation applies to an existing run, the rundir spec
    %% should be {overlay, RunDir}. In this case the operation will use
    %% the existing rundir.
    %%
    %% In this test let's create a new rundir.

    RunDirSpec = {new, "-test"},

    %% Next we provide information about the applicable project
    %% section. This is used to identify the runroot, which is section
    %% dependent, as well as write information about section including
    %% a snapshot of the project itself.

    ProjectDir = init_sample_project(),
    {ok, Project} = guild_project:from_dir(ProjectDir),
    {ok, Section} = guild_project:section(Project, ["model"]),

    %% Next we'll define the command args. In this case we'll use a
    %% test operation.

    CmdArgs = [test_exec_bin(), "arg1", "arg2"],

    %% We can also provide an environment for the command. In this
    %% case our test operaation will look for SLEEP for an interval to
    %% sleep.

    CmdEnv = [{"SLEEP", "0.5"}],

    %% Op tasks are started just after the operation command is
    %% executed and supervised until the command has completed or they
    %% are stopped explicitly. A task is associated with the command
    %% and will typically stop once the operation has exited, but that
    %% behavior is task specific.
    %%
    %% For these tests we'll create a test task that writes files to
    %% a file in rundir.

    Task1 = {guild_test_task, start_link, [100]},

    %% Finally we'll register a single stream handler. We register a
    %% factory, which returns a handler for an operation process - in
    %% some cases a handler may need to know details about an
    %% operation that are only available once the operation has started.
    %%
    %% In this test we'll use a stream handler that uses an output
    %% buffer to capture the operation's streams.

    {ok, Output} = guild_test_buffer:start_link(),
    StreamHandlerInit =
        fun(_Op) -> fun(Msg) -> guild_test_buffer:add(Output, Msg) end end,

    %% With this we can finally create our op. We first create an op
    %% structure.

    Opts =
        [{env, CmdEnv},
         {tasks, [Task1]},
         {stream_handlers, [StreamHandlerInit]}],
    Op = guild_operation:new(RunDirSpec, Section, Project, CmdArgs, Opts),

    %% Lets confirm our state before running the operation.

    [] = guild_optask_sup:tasks(),
    [] = guild_test_buffer:get_all(Output),

    %% We start the operation using the op supervisor.

    {ok, OpPid} = guild_operation_sup:start_op(test_op, Op),

    %% After a short time we should see some op tasks, one of which is
    %% our task (an operation starts other core tasks in addition to
    %% what we specify).

    timer:sleep(200),
    [_|_] = guild_optask_sup:tasks(),

    %% With the operation underway, we can get the run directory it
    %% created.

    RunDir = guild_operation:rundir(OpPid),

    %% To synchronize the call we'll wait for the process to terminate
    %% or timeout.

    guild_proc:reg(OpPid),
    {OpPid, normal}  = guild_proc:wait_for({proc, OpPid}),

    %% Here's our operation output.

    [{stderr, [{T1, [<<"test start">>]}]},
     {stdout, [{T2, [<<"args: arg1 arg2">>]}]},
     {stderr, [{T3, [<<"test stop">>]}]}]
        = guild_test_buffer:get_all(Output),

    %% Each output block (one or more lines, as read together from the
    %% external process) is tagged with an epoch microsecond timestamp.

    Now = erlang:system_time(micro_seconds),
    true = T1 =< T2,
    true = T2 =< T3,
    true = T3 =< Now,

    %% Our task should have performed some work during the
    %% operation. As this activity isn't strictly coordinated (the
    %% tasks perform work at regular intervals) we can't assert
    %% precise results. Our test task writes sample files
    %% approximately every 100 milliseconds and our test operation is
    %% configured to sleep for 500 milliseconds (see SLEEP env of
    %% "0.5" seconds above) - we should expect to see 4 to 5 sample
    %% files.
    %%
    %% In addition the operation creates a guild.d directory, which
    %% contains details about the operation.

    ["guild.d",
     "test-00",
     "test-01",
     "test-02",
     "test-03",
     "test-04"|_] = list_dir_sorted(RunDir),

    %% Op tasks should terminate cleanly once the operation is
    %% stopped. We'll wait a moment to let the task stop.

    timer:sleep(10),

    [] = guild_optask_sup:tasks(),

    %% Cleanup

    ok = guild_test_buffer:stop(Output),
    ok = guild_util:delete_tmp_dir(ProjectDir),

    ok().

init_sample_project() ->
    {ok, ProjectDir} = guild_util:make_tmp_dir(),
    SampleSrc = filename:join(guild_app:test_dir(), "sample-project"),
    "" = os:cmd("rsync -a \"" ++ SampleSrc ++ "/\" \"" ++ ProjectDir ++ "\""),
    ProjectDir.

init_rundir() ->
    RunDir = "/tmp/guild_test.rundir",
    [] = os:cmd("rm -rf \"" ++ RunDir ++ "\""),
    [] = os:cmd("mkdir \"" ++ RunDir ++ "\""),
    RunDir.

list_dir_sorted(Dir) ->
    {ok, L} = file:list_dir(Dir),
    lists:sort(L).

%% ===================================================================
%% Split cmd
%% ===================================================================

test_split_cmd() ->
    start("split_cmd"),

    Split = fun guild_util:split_cmd/1,

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

    Split = fun guild_util:split_keyvals/1,

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

    M = guild_run_db,

    {ok, _} = application:ensure_all_started(guild),

    %% Ensure our test run dir is empty.

    RunDir = init_rundir(),

    %% If we open without the create_if_missing, we get an error.

    {error, missing} = M:open(RunDir),
    ok = M:open(RunDir, [create_if_missing]),

    %% Initially the db is empty.

    {ok, []} = M:flags(RunDir),
    {ok, []} = M:series_keys(RunDir),

    %% Let's add some flags.

    ok = M:log_flags(RunDir, [{"foo", "123"}, {"bar", "456"}]),

    %% And some attrs.

    ok = M:log_attrs(RunDir, [{"baz", "654"}, {"bam", "321"}]),

    %% And some series values.

    Vals =
        [{"foo", [[123, 1, 1.0], [124, 2, 1.1], [125, 3, 1.2]]},
         {"bar", [[123, 1, 2.0], [124, 2, 2.1], [125, 3, 2.2]]},
         {"baz", [[123, 1, 3.0], [124, 2, 3.1], [125, 3, 3.2]]}],
    ok = M:log_series_values(RunDir, Vals),
    ok = M:log_series_values(RunDir, []),
    ok = M:log_series_values(RunDir, [{"foo", []}]),

    %% And some output.

    ok = M:log_output(RunDir, [{1, stdout, <<"Out 1">>},
                               {2, stdout, <<"Out 2">>}]),
    ok = M:log_output(RunDir, [{3, stderr, <<"Err 1">>},
                               {4, stderr, <<"Err 2">>}]),

    %% Let's read some data back!

    {ok, [{<<"bar">>, <<"456">>},
          {<<"foo">>, <<"123">>}]} = M:flags(RunDir),

    {ok, [{<<"bam">>, <<"321">>},
          {<<"baz">>,<<"654">>}]} = M:attrs(RunDir),

    {ok, [<<"bar">>, <<"baz">>, <<"foo">>]} = M:series_keys(RunDir),

    %% Series are read back using a regex pattern.

    {ok, []} = M:series(RunDir, "nomatch"),

    {ok, [{<<"foo">>, [[123, 1, 1.0],
                       [124, 2, 1.1],
                       [125, 3, 1.2]]}]} = M:series(RunDir, "foo"),

    {ok, [{<<"bar">>, [[123, 1, 2.0],
                       [124, 2, 2.1],
                       [125, 3, 2.2]]}]} = M:series(RunDir, "bar"),

    {ok, [{<<"baz">>, [[123, 1, 3.0],
                       [124, 2, 3.1],
                       [125, 3, 3.2]]}]} = M:series(RunDir, "baz"),

    {ok, [{<<"bar">>, [[123, 1, 2.0],
                       [124, 2, 2.1],
                       [125, 3, 2.2]]},
          {<<"baz">>, [[123, 1, 3.0],
                       [124, 2, 3.1],
                       [125, 3, 3.2]]},
          {<<"foo">>, [[123, 1, 1.0],
                       [124, 2, 1.1],
                       [125, 3, 1.2]]}]} = M:series(RunDir, ".*"),

    {ok, [{<<"bar">>, [[123, 1, 2.0],
                       [124, 2, 2.1],
                       [125, 3, 2.2]]},
          {<<"foo">>, [[123, 1, 1.0],
                       [124, 2, 1.1],
                       [125, 3, 1.2]]}]} = M:series(RunDir, "(foo|bar)"),

    %% Output comes back in the order logged.

    {ok, [{1, stdout, <<"Out 1">>},
          {2, stdout, <<"Out 2">>},
          {3, stderr, <<"Err 1">>},
          {4, stderr, <<"Err 2">>}]} = M:output(RunDir),

    %% We can update flags.

    ok = M:log_flags(RunDir, [{"foo", "456"}, {"bar", "789"}]),

    {ok, [{<<"bar">>, <<"789">>},
          {<<"foo">>, <<"456">>}]} = M:flags(RunDir),

    %% Let's close up!

    ok = M:close(RunDir),

    ok().

%% ===================================================================
%% Reduce to
%% ===================================================================

test_reduce_to() ->
    start("reduce_to"),

    R = fun guild_util:reduce_to/2,

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

    N = fun guild_util:normalize_series/3,

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

    guild_app:init_support(json),

    KeyValJson = fun(KVs) -> guild_json:encode({[{kv, {KVs}}]}) end,
    M = guild_collector_protocol,
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

%% ===================================================================
%% Make tmp dir
%% ===================================================================

test_make_tmp_dir() ->
    start("make_tmp_dir"),

    Dirs = make_tmp_dirs(100),
    confirm_files_exist(Dirs, true),

    delete_tmp_dirs(Dirs),
    confirm_files_exist(Dirs, false),

    ok().

make_tmp_dirs(N) -> make_tmp_dirs_acc(N, []).

make_tmp_dirs_acc(0, Acc) ->
    Acc;
make_tmp_dirs_acc(N, Acc) ->
    {ok, Dir} = guild_util:make_tmp_dir(),
    make_tmp_dirs_acc(N - 1, [Dir|Acc]).

delete_tmp_dirs(Dirs) ->
    lists:foreach(fun delete_tmp_dir/1, Dirs).

delete_tmp_dir(Dir) ->
    ok = guild_util:delete_tmp_dir(Dir).

confirm_files_exist(Files, Exist) ->
    lists:foreach(fun(F) -> confirm_file_exists(F, Exist) end, Files).

confirm_file_exists(F, Exist) ->
    case filelib:is_file(F) of
        Exist -> ok;
        true -> error({file_exists, F});
        false -> error({file_not_exist, F})
    end.

%% ===================================================================
%% Delete tmp dir
%% ===================================================================

test_delete_tmp_dir() ->
    start("delete_tmp_dir"),

    {ok, Tmp} = guild_util:make_tmp_dir(),
    confirm_file_exists(Tmp, true),

    ok = guild_util:delete_tmp_dir(Tmp),
    confirm_file_exists(Tmp, false),

    %% Because delete tmp dir is a potentially dangerous operation, it
    %% provides safeguards to prevent unintentional deletion of
    %% important directories.

    %% First, directories must be absolute.

    {'EXIT', {{refuse_delete_dir, "./foo/bar", relative}, _}}
        = (catch guild_util:delete_tmp_dir("./foo/bar")),

    %% Second, directories must be greater than two levels deep.

    {'EXIT', {{refuse_delete_dir, "/___foo___", min_depth}, _}}
        = (catch guild_util:delete_tmp_dir("/___foo___")),

    %% Note we're not testing "/" or any other likely top level
    %% directory here for obvious reasons.

    ok().

%% ===================================================================
%% Consult string
%% ===================================================================

test_consult_string() ->
    start("consult_string"),

    CS = fun guild_util:consult_string/1,

    {ok, []} = CS(""),

    {ok, [1]}    = CS("1."),
    {ok, [1, 2]} = CS("1. 2."),
    {ok, [1, 2]} = CS("1.\n2.\n"),

    {ok, [[1, 2, 3], {3, 4, 6}]} = CS("[1, 2, 3].\n{3, 4, 6}."),
    {ok, [[1, 2, 3], {3, 4, 6}]} = CS("[1,\n2,\n3].\n{3,\n4,\n6}."),

    {error, {1, erl_parse, ["syntax error before: ", []]}} = CS("1"),
    {error, {4, erl_parse, ["syntax error before: ", []]}} = CS("\n\n\n1"),
    {error, {1, erl_parse, ["syntax error before: ", []]}} = CS("1. 1"),
    {error, {4, erl_parse, ["syntax error before: ", []]}} = CS("1.\n\n\n1"),
    {error, {5, erl_parse, ["syntax error before: ", []]}} = CS("\n1.\n\n\n1"),

    ok().

%% ===================================================================
%% Port IO
%% ===================================================================

test_port_io() ->
    start("port_io"),

    {ok, F} = port_io:start_link("priv/bin/test-port"),

    %% Helpers

    Send =
        fun(Msg) ->
            ok = file:write(F, [length(Msg), Msg])
        end,

    Recv =
        fun() ->
            {ok, <<RespLen>>} = file:read(F, 1),
            {ok, Resp} = file:read(F, RespLen),
            Resp
        end,

    %% Send msg to port via file write

    Send("hello"),

    %% Read response from port via file read

    <<"you said hello">> = Recv(),

    %% Send requests consecutively

    Send("cat"), Send("dog"), Send("bird"),

    <<"you said cat">> = Recv(),
    <<"you said dog">> = Recv(),
    <<"you said bird">> = Recv(),

    %% Ask port to exit by sending 0, wait and confirm

    file:write(F, [0]),
    timer:sleep(100),
    false = erlang:is_process_alive(F),

    ok().

%% ===================================================================
%% Tensorflow port protocol
%% ===================================================================

test_tensorflow_port_protocol() ->
    start("tensorflow_port_protocol"),

    ensure_started(guild_tensorflow_port),
    ok = guild_tensorflow_port:test_protocol(),

    ok().

%% ===================================================================
%% Tensorflow read image
%% ===================================================================

test_tensorflow_read_image() ->
    start("tensorflow_read_image"),

    ensure_started(guild_tensorflow_port),

    Load = fun guild_tensorflow_port:read_image/2,

    %% Image from a non-existing directory

    {error, <<"not found">>} = Load("", 1),

    RunDir = filename:join(guild_app:test_dir(), "sample-rundir"),

    {ok, #{tag:=<<"images/image/0">>, bytes:=<<137,80,78,_/binary>>}}
        = Load(RunDir, 0),

    {ok, #{tag:=<<"images/image/1">>, bytes:=<<137,80,78,_/binary>>}}
        = Load(RunDir, 1),

    ok().

%% ===================================================================
%% Strip project sections
%% ===================================================================

test_strip_project_sections() ->
    start("strip_project_sections"),

    S = fun(Bin, Path) ->
                iolist_to_binary(guild_project_util:strip_sections(Bin, Path))
        end,

    <<>> = S("", "s1"),
    <<>> = S("[s1]", "s1"),
    <<>> = S("[s1]\nv1 = 123", "s1"),
    <<>> = S("[s \"1\"]", "s"),

    <<"[s2]\nv1 = 456">> = S("[s1]\nv1 = 123\n[s2]\nv1 = 456", "s1"),
    <<"[s2]\nv1 = 456">> = S("[s2]\nv1 = 456", "s1"),

    <<"# Header comment\n"
      "\n"
      "[s2]\n"
      "# s2 comment\n"
      "v1 = 321\n"
      "\n">> = S("# Header comment\n"
                 "\n"
                 "[s1]\n"
                 "\n"
                 "v1 = 123\n"
                 "v2 = 456 789\n"
                 "\n"
                 "[s2]\n"
                 "# s2 comment\n"
                 "v1 = 321\n"
                 "\n"
                 "[s1]\n"
                 "\n"
                 "# s1 comment\n"
                 "v1 = 987 654\n",
                 "s1"),
    ok().
