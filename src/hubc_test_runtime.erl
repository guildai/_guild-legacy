-module(hubc_test_runtime).

-behavior(e2_service).

-export([start_link/0]).

-export([handle_msg/3]).

-define(default_stats_task_repeat, 1000).
-define(max_timeseries_size, 1000).

%% ===================================================================
%% Start
%% ===================================================================

start_link() ->
    e2_service:start_link(?MODULE, [], [registered]).

%% ===================================================================
%% Dispatch
%% ===================================================================

handle_msg({init_train_op, Section, Project}, _From, State) ->
    {reply, train_op_reply(Section, Project), State};
handle_msg({init_eval_op, _Run, _Section, _Project}, _From, State) ->
    {reply, {error, evaluatable}, State}.

%% ===================================================================
%% Train op
%% ===================================================================

train_op_reply(Section, Project) ->
    case hubc_project:section_attr(Section, "train") of
        {ok, Cmd} -> {ok, train_cmd_op(Cmd, Section, Project)};
        error -> {error, trainable}
    end.

train_cmd_op(CmdSpec, Section, Project) ->
    Flags = hubc_project_util:flags(Section, Project),
    LogDir = logdir(Section, Project),
    Cmd = cmd(CmdSpec, Project, Flags, LogDir),
    Tasks = train_op_tasks(LogDir, Section, Flags, Cmd),
    StreamHandlers = train_stream_handlers(LogDir),
    hubc_operation:new(Cmd, LogDir, Tasks, StreamHandlers).

logdir(Section, Project) ->
    Root = hubc_project_util:logroot(Section, Project),
    filename:join(Root, generate_logdir_name(Section)).

generate_logdir_name(Section) ->
    Name = hubc_model:name_for_project_section(Section, "test"),
    integer_to_list(erlang:system_time(milli_seconds)) ++ "-" ++ Name.

cmd(Spec, Project, Flags, LogDir) ->
    Args = hubc_util:split_cmd(Spec),
    Cwd = hubc_project:project_dir(Project),
    Env = flags_env(Flags) ++ [{"LOGDIR", LogDir}],
    {Args, [{cwd, Cwd}, {env, Env}]}.

flags_env(Flags) ->
    [{string:to_upper(Name), Val} || {Name, Val} <- Flags].

train_op_tasks(LogDir, Section, Flags, Cmd) ->
    StatsInterval = stats_interval_opt(Flags),
    hubc_runtime_support:op_tasks(
      [{run_db, LogDir, Flags, Cmd},
       {run_meta, LogDir, Section},
       {collector, "system-stats", LogDir, StatsInterval},
       {collector, "op-stats", LogDir, StatsInterval},
       {collector, "test-collector", LogDir, StatsInterval}]).

stats_interval_opt(Flags) ->
    case proplists:get_value("stats_interval", Flags) of
        undefined -> ?default_stats_task_repeat;
        I -> list_to_integer(I) * 1000
    end.

train_stream_handlers(LogDir) ->
    hubc_runtime_support:stream_handlers(
      [{logging, LogDir},
       console]).
