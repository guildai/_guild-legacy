-module(hubc_tensorflow_runtime).

-behavior(e2_service).

-export([start_link/0]).

-export([handle_msg/3]).

-define(default_stats_task_repeat, 10000).
-define(eval_stop_timeout, 5000).

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
handle_msg({init_eval_op, Run, Section, Project}, _From, State) ->
    {reply, eval_op_reply(Run, Section, Project), State}.

%% ===================================================================
%% Train op
%% ===================================================================

train_op_reply(Section, Project) ->
    case hubc_project:section_attr(Section, "train") of
        {ok, Cmd} -> {ok, train_op(Cmd, Section, Project)};
        error -> {error, trainable}
    end.

train_op(CmdSpec, Section, Project) ->
    Flags = hubc_project_util:flags(Section, Project),
    LogDir = new_logdir(Section, Project),
    Cmd = cmd(CmdSpec, Project, Flags, LogDir, []),
    Tasks = train_op_tasks(LogDir, Section, Flags, Cmd),
    StreamHandlers = train_stream_handlers(LogDir),
    hubc_operation:new(Cmd, LogDir, Tasks, StreamHandlers).

new_logdir(Section, Project) ->
    Root = hubc_project_util:logroot(Section, Project),
    filename:join(Root, generate_logdir_name(Section)).

generate_logdir_name(Section) ->
    Name = hubc_model:name_for_project_section(Section, "tensorflow"),
    integer_to_list(erlang:system_time(milli_seconds)) ++ "-" ++ Name.

train_op_tasks(LogDir, Section, Flags, Cmd) ->
    StatsInterval = stats_interval_opt(Flags),
    hubc_runtime_support:op_tasks(
      [{run_db, LogDir, Flags, Cmd},
       {run_meta, LogDir, Section},
       {collector, "op-stats", LogDir, StatsInterval},
       {collector, "system-stats", LogDir, StatsInterval},
       {collector, "gpu-stats", LogDir, StatsInterval},
       {collector, "tensorflow-collector", LogDir, StatsInterval}]).

stats_interval_opt(Flags) ->
    case proplists:get_value("stats_interval", Flags) of
        undefined -> ?default_stats_task_repeat;
        I -> list_to_integer(I) * 1000
    end.

train_stream_handlers(LogDir) ->
    hubc_runtime_support:stream_handlers(
      [{logging, LogDir},
       console]).

%% ===================================================================
%% Eval op
%% ===================================================================

eval_op_reply(Run, Section, Project) ->
    case hubc_project:section_attr(Section, "evaluate") of
        {ok, Cmd} -> {ok, eval_op(Cmd, Run, Section, Project)};
        error -> {error, evaluatable}
    end.

eval_op(CmdSpec, Run, Section, Project) ->
    Flags = hubc_project_util:flags(Section, Project),
    LogDir = hubc_run:dir(Run),
    ExtraEnv = extra_eval_env(Run),
    Cmd = cmd(CmdSpec, Project, Flags, LogDir, ExtraEnv),
    Tasks = eval_op_tasks(Run),
    StreamHandlers = eval_stream_handlers(),
    hubc_operation:new(Cmd, LogDir, Tasks, StreamHandlers).

extra_eval_env(Run) ->
    [{"EVAL_RUN_ONCE", eval_run_once_flag(Run)}].

eval_run_once_flag(Run) ->
    case hubc_run:status(Run) of
        running -> "0";
        _       -> "1"
    end.

eval_op_tasks(Run) ->
    case hubc_run:os_pid(Run) of
        {ok, Pid} -> [exec_monitor_task(Pid)];
        {error, _} -> []
    end.

exec_monitor_task(ExecPid) ->
    {hubc_exec_monitor_task, start_link, [ExecPid, ?eval_stop_timeout]}.

eval_stream_handlers() ->
    hubc_runtime_support:stream_handlers([console]).

%% ===================================================================
%% Cmd support
%% ===================================================================

cmd(Spec, Project, Flags, LogDir, ExtraEnv) ->
    Env = [{"LOGDIR", LogDir}|ExtraEnv],
    BaseArgs = base_cmd_args(Spec, Env, Project),
    FlagArgs = flag_args(Flags, Env),
    Cwd = hubc_project:project_dir(Project),
    {BaseArgs ++ FlagArgs, [{cwd, Cwd}, {env, Env}]}.

base_cmd_args(Spec, Env, Project) ->
    [Mod|Args] = hubc_util:split_cmd(Spec),
    ResolvedArgs = resolve_env_refs(Args, Env),
    [python_exe(Project), "-um", Mod|ResolvedArgs].

resolve_env_refs(Args, Env) ->
    [resolve_arg_env_refs(Arg, Env) || Arg <- Args].

resolve_arg_env_refs(Arg, Env) ->
    Resolve = fun({Name, Val}, In) -> replace_env_refs(In, Name, Val) end,
    lists:foldl(Resolve, Arg, Env).

replace_env_refs(In, Name, Val) ->
    re:replace(In, "\\$" ++ Name, Val, [{return, list}, global]).

python_exe(_Project) ->
    hubc_util:find_exe("python").

flag_args(Flags, Env) ->
    lists:concat(
      [["--" ++ Name, resolve_arg_env_refs(Val, Env)]
       || {Name, Val} <- Flags]).
