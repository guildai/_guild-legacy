%% Copyright 2106 TensorHub, Inc.
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

-module(guild_tensorflow_runtime).

-export([start_link/0]).

-export([handle_msg/3]).

-define(default_stats_task_repeat, 10000).
-define(eval_stop_timeout, 5000).

start_link() ->
    e2_service:start_link(?MODULE, []).

handle_msg({init_train_op, Section, Project}, _From, State) ->
    {reply, train_op_reply(Section, Project), State};
handle_msg({init_eval_op, Run, Section, Project}, _From, State) ->
    {reply, eval_op_reply(Run, Section, Project), State};
handle_msg({init_prepare_op, Section, Project}, _From, State) ->
    {reply, prepare_op_reply(Section, Project), State};
handle_msg({init_serve_op, Run, Section, Project}, _From, State) ->
    {reply, serve_op_reply(Run, Section, Project), State}.

%% ===================================================================
%% Train op
%% ===================================================================

train_op_reply(Section, Project) ->
    train_op_for_spec(op_spec(Section, "train"), Section, Project).

train_op_for_spec({ok, Spec}, Section, Project) ->
    train_op_for_validated_requires(
      validate_train_requires(Section, Project),
      Spec, Section, Project);
train_op_for_spec(error, _Section, _Project) ->
    {error, trainable}.

validate_train_requires(Section, Project) ->
    case guild_project:section_attr(Section, "train_requires") of
        {ok, Requires} ->
            validate_train_requires(requires_list(Requires, Project));
        error ->
            ok
    end.

requires_list(Requires, Project) ->
    Dir = guild_project:project_dir(Project),
    [filename:absname(Path, Dir) || Path <- split_requires(Requires)].

split_requires(Requires) ->
    re:split(Requires, "\s*,\s*", [{return, list}]).

validate_train_requires([Path|Rest]) ->
    case filelib:is_file(Path) of
        true -> validate_train_requires(Rest);
        false -> {error, Path}
    end;
validate_train_requires([]) -> ok.

train_op_for_validated_requires(ok, Spec, Section, Project) ->
    {ok, train_op(Spec, Section, Project)};
train_op_for_validated_requires({error, Missing}, _Cmd, _Section, _Project) ->
    {error, {missing_requires, Missing}}.

train_op(TrainSpec, Section, Project) ->
    RunDirSpec = train_rundir_spec(Section),
    Flags = guild_project_util:flags(Section, Project),
    Cmd = cmd_args(TrainSpec, Flags),
    Env = train_extra_env(),
    Opts =
        [{env, Env},
         {app_support, train_app_support()},
         {tasks, train_tasks(Flags, Env)},
         {stream_handlers, train_stream_handlers()}],
    guild_operation:new(RunDirSpec, Section, Project, Cmd, Opts).

train_rundir_spec(Section) ->
    Name = guild_model:name_for_project_section(Section, "tensorflow"),
    {new, [{suffix, "-" ++ Name}]}.

train_extra_env() -> base_extra_env().

train_app_support() -> [json].

train_tasks(Flags, Env) ->
    Repeat = stats_interval_opt(Flags),
    guild_runtime_support:op_tasks(
      [{flags, resolve_flag_vals(Flags, Env)},
       {collector, "op-stats", Repeat},
       {collector, "system-stats", Repeat},
       {collector, "gpu-stats", Repeat},
       {collector,
        "tensorflow-collector",
        Repeat,
        collector_stderr_handler()}]).

stats_interval_opt(Flags) ->
    case proplists:get_value("stats_interval", Flags) of
        undefined -> ?default_stats_task_repeat;
        I -> list_to_integer(I) * 1000
    end.

resolve_flag_vals(Flags, Env) ->
    guild_util:resolve_keyvals(Flags, Env).

train_stream_handlers() ->
    guild_runtime_support:op_stream_handlers([console, run_db_output]).

collector_stderr_handler() ->
    {fun handle_collector_stderr/2, guild_util:new_input_buffer()}.

handle_collector_stderr(Bin, Buf) ->
    {Lines, Next} = guild_util:input(Buf, Bin),
    lists:foreach(fun log_collector_stderr/1, Lines),
    Next.

log_collector_stderr({_, [<<"I ", _/binary>>|_]}) -> ok;
log_collector_stderr({_, Line}) -> guild_log:internal(Line).

%% ===================================================================
%% Eval op
%% ===================================================================

eval_op_reply(Run, Section, Project) ->
    case op_spec(Section, "evaluate") of
        {ok, EvalSpec} -> {ok, eval_op(EvalSpec, Run, Section, Project)};
        error -> {error, evaluatable}
    end.

eval_op(EvalSpec, Run, Section, Project) ->
    RunDirSpec = eval_rundir_spec(Run),
    Flags = guild_project_util:flags(Section, Project),
    Cmd = cmd_args(EvalSpec, Flags),
    Opts =
        [{env, eval_extra_env(Run)},
         {tasks, eval_tasks(Run)},
         {stream_handlers, base_stream_handlers()}],
    guild_operation:new(RunDirSpec, Section, Project, Cmd, Opts).

eval_rundir_spec(Run) ->
    {overlay, guild_run:dir(Run)}.

eval_extra_env(Run) ->
    [{"RUNDIR", guild_run:dir(Run)},
     {"EVAL_RUN_ONCE", eval_run_once_flag(Run)}
     |base_extra_env()].

eval_run_once_flag(Run) ->
    guild_app:init_support(exec),
    case guild_run_util:run_status(Run) of
        running -> "0";
        _       -> "1"
    end.

eval_tasks(Run) ->
    case guild_run_util:run_os_pid(Run) of
        {ok, Pid} -> [exec_monitor_task(Pid)];
        error -> []
    end.

exec_monitor_task(ExecPid) ->
    {guild_exec_monitor_task, start_link, [ExecPid, ?eval_stop_timeout]}.

%% ===================================================================
%% Prepare op
%% ===================================================================

prepare_op_reply(Section, Project) ->
    case op_spec(Section, "prepare") of
        {ok, Spec} -> {ok, prepare_op(Spec, Section, Project)};
        error -> {error, preparable}
    end.

prepare_op(PrepareSpec, Section, Project) ->
    RunDirSpec = none,
    Flags = guild_project_util:flags(Section, Project),
    Cmd = cmd_args(PrepareSpec, Flags),
    Opts = [{stream_handlers, base_stream_handlers()}],
    guild_operation:new(RunDirSpec, Section, Project, Cmd, Opts).

%% ===================================================================
%% Serve op
%% ===================================================================

serve_op_reply(Run, Section, Project) ->
    case op_spec(Section, "serve") of
        {ok, ServeSpec} -> {ok, serve_op(ServeSpec, Run, Section, Project)};
        error -> {error, servable}
    end.

serve_op(ServeSpec, Run, Section, Project) ->
    RunDirSpec = serve_rundir_spec(Run),
    Flags = guild_project_util:flags(Section, Project),
    Cmd = cmd_args(ServeSpec, Flags),
    Opts =
        [{env, serve_extra_env(Run)},
         {stream_handlers, base_stream_handlers()}],
    guild_operation:new(RunDirSpec, Section, Project, Cmd, Opts).

serve_rundir_spec(Run) ->
    {overlay, guild_run:dir(Run)}.

serve_extra_env(Run) ->
    [{"RUNDIR", guild_run:dir(Run)}|base_extra_env()].

%% ===================================================================
%% Op support
%% ===================================================================

op_spec(Section, Name) ->
    case guild_project:section_attr(Section, Name) of
        {ok, ""} -> error;
        {ok, Spec} -> {ok, Spec};
        error -> error
    end.

cmd_args(CmdSpec, Flags) ->
    Python = guild_util:find_exe("python"),
    [Module|Args] = guild_util:split_cmd(CmdSpec),
    [Python, "-um", Module] ++ Args ++ flag_args(Flags).

flag_args(Flags) ->
    lists:concat([["--" ++ Name, Val] || {Name, Val} <- Flags]).

base_extra_env() ->
    [{"GPU_COUNT", gpu_count_env()}].

gpu_count_env() ->
    integer_to_list(length(guild_sys:gpu_attrs())).

base_stream_handlers() ->
    guild_runtime_support:op_stream_handlers([console]).
