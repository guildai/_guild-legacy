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

-module(guild_train_op).

-behavior(guild_op).

-export([from_spec/3, cmd_info/1]).

-export([init/1, handle_task/1, handle_msg/3]).

-record(op, {section, project, cmd, tasks, stream_handlers}).

-record(state, {op, started, cmd, rundir, exec_pid, exec_ospid,
                stream_handlers, stdout_buf, stderr_buf}).

-define(default_stats_task_repeat, 10000).

%% ===================================================================
%% Init
%% ===================================================================

from_spec(Spec, Section, Project) ->
    Flags = guild_project_util:flags(Section, Project),
    CmdArgs = guild_op_support:python_cmd(Spec, Flags),
    CmdEnv = static_env(),
    {?MODULE,
     #op{
        section=Section,
        project=Project,
        cmd={CmdArgs, CmdEnv},
        tasks=tasks(Flags),
        stream_handlers=stream_handlers()}}.

static_env() ->
    [{"PKGHOME", guild_app:pkg_dir()},
     {"GPU_COUNT", gpu_count_env()}].

gpu_count_env() ->
    integer_to_list(length(guild_sys:gpu_attrs())).

tasks(Flags) ->
    base_tasks(Flags) ++ collector_tasks(Flags).

base_tasks(Flags) ->
    [{guild_run_status_task, start_link, []},
     {guild_run_db_task, start_link, []},
     {guild_log_flags_task, start_link, [Flags]},
     {guild_log_system_attrs_task, start_link, []}].

collector_tasks(Flags) ->
    Repeat = stats_interval_opt(Flags),
    [collector("tensorflow-collector", Repeat),
     collector("op-stats", Repeat),
     collector("sys-stats", Repeat),
     collector("gpu-stats", Repeat)].

stats_interval_opt(Flags) ->
    case proplists:get_value("stats_interval", Flags) of
        undefined -> ?default_stats_task_repeat;
        I -> list_to_integer(I) * 1000
    end.

collector(Script, Repeat) ->
    {guild_collector_task, start_link, [Script, [{repeat, Repeat}]]}.

stream_handlers() ->
    guild_op_support:op_stream_handlers([console, run_db_output]).

%% ===================================================================
%% Cmd info
%% ===================================================================

cmd_info(#op{cmd={Args, Env}}) ->
    #{args => Args, env => Env}.

%% ===================================================================
%% Init
%% ===================================================================

init(Op) ->
    guild_proc:reg(operation, self()),
    process_flag(trap_exit, true),
    {ok, init_state(Op)}.

init_state(Op) ->
    #state{
       op=Op,
       stdout_buf=guild_util:new_input_buffer(),
       stderr_buf=guild_util:new_input_buffer()}.

%% ===================================================================
%% Task impl
%% ===================================================================

handle_task(State) ->
    Next = guild_util:fold_apply(op_steps(), State),
    {wait_for_msg, Next}.

op_steps() ->
    [fun init_app_support/1,
     fun started_timestamp/1,
     fun init_rundir/1,
     fun init_cmd/1,
     fun init_run_meta/1,
     fun snapshot_project/1,
     fun init_errors_log/1,
     fun init_stream_handlers/1,
     fun start_exec/1,
     fun start_tasks/1].

init_app_support(State) ->
    guild_app:init_support([exec, json]),
    State.

started_timestamp(S) ->
    S#state{started=guild_run:timestamp()}.

%% ===================================================================
%% Rundir
%% ===================================================================

init_rundir(State) ->
    RunDir = rundir_path(State),
    guild_rundir:init(RunDir),
    gproc:add_local_property(cwd, RunDir),
    State#state{rundir=RunDir}.

rundir_path(State) ->
    RunRoot = runroot(State),
    Name = rundir_name(State),
    filename:join(RunRoot, Name).

runroot(#state{op=#op{section=Section, project=Project}}) ->
    guild_project_util:runroot(Section, Project).

rundir_name(#state{started=Started, op=#op{section=Section}}) ->
    format_started(Started) ++ rundir_suffix(Section).

format_started(RunStarted) ->
    Now = guild_run:timestamp_to_now(RunStarted),
    {{Y, M, D}, {H, Mn, S}} = calendar:now_to_universal_time(Now),
    io_lib:format("~b~2..0b~2..0bT~2..0b~2..0b~2..0bZ", [Y, M, D, H, Mn, S]).

rundir_suffix(Section) ->
    case guild_model:name_for_project_section(Section) of
        {ok, Name} -> "-" ++ safe_path(Name);
        error -> ""
    end.

safe_path(Suffix) ->
    re:replace(Suffix, "/", "_", [global, {return, list}]).

%% ===================================================================
%% Cmd
%% ===================================================================

init_cmd(#state{op=#op{cmd={Args, BaseEnv}}}=State) ->
    Env = run_env(State) ++ BaseEnv,
    ResolvedArgs = guild_util:resolve_args(Args, Env),
    State#state{cmd={ResolvedArgs, Env}}.

run_env(#state{rundir=RunDir}) -> [{"RUNDIR", RunDir}].

%% ===================================================================
%% Run meta
%% ===================================================================

init_run_meta(#state{rundir=RunDir}=State) ->
    guild_rundir:write_attrs(RunDir, run_attrs(State)),
    State.

run_attrs(#state{op=#op{section=Section}, started=Started, cmd={Cmd, Env}}) ->
    [section_name_attr(Section),
     {started, Started},
     {cmd, format_cmd_attr(Cmd)},
     {env, format_env_attr(Env)}].

section_name_attr({[Type], _}) ->
    {Type, ""};
section_name_attr({[Type, Name|_], _}) ->
    {Type, Name}.

format_cmd_attr(Cmd) ->
    guild_util:format_cmd_args(Cmd).

format_env_attr(Env) ->
    [[Name, "=", Val, "\n"] || {Name, Val} <- Env].

%% ===================================================================
%% Snapshot project
%% ===================================================================

snapshot_project(#state{rundir=RunDir,
                        op=#op{section=Section, project=Project}}=State) ->
    Bin = guild_app:priv_bin("snapshot-project"),
    ProjectDir = guild_project:project_dir(Project),
    GuildDir = guild_rundir:guild_dir(RunDir),
    Sources = section_sources(Section, Project),
    guild_exec:run_quiet([Bin, ProjectDir, GuildDir, Sources]),
    State.

section_sources({SectionPath, _}, Project) ->
    Attrs =
        guild_project:section_attr_union(
          Project, [SectionPath, ["project"]]),
    proplists:get_value("sources", Attrs, "").

%% ===================================================================
%% Errors log
%% ===================================================================

init_errors_log(#state{rundir=RunDir}=State) ->
    Path = guild_rundir:guild_file(RunDir, "errors.log"),
    error_logger:logfile({open, Path}),
    State.

%% ===================================================================
%% Init stream handlers
%% ===================================================================

init_stream_handlers(#state{op=#op{stream_handlers=HandlerInits}}=S) ->
    Handlers = [Init(self()) || Init <- HandlerInits],
    S#state{stream_handlers=Handlers}.

%% ===================================================================
%% Start exec
%% ===================================================================

start_exec(#state{cmd={Cmd, Env}}=State) ->
    WorkingDir = project_dir(State),
    Opts =
        [{env, Env},
         {cd, WorkingDir},
         stdout, stderr],
    {ok, Pid, OSPid} = guild_exec:run_link(Cmd, Opts),
    gproc:add_local_property(ospid, OSPid),
    State#state{exec_pid=Pid, exec_ospid=OSPid}.

project_dir(#state{op=#op{project=Project}}) ->
    guild_project:project_dir(Project).

%% ===================================================================
%% Tasks
%% ===================================================================

start_tasks(#state{op=#op{tasks=Tasks}}=State) ->
    lists:foreach(fun start_task/1, Tasks),
    State.

start_task(TaskSpec) ->
    {ok, _} = guild_optask_sup:start_task(TaskSpec, self()).

%% ===================================================================
%% Messages
%% ===================================================================

handle_msg({Stream, OSPid, Bin}, _From, #state{exec_ospid=OSPid}=State) ->
    handle_input(Stream, Bin, State);
handle_msg({'EXIT', Pid, Reason}, _From, #state{exec_pid=Pid}) ->
    handle_exec_exit(Reason);
handle_msg({stop, Timeout}, _From, State) ->
    handle_stop(Timeout, State).

handle_input(Stream, Bin, State) ->
    {Lines, Next} = stream_input(Stream, Bin, State),
    handle_stream_lines(Stream, Lines, State),
    {noreply, Next}.

stream_input(stdout, Bin, #state{stdout_buf=Buf}=S) ->
    {Lines, NextBuf} = guild_util:input(Buf, Bin),
    {Lines, S#state{stdout_buf=NextBuf}};
stream_input(stderr, Bin, #state{stderr_buf=Buf}=S) ->
    {Lines, NextBuf} = guild_util:input(Buf, Bin),
    {Lines, S#state{stderr_buf=NextBuf}}.

handle_stream_lines(Stream, Lines, #state{stream_handlers=Handlers}) ->
    dispatch_to_stream_handlers({Stream, Lines}, Handlers).

dispatch_to_stream_handlers(Msg, Handlers) ->
    lists:foreach(fun(H) -> call_stream_handler(H, Msg) end, Handlers).

call_stream_handler(F, Msg) when is_function(F) -> F(Msg);
call_stream_handler({M, F, A}, Msg) -> M:F([Msg|A]).

handle_exec_exit(Reason) ->
    {stop, Reason}.

handle_stop(Timeout, #state{exec_pid=Pid}=State) ->
    exec:stop_and_wait(Pid, Timeout),
    {stop, normal, ok, State}.
