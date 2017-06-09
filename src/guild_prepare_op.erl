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

-module(guild_prepare_op).

-behavior(guild_op).

-export([from_spec/3, cmd_info/1]).

-export([init/1, handle_task/1, handle_msg/3]).

-record(op, {section, project, cmd, tasks, stream_handlers}).

-record(state, {op, cmd, exec_pid, exec_ospid, stream_handlers,
                stdout_buf, stderr_buf}).

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
        tasks=[],
        stream_handlers=stream_handlers()}}.

static_env() ->
    [{"PKGHOME", guild_app:pkg_dir()},
     {"GPU_COUNT", gpu_count_env()}].

gpu_count_env() ->
    integer_to_list(length(guild_sys:gpu_attrs())).

stream_handlers() ->
    guild_op_support:op_stream_handlers([console]).

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
     fun init_cmd/1,
     fun init_stream_handlers/1,
     fun start_exec/1,
     fun start_tasks/1].

init_app_support(State) ->
    guild_app:init_support([exec, json]),
    State.

%% ===================================================================
%% Cmd
%% ===================================================================

init_cmd(#state{op=#op{cmd={Args, Env}}}=State) ->
    ResolvedArgs = guild_util:resolve_args(Args, Env),
    State#state{cmd={ResolvedArgs, Env}}.

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
    guild_project:dir(Project).

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
