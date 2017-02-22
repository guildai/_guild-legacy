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

-module(guild_runtime).

-behavior(e2_service).

-export([start_link/0, for_section/2, init_prepare_op/3,
         init_train_op/3, init_eval_op/4, init_serve_op/4]).

-export([init/1, handle_msg/3]).

-record(state, {cache}).

%% ===================================================================
%% Start / init
%% ===================================================================

start_link() ->
    e2_service:start_link(?MODULE, [], [registered]).

init([]) ->
    {ok, #state{cache=[]}}.

%% ===================================================================
%% API
%% ===================================================================

for_section(Section, Project) ->
    e2_service:call(?MODULE, {rt, Section, Project}).

init_prepare_op(Runtime, Section, Project) ->
    call_runtime(Runtime, {init_prepare_op, Section, Project}).

init_train_op(Runtime, Section, Project) ->
    call_runtime(Runtime, {init_train_op, Section, Project}).

init_eval_op(Runtime, Run, Section, Project) ->
    call_runtime(Runtime, {init_eval_op, Run, Section, Project}).

init_serve_op(Runtime, Run, Section, Project) ->
    call_runtime(Runtime, {init_serve_op, Run, Section, Project}).

call_runtime(Runtime, Msg) -> e2_service:call(Runtime, Msg).

%% ===================================================================
%% Dispatch
%% ===================================================================

handle_msg({rt, Section, Project}, _From, State) ->
    {Result, Next} = runtime_for_section(Section, Project, State),
    {reply, Result, Next};
handle_msg({'DOWN', _Ref, process, Runtime, _Info}, noreply, State) ->
    {noreply, uncache_runtime(Runtime, State)}.

%% ===================================================================
%% Runtime for section
%% ===================================================================

runtime_for_section(Section, Project, State) ->
    maybe_runtime(runtime_mod_for_op(Section, Project), State).

runtime_mod_for_op(Section, Project) ->
    runtime_mod_for_attr(find_runtime_attr(Section, Project), Section).

find_runtime_attr(Section, Project) ->
    guild_util:find_apply(
      [fun() -> guild_project:section_attr(Section, "runtime") end,
       fun() -> guild_project:attr(Project, ["project"], "runtime") end],
      []).

runtime_mod_for_attr({ok, Val}, _Section) ->
    runtime_mod_for_name(Val);
runtime_mod_for_attr(error, {[Type], _}) ->
    {error, {no_runtime, Type, ""}};
runtime_mod_for_attr(error, {[Type, Name|_], _}) ->
    {error, {no_runtime, Type, Name}}.

runtime_mod_for_name("tensorflow")  -> {ok, guild_tensorflow_runtime};
runtime_mod_for_name("tflearn")     -> {ok, guild_tflearn_runtime};
runtime_mod_for_name(Name)          -> {error, {unknown_runtime, Name}}.

maybe_runtime({ok, Mod}, State) ->
    maybe_init_runtime(try_cache(Mod, State), Mod, State);
maybe_runtime({error, Err}, State) ->
    {{error, Err}, State}.

maybe_init_runtime({ok, Cached}, _Mod, State) ->
    {{ok, Cached}, State};
maybe_init_runtime(error, Mod, State) ->
    {ok, Pid} = guild_runtime_sup:start_child(Mod),
    monitor(process, Pid),
    {{ok, Pid}, cache_runtime(Mod, Pid, State)}.

%% ===================================================================
%% Runtime cache
%% ===================================================================

try_cache(Mod, #state{cache=C}) ->
    case lists:keyfind(Mod, 1, C) of
        {_, Runtime} -> {ok, Runtime};
        false        -> error
    end.

cache_runtime(Mod, Runtime, #state{cache=C}=S) ->
    S#state{cache=lists:keystore(Mod, 1, C, {Mod, Runtime})}.

uncache_runtime(Runtime, #state{cache=C}=S) ->
    S#state{cache=lists:keydelete(Runtime, 2, C)}.
