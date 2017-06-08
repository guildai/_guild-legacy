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
%%
%% guild_op
%%
%% Interface to operations. Operations are implemented by behavior
%% modules (typically ending in '_op' - e.g. guild_train_op,
%% guild_eval_op, etc.)
%%
%% Op implementations should provide a function that creates a static
%% op definition consisting of a {Module, OpState} definition. This
%% should be used in static information calls (see below). Callbacks
%% that apply to static state will be made as
%% Module:Function(OpState).
%%
%% There are two types of callback patterns for this behavior:
%%
%% - Calls that provide static information (i.e. information always
%% available for an op, regardless of whether it's running)
%%
%% - Calls that provide runtime information (i.e. information that's
%% only available after the operation is started)
%%
%% Runtime information calls are implemented using gproc process
%% values. Refer to uses of gproc:get_value/2 below for supported
%% values.

-module(guild_op).

-export([start_link/2, stop/2, cmd_info/1, cwd/1, ospid/1]).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{cmd_info, 1},
     {handle_task, 1}].

start_link(Name, {Mod, State}) ->
    e2_task:start_link(Mod, State, [{registered, Name}]).

stop(OpPid, Timeout) ->
    e2_task:stop(OpPid, Timeout).

cmd_info({Module, State}) ->
    Module:cmd_info(State).

cwd(Op) ->
    %% cwd -> string() | undefined
    gproc:get_value({p, l, cwd}, Op).

ospid(Op) ->
    %% ospid -> integer() | undefined
    gproc:get_value({p, l, ospid}, Op).
