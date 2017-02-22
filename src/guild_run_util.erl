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

-module(guild_run_util).

-export([run_status/1, run_os_pid/1]).

run_status(Run) ->
    run_status_for_pid(run_os_pid(Run)).

run_os_pid(Run) ->
    Path = guild_rundir:guild_file(guild_run:dir(Run), "LOCK"),
    case file:read_file(Path) of
        {ok, Bin} -> try_bin_to_pid(Bin);
        {error, enoent} -> error
    end.

try_bin_to_pid(Bin) ->
    try binary_to_integer(Bin) of
        Pid -> {ok, Pid}
    catch
        _:badarg -> error
    end.

run_status_for_pid({ok, Pid}) ->
    case guild_util:os_pid_exists(Pid) of
        true -> running;
        false -> crashed
    end;
run_status_for_pid(error) ->
    stopped.
