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

-module(guild_list_runs_cmd).

-export([parser/0, main/2]).

-define(true_filter, fun(_) -> true end).

%% ===================================================================
%% Parser
%% ===================================================================

parser() ->
    cli:parser(
      "guild list-runs",
      "[OPTION]...",
      "List project runs.",
      list_runs_opts() ++ guild_cmd_support:project_options(),
      [{pos_args, 0}]).

list_runs_opts() ->
    [{completed, "--completed",
      "show only completed runs", [flag]},
     {with_export, "--with-export",
      "show only runs an exported model", [flag]}].

%% ===================================================================
%% Main
%% ===================================================================

main(Opts, []) ->
    Project = guild_cmd_support:project_from_opts(Opts),
    print_runs(guild_run:runs_for_project(Project), Opts).

print_runs(Runs, Opts) ->
    Filtered = lists:filter(run_filter(Opts), Runs),
    lists:foreach(fun print_run/1, Filtered).

run_filter(Opts) ->
    Filters = [status_filter(Opts), exports_filter(Opts)],
    fun(Run) -> apply_filters(Run, Filters) end.

status_filter(Opts) ->
    case proplists:get_bool(completed, Opts) of
        true -> fun(Run) -> is_completed(Run) end;
        false -> ?true_filter
    end.

is_completed(Run) ->
    case guild_run:attr(Run, "exit_status") of
        {ok, <<"0">>} -> true;
        {ok, _}       -> false;
        error         -> false
    end.

exports_filter(Opts) ->
    case proplists:get_bool(with_export, Opts) of
        true -> fun(Run) -> has_export(Run) end;
        false -> ?true_filter
    end.

has_export(Run) ->
    Export = filename:join(guild_run:dir(Run), "model/export.meta"),
    filelib:is_file(Export).

apply_filters(Run, [F|Rest]) ->
    case F(Run) of
        true -> apply_filters(Run, Rest);
        false -> false
    end;
apply_filters(_Run, []) ->
    true.

print_run(R) ->
    Dir = guild_run:dir(R),
    guild_cli:closeable_out("~s~n", [Dir]).
