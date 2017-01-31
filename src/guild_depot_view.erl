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

-module(guild_depot_view).

-behavior(e2_service).

-export([start_link/1, projects/1, project_by_path/2,
         apply_projects_extra/2, apply_project_extra/2]).

-export([init/1, handle_msg/3]).

-record(state, {d}).

%% ===================================================================
%% New / init
%% ===================================================================

start_link(Depot) ->
    e2_service:start_link(?MODULE, [Depot]).

init([Depot]) ->
    {ok, init_state(Depot)}.

init_state(Depot) ->
    #state{d=Depot}.

%% ===================================================================
%% API
%% ===================================================================

projects(View) ->
    e2_service:call(View, projects).

project_by_path(View, Path) ->
    e2_service:call(View, {project_by_path, Path}).

%% ===================================================================
%% Message dispatch
%% ===================================================================

handle_msg(projects, _From, State) ->
    handle_projects(State);
handle_msg({project_by_path, Path}, _From, State) ->
    handle_project_by_path(Path, State).

%% ===================================================================
%% Projects
%% ===================================================================

handle_projects(State) ->
    {reply, projects_(State), State}.

projects_(#state{d=Depot}) ->
    Accounts = guild_depot:accounts(Depot),
    lists:foldl(fun account_projects_acc/2, [], Accounts).

account_projects_acc(Account, Acc) ->
    Projects = guild_depot:account_projects(Account),
    lists:foldl(fun(P, AccIn) -> [P|AccIn] end, Acc, Projects).

%% ===================================================================
%% Project by path
%% ===================================================================

handle_project_by_path(Path, State) ->
    {reply, project_by_path_(Path, State), State}.

project_by_path_(Path, #state{d=Depot}) ->
    guild_depot:project_by_path(Depot, Path).

%% ===================================================================
%% Project extra
%% ===================================================================

apply_projects_extra(Ps, Extra) ->
    [apply_project_extra(P, Extra) || P <- Ps].

apply_project_extra(P, Extra) ->
    guild_util:fold_apply(project_extra_funs(Extra), P).

project_extra_funs(Extra) ->
    [project_extra_fun(X) || X <- Extra].

project_extra_fun(stars)   -> fun apply_project_stars/1;
project_extra_fun(runs)    -> fun apply_project_runs/1;
project_extra_fun(updated) -> fun apply_project_updated/1.

apply_project_stars(#{path:=Path}=P) ->
    {ok, N} = guild_depot_db:project_stars(Path),
    P#{stars => N}.

apply_project_runs(#{guild_p:=GuildP}=P) ->
    Runs = guild_run:runs_for_project(GuildP),
    P#{runs => format_runs(Runs)}.

format_runs(Runs) ->
    Runs.

apply_project_updated(#{guild_p:=GuildP}=P) ->
    Patterns = project_file_patterns_for_updated(GuildP),
    Updated = guild_util:latest_mtime(Patterns),
    P#{updated => Updated}.

project_file_patterns_for_updated(GuildP) ->
    Dir = guild_project:project_dir(GuildP),
    [filename:join(Dir, "Guild"),
     filename:join(Dir, "runs/*")].
