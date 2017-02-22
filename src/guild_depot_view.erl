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

-module(guild_depot_view).

-behavior(e2_service).

-export([start_link/1, depot_config/2, depot_config/3, projects/1,
         projects/2, project/2, apply_projects_extra/2,
         apply_project_extra/2, account/2]).

-export([init/1, handle_msg/3]).

-record(state, {d, config}).

%% ===================================================================
%% New / init
%% ===================================================================

start_link(Depot) ->
    e2_service:start_link(?MODULE, [Depot]).

init([Depot]) ->
    {ok, init_state(Depot)}.

init_state(Depot) ->
    #state{
       d=Depot,
       config=load_depot_config(Depot)}.

load_depot_config(Depot) ->
    %% Using Guild project structure for Depot config
    case guild_project:from_dir(Depot) of
        {ok, Config} -> Config;
        {error, _} -> []
    end.

%% ===================================================================
%% API
%% ===================================================================

depot_config(View, Attr) ->
    e2_service:call(View, {fun config_/2, [Attr]}).

depot_config(View, Attr, Default) ->
    guild_util:maybe_default(depot_config(View, Attr), Default).

projects(View) ->
    e2_service:call(View, {fun projects_/1, []}).

projects(View, Filter) ->
    e2_service:call(View, {fun projects_/2, [Filter]}).

project(View, Path) ->
    e2_service:call(View, {fun project_/2, [Path]}).

account(View, Name) ->
    e2_service:call(View, {fun account_/2, [Name]}).

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
project_extra_fun(updated) -> fun apply_project_updated/1;
project_extra_fun(tags)    -> fun apply_project_tags/1;
project_extra_fun(F) when is_function(F)
                           -> F.

apply_project_stars(#{path:=Path}=P) ->
    {ok, Stars} = guild_depot_db:project_stars(Path),
    P#{stars => Stars,
       stars_count => length(Stars)}.

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
     filename:join(Dir, "README.md"),
     filename:join(Dir, "runs/*")].

apply_project_tags(#{guild_p:=GuildP}=P) ->
    P#{tags => project_tags(GuildP)}.

project_tags(GuildP) ->
    case guild_project:attr(GuildP, ["project"], "tags")of
        {ok, Tags} -> split_tags(Tags);
        error -> []
    end.

split_tags(Tags) ->
    re:split(Tags, "\\s+", [{return, list}, trim]).

%% ===================================================================
%% Message dispatch
%% ===================================================================

handle_msg({Fun, Args}, _From, State) when is_function(Fun) ->
    Resp = erlang:apply(Fun, Args ++ [State]),
    {reply, Resp, State}.

%% ===================================================================
%% Config
%% ===================================================================

config_(Attr, #state{config=Config}) ->
    guild_project:attr(Config, ["depot"], Attr).

%% ===================================================================
%% Projects
%% ===================================================================

projects_(#state{d=Depot}) ->
    Accounts = guild_depot:accounts(Depot),
    lists:foldl(fun account_projects_acc/2, [], Accounts).

account_projects_acc(Account, Acc) ->
    Projects = guild_depot:account_projects(Account),
    lists:foldl(fun(P, AccIn) -> [P|AccIn] end, Acc, Projects).

projects_(Paths, #state{d=Depot}) ->
    [guild_depot:project_for_path(Depot, Path) || Path <- Paths].

%% ===================================================================
%% Project
%% ===================================================================

project_(Path, #state{d=Depot}) ->
    guild_depot:project_for_path(Depot, Path).

%% ===================================================================
%% Account
%% ===================================================================

account_(Name, #state{d=Depot}) ->
    Accounts = guild_depot:accounts(Depot),
    case find_account(Name, Accounts) of
        {ok, Account} ->
            {ok, apply_account_projects(Account)};
        error ->
            error
    end.

find_account(Name, [#{name:=Name}=A|_]) -> {ok, A};
find_account(Name, [_|Rest]) -> find_account(Name, Rest);
find_account(_Name, []) -> error.

apply_account_projects(A) ->
    A#{projects => guild_depot:account_projects(A)}.
