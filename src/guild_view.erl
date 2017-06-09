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

-module(guild_view).

-export([start_link/2, project/1, formatted_runs/1, resolve_run/2,
         all_runs/1, settings/1, viewdef/2]).

-export([handle_msg/3]).

-behavior(e2_service).

-record(state, {pdir, run_roots, settings}).

-define(bin(X), iolist_to_binary(X)).

%% ===================================================================
%% Start / init
%% ===================================================================

start_link(Project, Settings) ->
    e2_service:start_link(?MODULE, init_state(Project, Settings)).

init_state(Project, Settings) ->
    #state{
       pdir=guild_project:dir(Project),
       run_roots=guild_project_util:all_runroots(Project),
       settings=Settings
      }.

%% ===================================================================
%% API
%% ===================================================================

project(View) ->
    e2_service:call(View, {fun project_/1, []}).

formatted_runs(View) ->
    e2_service:call(View, {fun formatted_runs_/1, []}).

resolve_run(View, Id) ->
    e2_service:call(View, {fun resolve_run_/2, [Id]}).

all_runs(View) ->
    e2_service:call(View, {fun all_runs_/1, []}).

settings(View) ->
    e2_service:call(View, {fun settings_/1, []}).

viewdef(View, Run) ->
    e2_service:call(View, {fun viewdef_/2, [Run]}).

%% ===================================================================
%% Dispatch
%% ===================================================================

handle_msg({F, A}, _From, State) ->
    {reply, apply(F, A ++ [State]), State}.

%% ===================================================================
%% Project summary
%% ===================================================================

project_(State) ->
    format_project(apply_viewdef_include(load_project(State))).

load_project(#state{pdir=Dir}) ->
    {ok, Project} = guild_project:from_dir(Dir),
    Project.

apply_viewdef_include(P) ->
    guild_project:apply_include(P, viewdef_project_include()).

viewdef_project_include() ->
    {ok, Include} = guild_project:from_file(guild_include_path()),
    Include.

guild_include_path() ->
    filename:join(guild_app:priv_dir("viewdef"), "GuildInclude").

format_project(P) ->
    [formatted_meta_section(P)|format_project_sections(P)].

formatted_meta_section(P) ->
    [[<<"__meta__">>], project_attrs(P)].

project_attrs(Project) ->
    #{
       title => ?bin(project_title(Project)),
       description => ?bin(project_description(Project))
     }.

project_title(P) ->
    Methods =
        [fun project_title_from_config/1,
         fun project_title_from_dir/1],
    guild_util:find_apply(Methods, [P], "").

project_title_from_config(P) ->
    guild_project:attr(P, ["project"], "name").

project_title_from_dir(P) ->
    Dir = guild_project:dir(P),
    {ok, dir_basename(Dir)}.

dir_basename(".") -> filename:basename(filename:absname(""));
dir_basename(Dir) -> filename:basename(filename:absname(Dir)).

project_description(P) ->
    case guild_project:attr(P, ["project"], "description") of
        {ok, Desc} -> Desc;
        error -> ""
    end.

format_project_sections(P) ->
    Sections = guild_project:sections(P, []),
    [format_project_section(S) || S <- Sections].

format_project_section({Path, Attrs}) ->
    PathBin = [list_to_binary(Part) || Part <- Path],
    AttrsBin = [{list_to_binary(K), list_to_binary(V)} || {K, V} <- Attrs],
    [PathBin, maps:from_list(AttrsBin)].

%% ===================================================================
%% Formatted runs
%% ===================================================================

formatted_runs_(State) ->
    format_runs(runs(State)).

runs(#state{run_roots=RunRoots}) ->
    guild_run:runs_for_runroots(RunRoots).

format_runs(Runs) ->
    sort_formatted_runs([guild_run_util:format_run(Run) || Run <- Runs]).

sort_formatted_runs(Runs) ->
    Cmp = fun(A, B) -> run_start_time(A) > run_start_time(B) end,
    lists:sort(Cmp, Runs).

run_start_time(#{<<"started">>:=Started}) -> Started;
run_start_time(#{}) -> 0.

%% ===================================================================
%% Resolve run
%% ===================================================================

resolve_run_(latest, State) -> first_run(runs(State));
resolve_run_(Id,     State) -> run_for_id(Id, runs(State)).

first_run([First|_]) -> First;
first_run([])        -> undefined.

run_for_id(Id, [Run|Rest]) ->
    run_for_id(guild_run:id(Run), Id, Run, Rest);
run_for_id(_Id, []) ->
    undefined.

run_for_id(Id, Id, Run, _)    -> Run;
run_for_id(_,  Id, _,   Rest) -> run_for_id(Id, Rest).

%% ===================================================================
%% All runs
%% ===================================================================

all_runs_(#state{run_roots=RunRoots}) ->
    guild_run:runs_for_runroots(RunRoots).

%% ===================================================================
%% Viewdef
%% ===================================================================

viewdef_(Run, State) ->
    Project = load_project(State),
    {ok, Model} = guild_project_util:run_model(Run, Project),
    guild_view_viewdef:viewdef(Model, Project).

%% ===================================================================
%% Settings
%% ===================================================================

settings_(#state{settings=S}) -> S.
