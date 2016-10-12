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

-module(guild_project_view2).

-behavior(e2_supervisor).

-export([start_link/2, index_page_vars/2, compare_page_vars/1, json/2,
         stop/1]).

-export([init/1, handle_msg/3]).

-record(state, {p, pdir, runroots, opts}).

-define(max_series_epochs, 1000).

%% ===================================================================
%% New / init
%% ===================================================================

start_link(Project, Opts) ->
    e2_service:start_link(?MODULE, [Project, Opts]).

init([Project, Opts]) ->
    {ok, init_state(Project, Opts)}.

init_state(Project, Opts) ->
    #state{
       p=Project,
       pdir=guild_project:project_dir(Project),
       runroots=guild_project_util:all_runroots(Project),
       opts=Opts}.

%% ===================================================================
%% API
%% ===================================================================

index_page_vars(View, Opts) ->
    e2_service:call(View, {index_page_vars, Opts}).

compare_page_vars(View) ->
    e2_service:call(View, compare_page_vars).

json(View, Request) ->
    e2_service:call(View, {json, Request}).

stop(View) ->
    e2_service:call(View, stop).

%% ===================================================================
%% Message dispatch
%% ===================================================================

handle_msg({index_page_vars, Opts}, _From, State) ->
    handle_index_page_vars(Opts, State);
handle_msg(compare_page_vars, _From, State) ->
    handle_compare_page_vars(State);
handle_msg({json, Request}, From, State) ->
    handle_json_request(Request, From, State);
handle_msg(stop, _From, State) ->
    {stop, normal, ok, State}.

%% ===================================================================
%% Index page vars
%% ===================================================================

handle_index_page_vars(Opts, State) ->
    Vars =
        [{project_title, project_title(State)},
         {viewdef,       index_page_viewdef(Opts, State)},
         {view_opts,     view_opts(State)}],
    {reply, Vars, State}.

%% ===================================================================
%% Compare page vars
%% ===================================================================

handle_compare_page_vars(State) ->
    Vars =
        [{project_title, project_title(State)},
         {viewdef,       compare_page_viewdef(State)},
         {view_opts,     view_opts(State)}],
    {reply, Vars, State}.

%% ===================================================================
%% Project title
%% ===================================================================

project_title(State) ->
    find_val(
      [fun project_config_title/1,
       fun project_dir_title/1],
      State).

find_val(Funs, View) ->
    case guild_util:find_apply(Funs, [View]) of
        {ok, Val} -> Val;
        error -> undefined
    end.

project_config_title(#state{p=P}) ->
    guild_project:attr(P, ["project"], "name").

project_dir_title(#state{pdir=Dir}) ->
    {ok, dir_basename(Dir)}.

dir_basename(".") -> filename:basename(filename:absname(""));
dir_basename(Dir) -> filename:basename(filename:absname(Dir)).

%% ===================================================================
%% Index page viewdef
%% ===================================================================

index_page_viewdef(Opts, State) ->
    Runs = runs(State),
    Run = active_run(Runs, Opts),
    index_viewdef_for_run(Run, State).

index_viewdef_for_run(undefined, _State) ->
    empty_index_viewdef();
index_viewdef_for_run(Run, #state{p=Project}) ->
    {ok, Model} = run_model(Run, Project),
    case guild_project_util:view_path("view", Model, Project) of
        {ok, View} -> load_viewdef(View);
        error -> default_index_viewdef(Model, Project)
    end.

run_model(Run, Project) ->
    case guild_run:attr(Run, "model") of
        {ok, <<>>} -> default_model(Project);
        {ok, Name} -> named_model(Project, binary_to_list(Name));
        error -> default_model(Project)
    end.

default_model(Project) ->
    guild_project:section(Project, ["model"]).

named_model(Project, Name) ->
    guild_project:section(Project, ["model", Name]).

empty_index_viewdef() -> [].

default_index_viewdef(_, _) ->
    %% TODO: what do we show by default here?
    empty_index_viewdef().

load_viewdef(Path) ->
    {ok, Def} = file:consult(Path),
    Def.

%% ===================================================================
%% Compare page viewdef
%% ===================================================================

compare_page_viewdef(#state{p=Project}) ->
    case guild_project_util:view_path("compare", Project) of
        {ok, View} -> load_viewdef(View);
        error -> default_compare_viewdef(Project)
    end.

default_compare_viewdef(_Project) ->
    %% TODO: what do we show by default here?
    empty_compare_viewdef().

empty_compare_viewdef() -> [].

%% ===================================================================
%% View opts
%% ===================================================================

view_opts(#state{opts=Opts}) -> Opts.

%% ===================================================================
%% JSON request
%% ===================================================================

handle_json_request(Request, From, State) ->
    dispatch_reader(reader_request(Request, State), From, State).

reader_request(runs, State) ->
    {runs_json, runroots(State)};
reader_request({flags, Run}, State) ->
    {flags_json, resolve_run(Run, State)};
reader_request({summary, Run}, State) ->
    {summary_json, resolve_run(Run, State)};
reader_request({series, Pattern, Opts}, State) ->
    Run = resolve_run(run_opt(Opts), State),
    Max = max_epochs_opt(Opts),
    {series_json, Pattern, Run, Max};
reader_request({output, Run}, State) ->
    {output_json, resolve_run(Run, State)};
reader_request({compare, Sources}, State) ->
    {compare_json, Sources, runs(State), self()}.

dispatch_reader(Request, From, State) ->
    guild_data_reader_sup:start_reader(Request, reply_fun(From)),
    {noreply, State}.

reply_fun(Client) ->
    fun(Reply) -> e2_service:reply(Client, Reply) end.

resolve_run(latest, State) -> first_run(runs(State));
resolve_run(Id, State)     -> run_for_id(Id, runs(State)).

run_opt(Opts) ->
    proplists:get_value(run, Opts, latest).

max_epochs_opt(Opts) ->
    proplists:get_value(max_epochs, Opts, ?max_series_epochs).

%% ===================================================================
%% Run support
%% ===================================================================

runs(#state{runroots=RunRoots}) ->
    guild_run:runs_for_runroots(RunRoots).

active_run(Runs, Opts) ->
    case proplists:get_value(run, Opts, latest) of
        latest -> first_run(Runs);
        Id -> run_for_id(Id, Runs)
    end.

first_run([First|_]) -> First;
first_run([])        -> undefined.

run_for_id(Id, [Run|Rest]) ->
    maybe_run_for_id(guild_run:id(Run), Id, Run, Rest);
run_for_id(_Id, []) ->
    undefined.

maybe_run_for_id(Id, Id, Run, _Rest) ->
    Run;
maybe_run_for_id(_Other, Id, _Run, Rest) ->
    run_for_id(Id, Rest).

%% ===================================================================
%% State interface
%% ===================================================================

runroots(#state{runroots=Dirs}) -> Dirs.
