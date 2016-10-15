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

-module(guild_project_view).

-behavior(e2_service).

-export([start_link/2, page_vars/2, json/2]).

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

page_vars(View, RunId) ->
    e2_service:call(View, {page_vars, RunId}).

json(View, Request) ->
    e2_service:call(View, {json, Request}).

%% ===================================================================
%% Message dispatch
%% ===================================================================

handle_msg({page_vars, RunId}, _From, State) ->
    handle_page_vars(RunId, State);
handle_msg({json, Request}, From, State) ->
    handle_json_request(Request, From, State).

%% ===================================================================
%% Page vars
%% ===================================================================

handle_page_vars(RunId, #state{opts=Opts}=State) ->
    Run = resolve_run(RunId, State),
    Vars =
        [{project_title, project_title(State)},
         {active_run,    run_attrs(Run)},
         {viewdef,       viewdef(Run, State)},
         {view_opts,     Opts}],
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
%% Run attrs
%% ===================================================================

run_attrs(undefined) -> undefined;
run_attrs(Run)       -> [{id, guild_run:id(Run)}].

%% ===================================================================
%% Viewdef
%% ===================================================================

viewdef(undefined, _State) -> undefined;
viewdef(Run, #state{p=Project}) ->
    {ok, Model} = run_model(Run, Project),
    case find_viewdef(Model, Project) of
        {ok, Viewdef} -> Viewdef;
        error -> undefined
    end.

find_viewdef(Model, Project) ->
    guild_util:find_apply(
      [fun custom_viewdef/2,
       fun generated_viewdef/2],
      [Model, Project]).

custom_viewdef(Model, Project) ->
    case guild_viewdef:viewdef_path(Model, Project) of
        {ok, Path} -> {ok, load_viewdef(Path)};
        error      -> error
    end.

run_model(Run, Project) ->
    case guild_run:attr(Run, "model") of
        {ok, <<>>} -> default_model(Project);
        {ok, Name} -> named_model(Project, binary_to_list(Name));
        error      -> default_model(Project)
    end.

default_model(Project) ->
    guild_project:section(Project, ["model"]).

named_model(Project, Name) ->
    guild_project:section(Project, ["model", Name]).

load_viewdef(Path) ->
    case file:consult(Path) of
        {ok, Def} -> Def;
        {error, Err} ->
            log_viewdef_error(Err, Path),
            undefined
    end.

log_viewdef_error(enoent, File) ->
    guild_log:warn("~s: file does not exist~n", [File]);
log_viewdef_error({Line, erl_parse, Msg}, File) ->
    guild_log:warn("~s:~b: ~s~n", [File, Line, Msg]).

generated_viewdef(Model, Project) ->
    case guild_viewdef:viewdef_section(Model, Project) of
        {ok, View} ->
            guild_viewdef:generate_viewdef(View, Project);
        error ->
            error
    end.

%% ===================================================================
%% JSON request
%% ===================================================================

handle_json_request(Request, From, State) ->
    dispatch_reader(reader_request(Request, State), From, State).

reader_request(runs, #state{runroots=RunRoots}) ->
    {runs_json, RunRoots};
reader_request({flags, RunId}, State) ->
    {flags_json, resolve_run(RunId, State)};
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

run_opt(Opts) ->
    proplists:get_value(run, Opts, latest).

max_epochs_opt(Opts) ->
    proplists:get_value(max_epochs, Opts, ?max_series_epochs).

%% ===================================================================
%% Utils
%% ===================================================================

resolve_run(latest, State) -> first_run(runs(State));
resolve_run(Id,     State) -> run_for_id(Id, runs(State)).

runs(#state{runroots=RunRoots}) ->
    guild_run:runs_for_runroots(RunRoots).

first_run([First|_]) -> First;
first_run([])        -> undefined.

run_for_id(Id, [Run|Rest]) ->
    run_for_id(guild_run:id(Run), Id, Run, Rest);
run_for_id(_Id, []) ->
    undefined.

run_for_id(Id, Id, Run, _)    -> Run;
run_for_id(_,  Id, _,   Rest) -> run_for_id(Id, Rest).
