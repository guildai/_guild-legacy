-module(hubc_project_view).

-behavior(e2_supervisor).

-export([start_link/2, index_page_vars/2, compare_page_vars/1, json/2,
         stop/1]).

-export([init/1, handle_msg/3]).

-record(state, {p, pdir, logroots, opts}).

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
       pdir=hubc_project:project_dir(Project),
       logroots=hubc_project_util:all_logroots(Project),
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
    case hubc_util:find_apply(Funs, [View]) of
        {ok, Val} -> Val;
        error -> undefined
    end.

project_config_title(#state{p=P}) ->
    hubc_project:attr(P, ["project"], "name").

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

index_viewdef_for_run(Run, #state{p=Project}) ->
    Model = run_model(Run, Project),
    {ok, ViewDef} =
        hubc_util:find_apply(
          [fun model_index_viewdef/2,
           fun project_index_viewdef/2,
           fun global_index_viewdef/2],
          [Model, Project]),
    ViewDef.

run_model(undefined, _Project) ->
    error;
run_model(Run, Project) ->
    case hubc_run:attr(Run, "model") of
        {ok, <<>>} -> default_model(Project);
        {ok, Name} -> named_model(Project, binary_to_list(Name));
        error -> default_model(Project)
    end.

default_model(Project) ->
    hubc_project:section(Project, ["model"]).

named_model(Project, Name) ->
    hubc_project:section(Project, ["model", Name]).

model_index_viewdef({ok, Model}, Project) ->
    maybe_viewdef_for_attr(
      hubc_project:section_attr(Model, "view"),
      Project);
model_index_viewdef(error, _Project) ->
    error.

project_index_viewdef(_, Project) ->
    maybe_viewdef_for_attr(project_view_attr(Project), Project).

project_view_attr(Project) ->
    case hubc_project:attr(Project, ["project"], "view") of
        {ok, Val} -> {ok, Val};
        error -> {ok, "view"}
    end.

global_index_viewdef(_, _) ->
    %% TODO: what do we show by default here?
    {ok, []}.

maybe_viewdef_for_attr({ok, Val}, Project) ->
    viewdef_for_path(Project, Val);
maybe_viewdef_for_attr(error, _Project) ->
    error.

viewdef_for_path(Project, Path) ->
    ConfigPath = viewdef_config_for_project_path(Project, Path),
    case file:consult(ConfigPath) of
        {ok, Def} -> {ok, Def};
        {error, enoent} -> error
    end.

viewdef_config_for_project_path(Project, Path) ->
    filename:join(hubc_project:project_dir(Project), Path ++ ".config").

%% ===================================================================
%% Compare page viewdef
%% ===================================================================

compare_page_viewdef(#state{p=Project}) ->
    {ok, ViewDef} =
        hubc_util:find_apply(
          [fun project_compare_viewdef/1,
           fun global_compare_viewdef/1],
          [Project]),
    ViewDef.

project_compare_viewdef(Project) ->
    maybe_viewdef_for_attr(project_compare_attr(Project), Project).

project_compare_attr(Project) ->
    case hubc_project:attr(Project, ["project"], "compare") of
        {ok, Val} -> {ok, Val};
        error -> {ok, "compare"}
    end.

global_compare_viewdef(_) ->
    %% TODO: what do we show by default here?
    {ok, []}.

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
    {runs_json, logsroot(State)};
reader_request({flags, Run}, State) ->
    {flags_json, resolve_run(Run, State)};
reader_request({summary, Run}, State) ->
    {summary_json, resolve_run(Run, State)};
reader_request({series, Pattern, Opts}, State) ->
    Run = resolve_run(run_opt(Opts), State),
    {series_json, Pattern, Run, ?max_series_epochs};
reader_request({output, Run}, State) ->
    {output_json, resolve_run(Run, State)};
reader_request({compare, Sources}, State) ->
    {compare_json, Sources, runs(State), self()}.

dispatch_reader(Request, From, State) ->
    hubc_data_reader_sup:start_reader(Request, reply_fun(From)),
    {noreply, State}.

reply_fun(Client) ->
    fun(Reply) -> e2_service:reply(Client, Reply) end.

resolve_run(latest, State) -> first_run(runs(State));
resolve_run(Id, State)     -> run_for_id(Id, runs(State)).

run_opt(Opts) ->
    proplists:get_value(run, Opts, latest).

%% ===================================================================
%% Run support
%% ===================================================================

runs(#state{logroots=LogRoots}) ->
    hubc_run:runs_for_logroots(LogRoots).

active_run(Runs, Opts) ->
    case proplists:get_value(run, Opts, latest) of
        latest -> first_run(Runs);
        Id -> run_for_id(Id, Runs)
    end.

first_run([First|_]) -> First;
first_run([])        -> undefined.

run_for_id(Id, [Run|Rest]) ->
    maybe_run_for_id(hubc_run:id(Run), Id, Run, Rest);
run_for_id(_Id, []) ->
    undefined.

maybe_run_for_id(Id, Id, Run, _Rest) ->
    Run;
maybe_run_for_id(_Other, Id, _Run, Rest) ->
    run_for_id(Id, Rest).

%% ===================================================================
%% State interface
%% ===================================================================

logsroot(#state{logroots=Dir}) -> Dir.
