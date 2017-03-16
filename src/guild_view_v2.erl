-module(guild_view_v2).

-export([start_link/2, app_page_env/2, formatted_runs/1,
         resolve_run/2, settings/1]).

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
       pdir=guild_project:project_dir(Project),
       run_roots=guild_project_util:all_runroots(Project),
       settings=Settings
      }.

%% ===================================================================
%% API
%% ===================================================================

app_page_env(View, Run) ->
    e2_service:call(View, {fun app_page_env_/2, [Run]}).

formatted_runs(View) ->
    e2_service:call(View, {fun formatted_runs_/1, []}).

resolve_run(View, Id) ->
    e2_service:call(View, {fun resolve_run_/2, [Id]}).

settings(View) ->
    e2_service:call(View, {fun settings_/1, []}).

%% ===================================================================
%% Dispatch
%% ===================================================================

handle_msg({F, A}, _From, State) ->
    {reply, apply(F, A ++ [State]), State}.

%% ===================================================================
%% App page env
%% ===================================================================

app_page_env_(Run, State) ->
    Project = load_project(State),
    {ok, Model} = guild_project_util:run_model(Run, Project),
    #{
       viewdef => guild_view_v2_viewdef:viewdef(Model, Project),
       project => project_summary(Project),
       settings => settings_(State)
     }.

%% ===================================================================
%% Project summary
%% ===================================================================

project_summary(Project) ->
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
    Dir = guild_project:project_dir(P),
    {ok, dir_basename(Dir)}.

dir_basename(".") -> filename:basename(filename:absname(""));
dir_basename(Dir) -> filename:basename(filename:absname(Dir)).

project_description(P) ->
    case guild_project:attr(P, ["project"], "description") of
        {ok, Desc} -> Desc;
        error -> ""
    end.

%% ===================================================================
%% Formatted runs
%% ===================================================================

formatted_runs_(State) ->
    format_runs(runs(State)).

runs(#state{run_roots=RunRoots}) ->
    guild_run:runs_for_runroots(RunRoots).

format_runs(Runs) ->
    sort_formatted_runs([format_run(Run) || Run <- Runs]).

format_run(Run) ->
    {[
      {id, guild_run:id(Run)},
      {dir, list_to_binary(guild_run:dir(Run))},
      {status, guild_run_util:run_status(Run)}
      |format_run_attrs(guild_run:attrs(Run))
     ]}.

format_run_attrs(Attrs) ->
    [format_run_attr(Attr) || Attr <- Attrs].

format_run_attr({Name, Val}) ->
    {list_to_binary(Name), format_attr_val(Name, Val)}.

format_attr_val("started",     Bin) -> binary_to_integer(Bin);
format_attr_val("stopped",     Bin) -> binary_to_integer(Bin);
format_attr_val("exit_status", Bin) -> binary_to_integer(Bin);
format_attr_val(_Name,         Bin) -> Bin.

sort_formatted_runs(Runs) ->
    Cmp = fun(A, B) -> run_start_time(A) > run_start_time(B) end,
    lists:sort(Cmp, Runs).

run_start_time({Attrs}) ->
    case lists:keyfind(<<"started">>, 1, Attrs) of
        {_, Val} -> Val;
        false -> 0
    end.

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
%% Helpers
%% ===================================================================

load_project(#state{pdir=Dir}) ->
    {ok, Project} = guild_project:from_dir(Dir),
    Project.

settings_(#state{settings=S}) -> S.
