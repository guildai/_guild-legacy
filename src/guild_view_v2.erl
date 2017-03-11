-module(guild_view_v2).

-export([start_link/1, app_page_env/1, runs/1]).

-export([handle_msg/3]).

-behavior(e2_service).

-record(state, {pdir, run_roots}).

-define(bin(X), iolist_to_binary(X)).

%% ===================================================================
%% Start / init
%% ===================================================================

start_link(Project) ->
    e2_service:start_link(?MODULE, init_state(Project)).

init_state(Project) ->
    #state{
       pdir=guild_project:project_dir(Project),
       run_roots=guild_project_util:all_runroots(Project)
      }.

%% ===================================================================
%% API
%% ===================================================================

app_page_env(View) ->
    e2_service:call(View, {fun app_page_env_/1, []}).

runs(View) ->
    e2_service:call(View, {fun runs_/1, []}).

%% ===================================================================
%% Dispatch
%% ===================================================================

handle_msg({F, A}, _From, State) ->
    {reply, apply(F, A ++ [State]), State}.

%% ===================================================================
%% App page env
%% ===================================================================

app_page_env_(State) ->
    Project = load_project(State),
    #{
       viewdef => guild_view_v2_viewdef:viewdef(Project),
       project => project_summary(Project)
     }.

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
%% Runs
%% ===================================================================

runs_(#state{run_roots=RunRoots}) ->
    Runs = guild_run:runs_for_runroots(RunRoots),
    format_runs(Runs).

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
%% Helpers
%% ===================================================================

load_project(#state{pdir=Dir}) ->
    {ok, Project} = guild_project:from_dir(Dir),
    Project.
