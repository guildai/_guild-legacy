-module(guild_view_v2).

-export([start_link/1, project_summary/1, run_roots/1]).

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

project_summary(View) ->
    e2_service:call(View, {fun project_summary_/1, []}).

run_roots(View) ->
    e2_service:call(View, {fun run_roots_/1, []}).

%% ===================================================================
%% Dispatch
%% ===================================================================

handle_msg({F, A}, _From, State) ->
    {reply, apply(F, A ++ [State]), State}.

%% ===================================================================
%% Project summary
%% ===================================================================

project_summary_(#state{pdir=Dir}) ->
    {ok, Project} = guild_project:from_dir(Dir),
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
%% Static attrs (i.e. not reloaded on each call)
%% ===================================================================

run_roots_(#state{run_roots=RunRoots}) -> RunRoots.
