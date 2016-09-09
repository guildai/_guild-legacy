-module(hubc_project_view_sup).

-behavior(e2_supervisor).

-export([start_link/0, start_view/2, stop_view/1]).

start_link() ->
    e2_supervisor:start_link(
      ?MODULE,
      [{hubc_project_view, [temporary]}],
      [simple_one_for_one, registered]).

start_view(Project, Opts) ->
    e2_supervisor:start_child(?MODULE, [Project, Opts]).

stop_view(View) ->
    hubc_project_view:stop(View).
