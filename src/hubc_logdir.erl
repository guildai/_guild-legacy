-module(hubc_logdir).

-export([log_root_for_project/1]).

log_root_for_project(Project) ->
    case hubc_project:attr(Project, ["project"], "log_root") of
        {ok, Val} -> project_relative_path(Project, Val);
        error -> system_log_root()
    end.

project_relative_path(Project, Val) ->
    filename:abspath(Val, hubc_project:project_dir(Project)).

system_log_root() ->
    error('TODO').
