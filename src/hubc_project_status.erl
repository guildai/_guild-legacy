-module(hubc_project_status).

-export([status/2]).

status(_Project, _Part) ->
    io:format("TODO: Got a project and a part, so what now?~n"),
    "unknown".
