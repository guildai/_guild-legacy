-module(guild_project_view_http_util).

-export([run_opt/1, resolve_run/2]).

run_opt(Params) ->
    Schema = [{"run", [integer]}],
    case guild_http:validate_params(Params, Schema) of
        [{_, Run}] -> Run;
        []         -> latest
    end.

resolve_run(RunId, View) ->
    case guild_project_view:project_run(View, RunId) of
        undefined -> no_such_run_error(RunId);
        Run       -> Run
    end.

no_such_run_error(RunId) ->
    throw(guild_http:bad_request(io_lib:format("no such run ~s", [RunId]))).
