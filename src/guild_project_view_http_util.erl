-module(guild_project_view_http_util).

-export([run_opt/1, resolve_project_run/2, ensure_tensorflow_port/0]).

run_opt(Params) ->
    Schema = [{"run", [integer]}],
    case guild_http:validate_params(Params, Schema) of
        [{_, Run}] -> Run;
        []         -> latest
    end.

resolve_project_run(RunId, View) ->
    %% See implementation note in guild_project_view for more
    %% information about this interface.
    case guild_project_view:project_run(View, RunId) of
        {_, undefined} -> no_such_run_error(RunId);
        {Project, Run} -> {Project, Run}
    end.

no_such_run_error(RunId) ->
    throw(guild_http:bad_request(io_lib:format("no such run ~s", [RunId]))).

ensure_tensorflow_port() ->
    %% TEMP bootstrapping of tensorflow port support - when this
    %% stabilizes we need to move long running runtime-specific
    %% services into the supervisory tree, lazily initialized.
    guild_app:init_support(exec),
    case guild_app:start_child(guild_tensorflow_port) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.
