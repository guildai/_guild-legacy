-module(hubc_app).

-export([init/0, init_support/1, start_child/1]).

-export([priv_dir/0, priv_dir/1, priv_bin/1, tmp_dir/0, set_env/2,
         get_env/1, get_env/2]).

-define(app, tensorhub_client).

-define(sup_child(M), {M, [{shutdown, infinity}]}).
-define(core_child(M, T), {M, [{shutdown, T}]}).

%% ===================================================================
%% Init app
%% ===================================================================

init() ->
    {ok, supervisors() ++ core_services()}.

supervisors() ->
    [?sup_child(hubc_operation_sup),
     ?sup_child(hubc_optask_sup),
     ?sup_child(hubc_http_sup),
     ?sup_child(hubc_project_view_sup),
     ?sup_child(hubc_runtime_sup),
     ?sup_child(hubc_data_reader_sup)].

core_services() ->
    [?core_child(hubc_proc,    100),
     ?core_child(hubc_runtime, 100),
     ?core_child(hubc_run_db,  1000)].

%% ===================================================================
%% Init support
%% ===================================================================

init_support(Multiple) when is_list(Multiple) ->
    lists:foreach(fun init_support/1, Multiple);
init_support(exec) ->
    validate_started(hubc_exec:init());
init_support(json) ->
    validate_started(hubc_json:init());
init_support({app_child, Child}) ->
    validate_started(start_child(Child)).

validate_started(ok) -> ok;
validate_started({ok, _}) -> ok;
validate_started({error, {already_started, _}}) -> ok;
validate_started(Other) -> error(Other).

%% ===================================================================
%% Start child (add to root supervsor)
%% ===================================================================

start_child(Child) ->
    e2_supervisor:start_child(hubc_app, Child).

%% ===================================================================
%% Misc app functions
%% ===================================================================

priv_dir() ->
    hubc_util:priv_dir(?app).

priv_dir(Subdir) ->
    filename:join(priv_dir(), Subdir).

priv_bin(Name) ->
    filename:join(priv_dir("bin"), Name).

tmp_dir() -> "/tmp".

set_env(Key, Val) ->
    application:set_env(?app, Key, Val).

get_env(Key) ->
    application:get_env(?app, Key).

get_env(Key, Default) ->
    case get_env(Key) of
        {ok, Val} -> Val;
        undefined -> Default
    end.
