%% Copyright 2106 TensorHub, Inc.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(guild_depot_db).

-behavior(e2_service).

-export([start_link/0, open/1, open/2, star_project/2,
         unstar_project/2, project_stars/1, close/0]).

-export([init/1, handle_msg/3, terminate/2]).

-record(state, {depot, db}).

-define(depot_db_name, "depot.db").

%% ===================================================================
%% Start / init
%% ===================================================================

start_link() ->
    e2_service:start_link(?MODULE, [], [registered]).

init([]) ->
    {ok, #state{}}.

%% ===================================================================
%% API
%% ===================================================================

open(Depot) ->
    open(Depot, []).

open(Depot, Options) ->
    e2_service:call(?MODULE, {open, Depot, Options}).

star_project(Project, User) ->
    e2_service:call(?MODULE, {op, {star_project, Project, User}}).

unstar_project(Project, User) ->
    e2_service:call(?MODULE, {op, {unstar_project, Project, User}}).

project_stars(Project) ->
    e2_service:call(?MODULE, {op, {project_stars, Project}}).

close() ->
    e2_service:call(?MODULE, close).

%% ===================================================================
%% Dispatch
%% ===================================================================

handle_msg({open, Depot, Options}, _From, State) ->
    handle_open(Depot, Options, State);
handle_msg(close, _From, State) ->
    handle_close(State);
handle_msg({op, Op}, _From, State) ->
    handle_db_op(Op, State);
handle_msg({'EXIT', MaybeDb, normal}, noreply, State) ->
    handle_db_exit(MaybeDb, State).

handle_db_op(Op, #state{db=Db}=State) ->
    {reply, db_op(Db, Op), State}.

%% ===================================================================
% Open
%% ===================================================================

handle_open(Depot, Options, State) ->
    {Reply, Next} = ensure_opened(Depot, Options, State),
    {reply, Reply, Next}.

ensure_opened(Depot, Options, State) ->
    case is_db_open(Depot, State) of
        true ->
            {ok, State};
        false ->
            try_close_db(State),
            try_open_db(Depot, Options, State)
    end.

is_db_open(Depot, #state{depot=Depot}) -> true;
is_db_open(_Depot, _State) -> false.

try_open_db(Depot, Opts, State) ->
    Path = db_path(Depot),
    Exists = filelib:is_file(Path),
    Create = proplists:get_bool(create_if_missing, Opts),
    maybe_open_db(Exists, Create, Path, Depot, State).

db_path(Depot) ->
    filename:join(Depot, ?depot_db_name).

maybe_open_db(_Exists=true, _Create, Path, Depot, State) ->
    open_db(Path, [], Depot, State);
maybe_open_db(_Exists=false, _Create=true, Path, Depot, State) ->
    open_db(Path, [init_schema], Depot, State);
maybe_open_db(_Exsts=false, _Create=false, _Path, _Depot, State) ->
    {{error, missing}, State}.

open_db(Path, Opts, Depot, State) ->
    ok = filelib:ensure_dir(Path),
    Db = sqlite3_open_db(Path),
    maybe_init_schema(proplists:get_bool(init_schema, Opts), Db),
    {ok, add_db(Depot, Db, State)}.

sqlite3_open_db(File) ->
    {ok, Db} = sqlite3:open(anonymous, [{file, File}]),
    Db.

maybe_init_schema(true, Db) ->
    SQL =
        "create table if not exists project_star ("
        "    project text,"
        "    user text,"
        "    primary key (project, user));",
    [ok] = sqlite3:sql_exec_script_timeout(Db, SQL, infinity);
maybe_init_schema(false, _Db) ->
    ok.

add_db(Depot, Db, S) ->
    S#state{depot=Depot, db=Db}.

%% ===================================================================
%% DB ops
%% ===================================================================

db_op(Db, {star_project, P, User})   -> star_project_(Db, P, User);
db_op(Db, {unstar_project, P, User}) -> unstar_project_(Db, P, User);
db_op(Db, {project_stars, P})        -> project_stars_(Db, P).

%% -------------------------------------------------------------------
%% Star project
%% -------------------------------------------------------------------

star_project_(Db, P, User) ->
    SQL = "insert or ignore into project_star values (?, ?)",
    Args = [iolist_to_binary(P), iolist_to_binary(User)],
    guild_sql:exec_insert(Db, SQL, Args).

%% -------------------------------------------------------------------
%% Unstar project
%% -------------------------------------------------------------------

unstar_project_(Db, P, User) ->
    SQL = "delete from project_star where project = ? and user = ?",
    Args = [iolist_to_binary(P), iolist_to_binary(User)],
    guild_sql:exec_delete(Db, SQL, Args).

%% -------------------------------------------------------------------
%% Project stars
%% -------------------------------------------------------------------

project_stars_(Db, P) ->
    SQL = "select count(*) from project_star where project = ?",
    Args = [P],
    case guild_sql:exec_select(Db, SQL, Args) of
        {ok, {_, [{Count}]}} -> {ok, Count};
        {error, Error} -> {error, Error}
    end.

%% ===================================================================
%% Close
%% ===================================================================

handle_close(State) ->
    Next = try_close_db(State),
    {reply, ok, Next}.

try_close_db(#state{db=undefined}=State) ->
    State;
try_close_db(#state{db=Db}=State) ->
    close_db(Db),
    remove_db(State).

close_db(Db) ->
    ok = sqlite3:close(Db).

remove_db(S) ->
    S#state{depot=undefined, db=undefined}.

%% ===================================================================
%% Db exit
%% ===================================================================

handle_db_exit(Db, #state{db=Db}=State) ->
    {noreply, remove_db(State)};
handle_db_exit(_Db, State) ->
    {noreply, State}.

%% ===================================================================
%% Shutdown
%% ===================================================================

terminate(_Reason, State) ->
    try_close_db(State).
