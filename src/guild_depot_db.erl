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

-export([start_link/1, open/1, star_project/2, unstar_project/2,
         project_stars/1, close/0]).

-export([init/1, handle_msg/3, terminate/2]).

-record(state, {depot, db}).

-define(depot_db_name, "depot.db").

%% ===================================================================
%% Start / init
%% ===================================================================

start_link(Depot) ->
    e2_service:start_link(?MODULE, [Depot], [registered]).

init([Depot]) ->
    {ok, #state{}, {handle_msg, {open, Depot}}}.

open(Depot) ->
    MFA = {?MODULE, start_link, [Depot]},
    Opts = [{restart, transient}],
    guild_db_sup:start_child({MFA, Opts}).

%% ===================================================================
%% API
%% ===================================================================

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

handle_msg({open, Depot}, noreply, State) ->
    handle_open(Depot, State);
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

handle_open(Depot, State) ->
    Next = ensure_opened(Depot, State),
    {noreply, Next}.

ensure_opened(Depot, State) ->
    case is_db_open(Depot, State) of
        true -> State;
        false ->
            try_close_db(State),
            open_db(Depot, State)
    end.

is_db_open(Depot, #state{depot=Depot}) -> true;
is_db_open(_Depot, _State) -> false.

open_db(Depot, State) ->
    Path = db_path(Depot),
    Exists = filelib:is_file(Path),
    ok = filelib:ensure_dir(Path),
    Db = sqlite3_open_db(Path),
    maybe_init_schema(not Exists, Db),
    add_db(Depot, Db, State).

db_path(Depot) ->
    filename:join(Depot, ?depot_db_name).

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
    SQL = "select user from project_star where project = ?",
    Args = [P],
    case guild_sql:exec_select(Db, SQL, Args) of
        {ok, {_, Users}} -> {ok, [U || {U} <- Users]};
        {error, Error} -> {error, Error}
    end.

%% ===================================================================
%% Close
%% ===================================================================

handle_close(State) ->
    Next = try_close_db(State),
    {stop, normal, ok, Next}.

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
