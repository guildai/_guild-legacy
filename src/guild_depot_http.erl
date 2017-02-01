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

-module(guild_depot_http).

-export([start_server/3, stop_server/0]).

-export([handle_page/4]).

-export([init/1, handle_msg/3]).

%% ===================================================================
%% Start / stop server
%% ===================================================================

start_server(View, Port, Opts) ->
    guild_dtl:init(Opts),
    App = create_app(View, Opts),
    ServerOpts = [{proc_callback, {?MODULE, [View]}}],
    guild_http_sup:start_server(Port, App, ServerOpts).

init([View]) ->
    erlang:monitor(process, View),
    erlang:register(?MODULE, self()).

stop_server() ->
    proc:cast(?MODULE, stop).

handle_msg(stop, _From, _App) ->
    {stop, normal}.

%% ===================================================================
%% Main app / routes
%% ===================================================================

create_app(View, Opts) ->
    psycho_util:chain_apps(routes(View), middleware(Opts)).

routes(View) ->
    psycho_route:create_app(
      [{{starts_with, "/assets/"}, static_handler()},
       {"/login",                  login_handler(View)},
       {"/logout",                 logout_handler()},
       {"/oauthcb",                oauth_callback_handler(View)},
       {"/bye",                    bye_handler()},
       {'_',                       page_handler(View)}
      ]).

static_handler() ->
    psycho_static:create_app(guild_app:priv_dir()).

login_handler(View) ->
    guild_depot_auth_http:login_handler(View).

logout_handler() ->
    guild_depot_auth_http:logout_handler().

oauth_callback_handler(View) ->
    guild_depot_auth_http:oauth_callback_handler(View).

bye_handler() ->
    fun(_Env) -> handle_bye() end.

handle_bye() ->
    timer:apply_after(500, ?MODULE, stop_server, []),
    guild_http:ok_text("Stopping server\n").

page_handler(View) ->
    psycho_util:chain_apps(page_app(View), [user_middleware(View)]).

page_app(View) ->
    psycho_util:dispatch_app(
      {?MODULE, handle_page},
      [method, parsed_path, View, env]).

user_middleware(View) ->
    fun(Upstream) -> guild_depot_auth_http:user_middleware(View, Upstream) end.

middleware(Opts) ->
    lists:foldl(fun middleware_acc/2, [], proplists:compact(Opts)).

middleware_acc(log, Acc) ->
    [log_middleware()|Acc];
middleware_acc(_, Acc) ->
    Acc.

log_middleware() ->
    fun(Upstream) -> guild_log_http:create_app(Upstream) end.

%% ===================================================================
%% Page handler
%% ===================================================================

handle_page("GET", {"/", _, _}, View, Env) ->
    handle_index(View, Env);
handle_page("GET", {Path, _, Params}, View, Env) ->
    handle_path(parse_path(Path), Params, View, Env);
handle_page(_Method, _Path, _View, _Env) ->
    guild_http:bad_request().

handle_path({ok, {project, Path}}, Params, View, Env) ->
    handle_project_path(Path, Params, View, Env);
handle_path({ok, {account, Name}}, _Params, _View, _Env) ->
    guild_http:ok_text("TODO: account page for " ++ Name);
handle_path(error, _Params, _View, _Env) ->
    guild_http:bad_request().

parse_path(Path) ->
    guild_util:find_apply(
      [fun try_project_path/1,
       fun try_account_path/1],
      [Path]).

-define(project_path_part_pattern, "[\\w\\d-_]+").

try_project_path(Path) ->
    Pattern =
        "^/(" ?project_path_part_pattern "/"
        ?project_path_part_pattern ")$",
    case re:run(Path, Pattern, [{capture, [1], list}]) of
        {match, [ProjectPath]} -> {ok, {project, ProjectPath}};
        nomatch -> error
    end.

try_account_path(Path) ->
    Pattern = "^/(" ?project_path_part_pattern ")$",
    case re:run(Path, Pattern, [{capture, [1], list}]) of
        {match, [AccountPath]} -> {ok, {account, AccountPath}};
        nomatch -> error
    end.

%% ===================================================================
%% Index
%% ===================================================================

handle_index(View, Env) ->
    Vars =
        [{html_title, "Guild Depot"},
         {nav_title, "Guild Depot"},
         {narrow_page, true},
         {active_page, "depot-index"},
         {projects, index_projects(View)},
         {filter_tags, fake_data:filter_tags()},
         {now_seconds, now_seconds()},
         {env, Env}],
    Page = guild_dtl:render(guild_depot_index_page, Vars),
    guild_http:ok_html(Page).

index_projects(View) ->
    Projects = guild_depot_view:projects(View),
    Extra =
        [stars,
         updated,
         tags],
    guild_depot_view:apply_projects_extra(Projects, Extra).

%% ===================================================================
%% Project
%% ===================================================================

handle_project_path(Path, Params, View, Env) ->
    handle_project(project_by_path(View, Path), Params, Env).

project_by_path(View, Path) ->
    case guild_depot_view:project_by_path(View, Path) of
        {ok, P} -> {ok, apply_project_extra(P)};
        {error, Err} -> {error, Path, Err}
    end.

apply_project_extra(P) ->
    Extras =
        [stars,
         runs,
         updated,
         tags],
    guild_depot_view:apply_project_extra(P, Extras).

handle_project({ok, P}, Params, Env) ->
    Vars =
        [{html_title, project_title(P)},
         {nav_title, "Guild Depot"},
         {narrow_page, true},
         {active_page, "depot-project"},
         {p, P},
         {active_file, active_file(Params)},
         {now_seconds, now_seconds()},
         {env, Env}],
    Page = guild_dtl:render(guild_depot_project_page, Vars),
    guild_http:ok_html(Page);
handle_project({error, Path, Err}, _Params, _Env) ->
    guild_log:internal("Error loading project from ~p: ~p~n", [Path, Err]),
    guild_http:not_found().

project_title(#{account:=#{name:=Account}, name:=Project}) ->
    io_lib:format("~s / ~s - Guild Depot", [Account, Project]).

active_file(Params) ->
    Defined =
        [proplists:is_defined("readme", Params),
         proplists:is_defined("guild", Params)],
    case Defined of
        [_, true] -> "guild";
        [_, _] -> "readme"
    end.

now_seconds() ->
    erlang:system_time() div 1000000000.
