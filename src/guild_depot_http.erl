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

handle_page("GET", {"/", _, Params}, View, Env) ->
    handle_index(View, Params, Env);
handle_page("GET", {Path, _, Params}, View, Env) ->
    handle_path(parse_path(Path), Params, View, Env);
handle_page(_Method, _Path, _View, _Env) ->
    guild_http:bad_request().

handle_path({ok, {project, Path}}, Params, View, Env) ->
    handle_project_path(Path, Params, View, Env);
handle_path({ok, {account, Name}}, Params, View, Env) ->
    handle_account_path(Name, Params, View, Env);
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

handle_index(View, Params, Env) ->
    Projects = filter_projects(all_projects(View), Params),
    Tags = apply_tag_selected(tags(Projects), Params),
    Vars =
        [{html_title, "Guild Depot"},
         {nav_title, "Guild Depot"},
         {narrow_page, true},
         {active_page, "depot-index"},
         {projects, Projects},
         {tags, Tags},
         {now_seconds, now_seconds()},
         {env, Env},
         {params, Params}],
    Page = guild_dtl:render(guild_depot_index_page, Vars),
    guild_http:ok_html(Page).

all_projects(View) ->
    Projects = guild_depot_view:projects(View),
    Extra =
        [stars,
         updated,
         tags],
    guild_depot_view:apply_projects_extra(Projects, Extra).

filter_projects(Projects, Params) ->
    Opts =
        [{text, proplists:get_value("q", Params)},
         {tags, proplists:get_all_values("t", Params)}],
    guild_depot_util:filter_projects(Projects, Opts).

tags(Projects) ->
    TagCounts = guild_depot_util:count_tags(Projects),
    [#{name => Tag,
       count => Count,
       color => guild_dtl_lib:tag_color(Tag),
       id => guild_dtl_lib:tag_id(Tag)}
     || {Tag, Count} <- TagCounts].

apply_tag_selected(Tags, Params) ->
    Selected = proplists:get_all_values("t", Params),
    [T#{selected => tag_selected(T, Selected)} || T <- Tags].

tag_selected(#{name:=Name}, Selected) -> lists:member(Name, Selected).

%% ===================================================================
%% Project
%% ===================================================================

handle_project_path(Path, Params, View, Env) ->
    handle_project(project_by_path(View, Path), Params, Env).

project_by_path(View, Path) ->
    case guild_depot_view:project(View, Path) of
        {ok, P} -> {ok, P};
        {error, Err} -> {error, Path, Err}
    end.

handle_project({ok, P}, Params, Env) ->
    case project_action(Params) of
        "view"   -> view_project(P, Params, Env);
        "star"   -> star_project(P, Env);
        "unstar" -> unstar_project(P, Env)
    end;
handle_project({error, Path, Err}, _Params, _Env) ->
    guild_log:internal("Error loading project from ~p: ~p~n", [Path, Err]),
    guild_http:not_found().

project_action(Params) ->
    find_param(["view", "star", "unstar"], Params).

view_project(P0, Params, Env) ->
    P = apply_project_extra(P0),
    Vars =
        [{html_title, project_page_title(P)},
         {nav_title, "Guild Depot"},
         {narrow_page, true},
         {active_page, "depot-project"},
         {p, P},
         {active_tab, active_project_tab(Params)},
         {now_seconds, now_seconds()},
         {env, Env}],
    Page = guild_dtl:render(guild_depot_project_page, Vars),
    guild_http:ok_html(Page).

apply_project_extra(P) ->
    Extras =
        [stars,
         runs,
         updated,
         tags],
    guild_depot_view:apply_project_extra(P, Extras).

project_page_title(#{account:=#{name:=Account}, name:=Project}) ->
    [Account, " / ", Project, " - Guild Depot"].

active_project_tab(Params) ->
    find_param(["readme", "guild"], Params).

star_project(#{path:=Path}, Env) ->
    guild_http:ok_text(
      io_lib:format(
        "TODO: star ~s using ~p",
        [Path, proplists:get_value(user, Env)])).

unstar_project(#{path:=Path}, Env) ->
    guild_http:ok_text(
      io_lib:format(
        "TODO: unstar ~s using ~p",
        [Path, proplists:get_value(user, Env)])).

%% ===================================================================
%% Account
%% ===================================================================

handle_account_path(Path, Params, View, Env) ->
    handle_account(guild_depot_view:account(View, Path), Params, Env).

handle_account({ok, A0}, Params, Env) ->
    A = apply_account_project_extras(A0),
    Vars =
        [{html_title, account_page_title(A)},
         {nav_title, "Guild Depot"},
         {narrow_page, true},
         {a, A},
         {active_tab, active_account_tab(Params)},
         {now_seconds, now_seconds()},
         {env, Env}],
    Page = guild_dtl:render(guild_depot_account_index_page, Vars),
    guild_http:ok_html(Page);
handle_account(error, _Params, _Env) ->
    guild_http:not_found().

account_page_title(#{name:=Account}) ->
    [Account, " - Guild Depot"].

apply_account_project_extras(#{projects:=P0}=A) ->
    Extra =
        [stars,
         updated,
         tags],
    P = guild_depot_view:apply_projects_extra(P0, Extra),
    A#{projects:=P}.

active_account_tab(Params) ->
    find_param(["projects", "stars"], Params).

%% ===================================================================
%% Helpers
%% ===================================================================

find_param([First|_]=Targets, Params) ->
    find_param(Targets, Params, First).

find_param([Name|Rest], Params, Default) ->
    case proplists:is_defined(Name, Params) of
        true -> Name;
        false -> find_param(Rest, Params, Default)
    end;
find_param([], _Params, Default) -> Default.

now_seconds() ->
    erlang:system_time() div 1000000000.
