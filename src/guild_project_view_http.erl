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

-module(guild_project_view_http).

-export([start_server/3, stop_server/0]).

-export([init/1, handle_msg/3]).

-export([handle_view_page/2]).

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
       {{starts_with, "/data/"},   data_handler(View)},
       {"/bye",                    bye_handler()},
       {'_',                       view_page_handler(View)}
      ]).

static_handler() ->
    psycho_static:create_app(guild_app:priv_dir()).

data_handler(View) ->
    psycho_util:dispatch_app(
      {guild_project_view_data_http, app},
      [parsed_path, View]).

bye_handler() ->
    fun(_Env) -> handle_bye() end.

handle_bye() ->
    timer:apply_after(500, ?MODULE, stop_server, []),
    Page = guild_dtl:render(guild_bye_page, []),
    guild_http:ok_html(Page).

view_page_handler(View) ->
    psycho_util:dispatch_app({?MODULE, handle_view_page}, [parsed_path, View]).

middleware(Opts) ->
    maybe_apply_log_middleware(Opts, []).

maybe_apply_log_middleware(Opts, Acc) ->
    case proplists:get_bool(log, Opts) of
        true -> [log_middleware()|Acc];
        false -> Acc
    end.

log_middleware() ->
    fun(Upstream) -> guild_log_http:create_app(Upstream) end.

%% ===================================================================
%% View page handler
%% ===================================================================

handle_view_page({Path, _, Params}, ProjectView) ->
    RunId = selected_run(Params),
    ViewVars = guild_project_view:page_vars(ProjectView, RunId),
    handle_view_page_(page_view(Path, ViewVars), Params, ViewVars).

page_view(Path, ViewVars) ->
    Result =
        guild_util:find_apply2(
          [fun() -> check_active_run(ViewVars) end,
           fun() -> check_viewdef(ViewVars) end,
           fun() -> {stop, find_page_view(Path, ViewVars)} end], []),
    Result.

check_active_run(Vars) ->
    case proplists:get_value(active_run, Vars) of
        undefined -> {stop, {error, no_run}};
        _         -> continue
    end.

check_viewdef(Vars) ->
    case proplists:get_value(viewdef, Vars) of
        undefined -> {stop, {error, no_viewdef}};
        _         -> continue
    end.

find_page_view(Path, ViewVars) ->
    Viewdef = proplists:get_value(viewdef, ViewVars, []),
    Views = [{Name, Attrs} || {view, Name, Attrs} <- Viewdef],
    find_page_view_(Path, Views).

find_page_view_("/",    [First|_])       -> {ok, First};
find_page_view_("/"++X, [{X, _}=View|_]) -> {ok, View};
find_page_view_(Path,   [_|Rest])        -> find_page_view_(Path, Rest);
find_page_view_(_Path,  [])              -> {error, path_not_found}.

selected_run(Params) ->
    case psycho_util:validate(Params, [{"run", [integer]}]) of
        {ok, Validated} -> proplists:get_value("run", Validated, latest);
        {error, Err} -> bad_request(psycho_util:format_validate_error(Err))
    end.

bad_request(Msg) ->
    throw(guild_http:bad_request(Msg)).

handle_view_page_({ok, PageView}, Params, ViewVars) ->
    PageVars = view_page_vars(PageView, Params, ViewVars),
    Page = guild_dtl:render(guild_project_view_page, PageVars),
    guild_http:ok_html(Page);
handle_view_page_({error, path_not_found}, _Params, _ViewVars) ->
    guild_http:not_found();
handle_view_page_({error, Error}, Params, ViewVars) ->
    PageVars = [{error, Error}|view_page_vars(Params, ViewVars)],
    Page = guild_dtl:render(guild_project_error_page, PageVars),
    guild_http:ok_html(Page).

view_page_vars(Params, ViewVars) ->
    view_page_vars(undefined, Params, ViewVars).

view_page_vars(ActiveView, Params, ViewVars) ->
    apply_view_render_context(
      [{html_title,  html_title_for_view(ViewVars)},
       {nav_title,   "Guild View"},
       {active_view, ActiveView},
       {params,      Params}
       |ViewVars]).

html_title_for_view(ViewVars) ->
    case proplists:get_value(project_title, ViewVars) of
        undefined -> "Guild";
        ProjectTitle -> ProjectTitle ++ " - Guild"
    end.

apply_view_render_context(Vars) ->
    [{view_render_context, view_render_context(Vars)}|Vars].

view_render_context(Vars) ->
    [{project_title, proplists:get_value(project_title, Vars)}].
