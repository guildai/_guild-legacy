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

-export([start_server/2, stop_server/0]).

-export([handle_page/2]).

-export([init/1, handle_msg/3]).

%% ===================================================================
%% Start / stop server
%% ===================================================================

start_server(Port, Opts) ->
    guild_dtl:init(Opts),
    App = create_app(Opts),
    ServerOpts = [{proc_callback, {?MODULE, []}}],
    guild_http_sup:start_server(Port, App, ServerOpts).

init([]) ->
    erlang:register(?MODULE, self()).

stop_server() ->
    proc:cast(?MODULE, stop).

handle_msg(stop, _From, _App) ->
    {stop, normal}.

%% ===================================================================
%% Main app / routes
%% ===================================================================

create_app(Opts) ->
    psycho_util:chain_apps(routes(), middleware(Opts)).

routes() ->
    psycho_route:create_app(
      [{{starts_with, "/assets/"}, static_handler()},
       {"/bye",                    bye_handler()},
       {'_',                       page_handler()}
      ]).

static_handler() ->
    psycho_static:create_app(guild_app:priv_dir()).

bye_handler() ->
    fun(_Env) -> handle_bye() end.

handle_bye() ->
    timer:apply_after(500, ?MODULE, stop_server, []),
    guild_http:ok_text("Stopping server\n").

page_handler() ->
    psycho_util:dispatch_app(
      {?MODULE, handle_page},
      [method, parsed_path]).

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
%% Page handler
%% ===================================================================

handle_page("GET", {"/", _, Params}) ->
    handle_index(Params);
handle_page(_, _) ->
    guild_http:bad_request().

handle_index(_Params) ->
    Vars =
        [{html_title, "Depot - Guild"},
         {nav_title, "Guild Depot"}],
    Page = guild_dtl:render(guild_depot_index_page, Vars),
    guild_http:ok_html(Page).
