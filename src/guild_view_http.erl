%% Copyright 2016-2017 TensorHub, Inc.
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

-module(guild_view_http).

-export([start_server/3, stop_server/0, run_for_params/2]).

-export([handle_app_page/1]).

-export([init/1, handle_msg/3]).

%% ===================================================================
%% Start / stop server
%% ===================================================================

start_server(View, Port, Opts) ->
    guild_dtl:init([recompile_templates]),
    App = create_app(View, Opts),
    ServerOpts = [{proc_callback, {?MODULE, [View]}}],
    guild_http_sup:start_server(Port, App, ServerOpts).

init([_View]) ->
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
      [{{starts_with, "/assets/"},     static_handler()},
       {{starts_with, "/components/"}, components_handler()},
       {{starts_with, "/data/"},       data_handler(View)},
       {"/bye",                        bye_handler()},
       {'_',                           app_page_handler()}]).

static_handler() ->
    psycho_static:create_app(guild_app:priv_dir()).

components_handler() ->
    psycho_static:create_app(guild_app:priv_dir()).

data_handler(View) ->
    guild_view_data_http:create_app(View).

bye_handler() ->
    fun(_Env) -> handle_bye() end.

handle_bye() ->
    timer:apply_after(500, ?MODULE, stop_server, []),
    Page = guild_dtl:render(guild_bye_page, []),
    guild_http:ok_html(Page).

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
%% App page handler
%% ===================================================================

app_page_handler() ->
    psycho_util:dispatch_app({?MODULE, handle_app_page}, [parsed_path]).

handle_app_page({"/", QS, _}) ->
    handle_index_page(QS);
handle_app_page(_) ->
    handle_app_page().

handle_index_page(QS) ->
    guild_http:redirect(viewdef_page1_path(QS)).

viewdef_page1_path(QS) ->
    ["/overview", maybe_qs(QS)].

maybe_qs("") -> "";
maybe_qs(QS) -> ["?", QS].

handle_app_page() ->
    Page = guild_dtl:render(guild_view_index_page, []),
    guild_http:ok_html(Page).

%% ===================================================================
%% Helpers
%% ===================================================================

run_for_params(Params, View) ->
    Id = run_id_for_params(Params),
    case guild_view:resolve_run(View, Id) of
        undefined -> throw(guild_http:not_found());
        Run -> Run
    end.

run_id_for_params(Params) ->
    Schema = [{"run", [integer]}],
    case guild_http:validate_params(Params, Schema) of
        [{_, Run}] -> Run;
        []         -> latest
    end.
