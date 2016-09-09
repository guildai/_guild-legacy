-module(hubc_project_view_http).

-export([start_server/3, stop_server/0]).

-export([init/1, handle_msg/3]).

%% ===================================================================
%% Start / stop server
%% ===================================================================

start_server(View, Port, Opts) ->
    hubc_template:init(Opts),
    App = create_app(View, Opts),
    ServerOpts = [{proc_callback, {?MODULE, [View]}}],
    hubc_http_sup:start_server(Port, App, ServerOpts).

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
      [{{starts_with, "/assets/"}, static_app()},
       {{starts_with, "/data/"},   data_app(View)},
       {"/",                       view_page(fun index_page/2, View)},
       {"/compare",                view_page(fun compare_page/2, View)},
       {"/bye",                    view_page(fun exit_and_bye_page/2, View)}
      ]).

static_app() ->
    psycho_static:create_app(hubc_app:priv_dir()).

data_app(View) ->
    psycho_util:dispatch_app(
      {hubc_project_view_data_http, app},
      [parsed_path, View]).

view_page(Fun, View) ->
    psycho_util:dispatch_app(Fun, [View, parsed_query_string]).

middleware(Opts) ->
    maybe_apply_log_middleware(Opts, []).

maybe_apply_log_middleware(Opts, Acc) ->
    case proplists:get_bool(log, Opts) of
        true -> [log_middleware()|Acc];
        false -> Acc
    end.

log_middleware() ->
    fun(Upstream) -> hubc_log_http:create_app(Upstream) end.

%% ===================================================================
%% Index page
%% ===================================================================

index_page(View, Params) ->
    handle_index_page_for_params(validate_index_page_params(Params), View).

validate_index_page_params(Params) ->
    psycho_util:validate(Params, [{"run", [integer]}]).

handle_index_page_for_params({ok, Params}, View) ->
    Vars = index_page_vars(View, Params),
    Page = hubc_template:render(hubc_project_index_page, Vars),
    hubc_http:ok_html(Page);
handle_index_page_for_params({error, Err}, _View) ->
    hubc_http:bad_request(psycho_util:format_validate_error(Err)).

index_page_vars(View, Params) ->
    Run = proplists:get_value("run", Params, latest),
    ViewVars = hubc_project_view:index_page_vars(View, [{run, Run}]),
    apply_base_page_vars(ViewVars, Params).

html_title(Vars) ->
    case proplists:get_value(project_title, Vars) of
        undefined -> "TensorHub";
        Title -> Title ++ " - TensorHub"
    end.

%% ===================================================================
%% Compare page
%% ===================================================================

compare_page(View, Params) ->
    Vars = compare_page_vars(View, Params),
    Page = hubc_template:render(hubc_project_compare_page, Vars),
    hubc_http:ok_html(Page).

compare_page_vars(View, Params) ->
    ViewVars = hubc_project_view:compare_page_vars(View),
    apply_base_page_vars(ViewVars, Params).

%% ===================================================================
%% Base page vars
%% ===================================================================

apply_base_page_vars(ViewVars, Params) ->
    [{html_title, html_title(ViewVars)},
     {nav_title,  "TensorHub View"},
     {params,     Params}
     |ViewVars].

%% ===================================================================
%% Bye
%% ===================================================================

exit_and_bye_page(_View, _Env) ->
    timer:apply_after(500, ?MODULE, stop_server, []),
    Page = hubc_template:render(hubc_bye_page, []),
    hubc_http:ok_html(Page).
