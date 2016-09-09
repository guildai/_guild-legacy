-module(hubc_http).

-export([start_link/3]).

-export([ok_html/1,
         ok_html/2,
         ok_text/1,
         ok_json/1,
         error_html/1,
         redirect/1,
         redirect/2,
         not_found/0,
         not_found/1,
         bad_request/0,
         bad_request/1,
         internal_error/0,
         internal_error/1]).

%% ===================================================================
%% Start
%% ===================================================================

start_link(Port, App, Opts) ->
    init_mime_types(),
    psycho_server:start_link(Port, App, Opts).

init_mime_types() ->
    Types = psycho_mime:load_types(mime_types_source()),
    psycho_mime:init(Types).

mime_types_source() ->
    filename:join(hubc_util:priv_dir(psycho), "mime.types").

%% ===================================================================
%% Response helpers
%% ===================================================================

ok_html(Page) ->
    ok_html(Page, []).

ok_html(Page, ExtraHeaders) ->
    {{200, "OK"}, [{"Content-Type", "text/html"}|ExtraHeaders], Page}.

ok_text(Page) ->
    {{200, "OK"}, [{"Content-Type", "text/plain"}], Page}.

ok_json(JSON) ->
    {{200, "OK"}, [{"Content-Type", "application/json"}], JSON}.

error_html(Page) ->
    {{500, "Internal Error"}, [{"Content-Type", "text/html"}], Page}.

redirect(Location) ->
    redirect(Location, []).

redirect(Location, Headers) ->
    {{302, "See Other"}, [{"Location", Location}|Headers]}.

not_found() ->
    {{404, "Not Found"}, [{"Content-Type", "text/plain"}], "Not Found"}.

not_found(Page) ->
    {{404, "Not Found"}, [{"Content-Type", "text/html"}], Page}.

bad_request() ->
    bad_request("Bad Request").

bad_request(Msg) ->
    {{400, "Bad Request"}, [{"Content-Type", "text/plain"}], Msg}.

internal_error() ->
    internal_error("Internal Error").

internal_error(Msg) ->
    {{500, "Internal Error"}, [{"Content-Type", "text/plain"}], Msg}.
