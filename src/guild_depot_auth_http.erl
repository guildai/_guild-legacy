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

-module(guild_depot_auth_http).

-export([login_handler/1, logout_handler/0, oauth_callback_handler/1,
         user_middleware/2]).

-export([handle_login/3, handle_logout/1, handle_oauth_callback/3,
         apply_user/3]).

-record(auth_config, {github_client_id, github_client_secret,
                      oauth_encryption_key}).

-define(github_oauth_authorize_url,
        "https://github.com/login/oauth/authorize").

-define(github_oauth_access_token_url,
        "https://github.com/login/oauth/access_token").

-define(github_user_url, "https://api.github.com/user").

%% ===================================================================
%% Apps
%% ===================================================================

login_handler(View) ->
    Config = init_auth_config(View),
    psycho_util:dispatch_app(
      {?MODULE, handle_login},
      [parsed_query_string, parsed_cookie, Config]).

logout_handler() -> {?MODULE, handle_logout}.

oauth_callback_handler(View) ->
    Config = init_auth_config(View),
    psycho_util:dispatch_app(
      {?MODULE, handle_oauth_callback},
      [parsed_query_string, parsed_cookie, Config]).

user_middleware(View, Upstream) ->
    Config = init_auth_config(View),
    psycho_util:dispatch_app(
      {?MODULE, apply_user},
      [env, Upstream, Config]).

%% ===================================================================
%% Auth config
%% ===================================================================

init_auth_config(View) ->
    Config = fun(Name, Default) -> config(View, Name, Default) end,
    #auth_config{
       github_client_id=Config("github_client_id", ""),
       github_client_secret=Config("github_client_secret", ""),
       oauth_encryption_key=list_to_binary(Config("oauth_encryption_key", ""))}.

config(View, Name, Default) ->
    guild_depot_view:depot_config(View, Name, Default).

github_client_id(#auth_config{github_client_id=Id}) -> Id.

github_client_secret(#auth_config{github_client_secret=S}) -> S.

oauth_encryption_key(#auth_config{oauth_encryption_key=Key}) -> Key.

%% ===================================================================
%% Login handler
%% ===================================================================

handle_login(Params, Cookie, Config) ->
    ClientId = current_or_new_client_id(Cookie),
    OAuthLogin = github_oauth_login_url(ClientId, Config),
    guild_http:redirect(
      OAuthLogin,
      [client_id_cookie(ClientId), next_url_cookie(Params)]).

current_or_new_client_id(Cookie) ->
    case current_client_id(Cookie) of
        undefined -> new_client_id();
        ClientId -> ClientId
    end.

current_client_id(Cookie) ->
    proplists:get_value("client_id", Cookie).

new_client_id() ->
    base64:encode(crypto:strong_rand_bytes(32)).

github_oauth_login_url(UserClientId, Config) ->
    Params =
        [{"client_id", github_client_id(Config)},
         {"state",     client_oauth_state(UserClientId, Config)}],
    psycho_util:encode_url(?github_oauth_authorize_url, Params).

client_oauth_state(ClientId, Config) ->
    Key = oauth_encryption_key(Config),
    base64:encode(crypto:hmac(sha, Key, ClientId)).

client_id_cookie(ClientId) ->
    psycho_util:cookie_header("client_id", ClientId, [{path, "/"}]).

next_url_cookie(Params) ->
    Next = encoded_next_url_from_params(Params),
    psycho_util:cookie_header("next_url", Next, [{path, "/"}]).

encoded_next_url_from_params(Params) ->
    Next = proplists:get_value("next", Params, ""),
    base64:encode(Next).

%% ===================================================================
%% Logout handler
%% ===================================================================

handle_logout(_Env) ->
    guild_http:redirect("/", [clear_user_cookie()]).

clear_user_cookie() ->
    psycho_util:cookie_header("user", "", [{path, "/"}]).

%% ===================================================================
%% OAuth callback handler
%% ===================================================================

handle_oauth_callback(Params, Cookie, Config) ->
    {State, TempCode} =
        verified_oauth_state_and_tempcode(Params, Cookie, Config),
    AccessToken = request_oauth_access_token(TempCode, State, Config),
    User = request_user(AccessToken),
    guild_http:redirect(
      next_url(Cookie),
      [user_cookie(User, Config), clear_next_url_cookie()]).

verified_oauth_state_and_tempcode(Params, Cookie, Config) ->
    State = proplists:get_value("state", Params),
    verify_oauth_state(State, Cookie, Config),
    TempCode = proplists:get_value("code", Params),
    {State, TempCode}.

verify_oauth_state(State, Cookie, Config) ->
    verify_oauth_client(current_client_id(Cookie), State, Config).

verify_oauth_client(undefined, _OAuthState, _Config) ->
    error(no_current_client);
verify_oauth_client(ClientId, OAuthState, Config) ->
    ClientState = client_oauth_state(ClientId, Config),
    verify_oauth_state_for_client(ClientState, OAuthState).

verify_oauth_state_for_client(ClientStateBin, OAuthState) ->
    case binary_to_list(ClientStateBin) of
        OAuthState -> ok;
        Other -> error({oauth_state_mismatch, Other, OAuthState})
    end.

request_oauth_access_token(TempCode, OAuthState, Config) ->
    Params =
        [{"client_id", github_client_id(Config)},
         {"client_secret", github_client_secret(Config)},
         {"code", TempCode},
         {"state", OAuthState}],
    handle_access_token_response(
      guild_curl:post(?github_oauth_access_token_url, [{data, Params}])).

handle_access_token_response({ok, Resp}) ->
    parse_access_token_response(Resp).

parse_access_token_response(Resp) ->
    RespStr = binary_to_list(iolist_to_binary(Resp)),
    {_, _, Params} = psycho_util:parse_request_path([$?|RespStr]),
    case lists:keyfind("access_token", 1, Params) of
        {_, AccessToken} -> AccessToken;
        false -> error({access_token_response, Params})
    end.

request_user(AccessToken) ->
    Headers = [{"Authorization", "token " ++ AccessToken}],
    handle_user_response(
      guild_curl:get(?github_user_url, [{headers, Headers}])).

handle_user_response({ok, Resp}) ->
    user_from_json(Resp).

user_from_json(JSON) ->
    {User} = jiffy:decode(JSON),
    lists:foldl(fun user_attrs_acc/2, #{}, User).

user_attrs_acc({<<"login">>,      Val}, Acc) -> Acc#{login    => Val};
user_attrs_acc({<<"name">>,       Val}, Acc) -> Acc#{name     => Val};
user_attrs_acc({<<"avatar_url">>, Val}, Acc) -> Acc#{avatar   => Val};
user_attrs_acc({<<"location">>,   Val}, Acc) -> Acc#{location => Val};
user_attrs_acc({<<"company">>,    Val}, Acc) -> Acc#{company  => Val};
user_attrs_acc(_,                       Acc) -> Acc.

next_url(Cookie) ->
    case proplists:get_value("next_url", Cookie) of
        undefined -> "/";
        "" -> "/";
        NextEncoded -> base64:decode(NextEncoded)
    end.

user_cookie(User, Config) ->
    Encoded = encode_user(User, Config),
    psycho_util:cookie_header("user", Encoded, [{path, "/"}]).

encode_user(User, Config) ->
    TermBin = term_to_binary(User),
    Compressed = zlib:compress(TermBin),
    Key = oauth_encryption_key(Config),
    Encrypted = psycho_util:encrypt(Compressed, Key),
    base64:encode(Encrypted).

clear_next_url_cookie() ->
    psycho_util:cookie_header("next_url", "", [{path, "/"}]).

%% ===================================================================
%% Apply user
%% ===================================================================

apply_user(Env0, Upstream, Config) ->
    Env = try_add_user(Env0, Config),
    psycho:call_app(Upstream, Env).

try_add_user(Env0, Config) ->
    {Cookie, Env} = psycho_util:ensure_parsed_cookie(Env0),
    case proplists:get_value("user", Cookie) of
        undefined -> Env;
        ""        -> Env;
        Encoded   -> try_decode_and_add_user(Encoded, Config, Env)
    end.

try_decode_and_add_user(Encoded, Config, Env) ->
    try decode_user(Encoded, Config) of
        User -> [{user, User}|Env]
    catch
        error:Err -> handle_decode_error(Err, Env)
    end.

handle_decode_error(Err, Env) ->
    guild_log:internal("Error decoding user: ~p~n", [Err]),
    Env.

decode_user(Encoded, Config) ->
    Encrypted = base64:decode(Encoded),
    Key = oauth_encryption_key(Config),
    {ok, Compressed} = psycho_util:decrypt(Encrypted, Key),
    TermBin = zlib:uncompress(Compressed),
    binary_to_term(TermBin).
