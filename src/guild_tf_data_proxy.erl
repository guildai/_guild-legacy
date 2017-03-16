-module(guild_tf_data_proxy).

-export([data/2]).

-record(resp, {sock, len, type, status, body}).

data(Port, Path) ->
    Opts = [binary, {packet, http_bin}, {active, false}],
    {ok, Sock} = gen_tcp:connect("localhost", Port, Opts),
    Req = request(Path),
    ok = gen_tcp:send(Sock, Req),
    headers(#resp{sock=Sock, body=[]}).

request(Path) ->
    [<<"GET /data/">>, Path, <<" HTTP/1.0\r\n\r\n">>].

headers(#resp{sock=Sock}=Resp) ->
    handle_header(gen_tcp:recv(Sock, 0), Resp).

handle_header({ok, {http_response, _, Code, Reason}}, R) ->
    headers(R#resp{status={Code, Reason}});
handle_header({ok, {http_header, _, 'Content-Type', _, Type}}, R) ->
    headers(R#resp{type=Type});
handle_header({ok, {http_header, _, 'Content-Length', _, Val}}, R) ->
    headers(R#resp{len=binary_to_integer(Val)});
handle_header({ok, {http_header, _, _, _, _}}, R) ->
    headers(R);
handle_header({ok, http_eoh}, R) ->
    maybe_body(R);
handle_header({error, Err}, R) ->
    handle_error(Err, R).

handle_error(Err, #resp{sock=Sock}) ->
    gen_tcp:close(Sock),
    {error, Err}.

maybe_body(R) ->
    resp_check(
      [fun resp_status/1,
       fun resp_type/1,
       fun resp_len/1],
      fun body/1, R).

resp_check([Check|Rest], Next, R) ->
    case Check(R) of
        ok -> resp_check(Rest, Next, R);
        {error, Err} -> handle_error(Err, R)
    end;
resp_check([], Next, R) -> Next(R).

resp_status(#resp{status={200, <<"OK">>}}) -> ok;
resp_status(#resp{status=Status}) -> {error, {status, Status}}.

resp_type(#resp{type= <<"application/json">>}) -> ok;
resp_type(#resp{type=Type}) -> {error, {type, Type}}.

resp_len(#resp{len=Len}) when is_integer(Len), Len >= 0 -> ok;
resp_len(#resp{len=Len}) -> {error, {resp_len, Len}}.

body(#resp{sock=Sock, len=Len}=Resp) ->
    ok = inet:setopts(Sock, [{packet, raw}]),
    handle_body(gen_tcp:recv(Sock, Len), Resp).

handle_body({ok, Bin}, #resp{len=Len, body=Body}=R) ->
    body(R#resp{len=Len-size(Bin), body=[Bin|Body]});
handle_body({error, closed}, #resp{len=0, body=Body}) ->
    {ok, lists:reverse(Body)};
handle_body({error, Err}, R) ->
    handle_error(Err, R).
