-module(hubc_json).

-export([init/0, encode/1, try_encode/1, try_decode/1]).

init() ->
    ok = jiffy:init(nif_path()).

nif_path() ->
    filename:join(hubc_util:priv_dir(jiffy), "jiffy").

encode(Term) ->
    jiffy:encode(Term).

try_encode(Term) ->
    try jiffy:encode(Term) of
        Bin -> {ok, Bin}
    catch
        throw:{error, Err} -> {error, Err}
    end.

try_decode(Bin) ->
    try jiffy:decode(Bin) of
        Term -> {ok, Term}
    catch
        throw:{error, Err} -> {error, Err}
    end.
