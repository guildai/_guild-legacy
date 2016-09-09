-module(hubc_log).

-export([internal/1, internal/2]).

internal(Output) ->
    io:put_chars(standard_error, Output).

internal(Msg, Data) ->
    io:format(standard_error, Msg, Data).
