-module(hubc_collector_protocol).

-export([new_input_buffer/0, input/2]).

-define(PART_DELIM, <<"\n\n">>).

new_input_buffer() -> {[], undefined}.

input(Buf, Bin) ->
    Now = input_timestamp(),
    handle_parts(split_input(Bin), Now, Buf).

input_timestamp() ->
    erlang:system_time(milli_seconds).

split_input(Bin) ->
    re:split(Bin, ?PART_DELIM, [{return, binary}]).

handle_parts([<<>>], _Now, Buf) ->
    finalize_decoded(Buf);
handle_parts([Part], Now, Buf) ->
    finalize_decoded(buffer_part(Part, Now, Buf));
handle_parts([Part|Rest], Now, Buf) ->
    NextBuf = finalize_parts(buffer_part(Part, Now, Buf)),
    handle_parts(Rest, Now, finalize_parts(NextBuf)).

buffer_part(Part, Now, {Decoded, undefined}) ->
    {Decoded, {Now, [Part]}};
buffer_part(Part, _Now, {Decoded, {Time, Parts}}) ->
    {Decoded, {Time, [Part|Parts]}}.

finalize_decoded({Decoded, Working}) ->
    {lists:reverse(Decoded), {[], Working}}.

finalize_parts({Decoded, {Time, Parts}}) ->
    {[{Time, decode(lists:reverse(Parts))}|Decoded], undefined};
finalize_parts({Decoded, undefined}) ->
    {Decoded, undefined}.

decode([<<>>]) -> eof;
decode(Bin) ->
    case hubc_json:try_decode(Bin) of
        {ok, Decoded} -> format_decoded(Decoded);
        {error, _Err} -> {invalid, Bin}
    end.

format_decoded({[{<<"kv">>, {KeyVals}}]}) ->
    {kv, KeyVals};
format_decoded({[{<<"ktsv">>, {KeyTimeStepVals}}]}) ->
    {ktsv, KeyTimeStepVals};
format_decoded(Other) ->
    {other, Other}.
