-module(hubc_trace).

-export([init_from_global_opts/1, init_from_env/1]).

init_from_global_opts(Opts) ->
    apply_trace(parse_trace(trace_opt(Opts))).

init_from_env(false) ->
    ok;
init_from_env(Env) ->
    apply_trace(parse_trace(Env)).

trace_opt(Opts) ->
    proplists:get_value(trace, Opts).

parse_trace(undefined) ->
    [];
parse_trace(Str) ->
    [trace_spec(Token) || Token <- string:tokens(Str, ",")].

trace_spec(Str) ->
    [list_to_atom(Token) || Token <- string:tokens(Str, ":")].

apply_trace([[M]|Rest]) ->
    e2_debug:trace_module(M),
    apply_trace(Rest);
apply_trace([[M, F]|Rest]) ->
    e2_debug:trace_function(M, F),
    apply_trace(Rest);
apply_trace([]) ->
    ok.
