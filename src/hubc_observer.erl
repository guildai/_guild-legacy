-module(hubc_observer).

-export([maybe_start_from_global_opts/1]).

maybe_start_from_global_opts(Opts) ->
    maybe_start_observer(proplists:get_bool(observer, Opts)).

maybe_start_observer(true) ->
    observer:start(),
    hubc_proc:reg(global, observer);
maybe_start_observer(false) ->
    ok.
