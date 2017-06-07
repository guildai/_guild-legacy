-module(guild_op).

-export([start_link/2]).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{cmd_info, 1},
     {handle_task, 1}].

start_link(Name, {Mod, State}) ->
    e2_task:start_link(Mod, State, [{registered, Name}]).
