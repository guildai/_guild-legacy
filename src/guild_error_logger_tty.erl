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

-module(guild_error_logger_tty).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-define(logger, error_logger_tty_h).

init(Args) ->
    ?logger:init(Args).

handle_event({error, _, {_, _, Data}}=Event, State) ->
    case is_operation_exit(Data) of
        true -> {ok, State};
        false -> ?logger:handle_event(Event, State)
    end;
handle_event(Event, State) ->
    ?logger:handle_event(Event, State).

is_operation_exit([Name, {'EXIT', _, _}|_]) when
      Name == guild_train_op;
      Name == guild_eval_op;
      Name == guild_prepare_op
      -> true;
is_operation_exit(_) -> false.

handle_info(Msg, State) ->
    ?logger:handle_info(Msg, State).

handle_call(Query, State) ->
    ?logger:handle_call(Query, State).

terminate(Reason, State) ->
    ?logger:terminate(Reason, State).

code_change(OldVsn, State, Extra) ->
    ?logger:code_change(OldVsn, State, Extra).
