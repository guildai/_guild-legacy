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

-module(guild_project_view_model_http).

-export([app/4]).

%% ===================================================================
%% App
%% ===================================================================

app("POST", {"/model/run", _, Params}, Env, View) ->
    Dispatch = fun(Data, _) -> handle_model_run(Params, Data, View) end,
    {recv_form_data, Dispatch, Env};
app(_, _, _, _) ->
    guild_http:bad_request().

%% ===================================================================
%% Model run
%% ===================================================================

handle_model_run(Params, {ok, Data}, _View) ->
    guild_http:ok_text(io_lib:format("Params: ~p~nData: ~p~n", [Params, Data])).
