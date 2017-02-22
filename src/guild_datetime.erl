%% Copyright 2016-2017 TensorHub, Inc.
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

-module(guild_datetime).

-export([timestamp_to_now/1, format_date/1]).

timestamp_to_now(EpochMs) ->
    Mega = EpochMs div 1000000000,
    Sec = (EpochMs - Mega * 1000000000) div 1000,
    Micro = (EpochMs - (Mega * 1000000000) - (Sec * 1000)) * 1000,
    {Mega, Sec, Micro}.

format_date(EpochSeconds) when is_integer(EpochSeconds) ->
    format_date(timestamp_to_now(EpochSeconds * 1000));
format_date({_, _, _}=Now) ->
    {{Y, M, D}, _} = calendar:now_to_datetime(Now),
    io_lib:format("~s ~b, ~p", [month_en(M), D, Y]).

month_en(1) -> "Jan";
month_en(2) -> "Feb";
month_en(3) -> "Mar";
month_en(4) -> "Apr";
month_en(5) -> "May";
month_en(6) -> "Jun";
month_en(7) -> "Jul";
month_en(8) -> "Aug";
month_en(9) -> "Sep";
month_en(10) -> "Oct";
month_en(11) -> "Nov";
month_en(12) -> "Dec".
