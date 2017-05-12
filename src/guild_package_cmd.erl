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

-module(guild_package_cmd).

-export([parser/0, main/2]).

parser() ->
    cli:parser(
      "guild package",
      "[OPTION]...",
      "Creates a Guild package.",
      []).

%% ===================================================================
%% Main
%% ===================================================================

main(_Opts, _Args) ->
    io:format("TODO: check dependencies (e.g. tensorflow-1.1+)~n"),
    io:format("TODO: download sources~n"),
    io:format("TODO: run package script~n"),
    io:format("TODO: create package~n"),
    io:format("DONE").
