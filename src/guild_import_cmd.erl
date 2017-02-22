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

-module(guild_import_cmd).

-export([parser/0, main/2]).

parser() ->
    cli:parser(
      "guild import",
      "[OPTION]... ARCHIVE",
      "Import exported run results from ARCHIVE into the project.",
      guild_cmd_support:project_options(),
      [{pos_args, 1}]).

main(Opts, [Archive]) ->
    Project = guild_cmd_support:project_from_opts(Opts),
    import(Archive, Project).

import(Archive, _Project) ->
    guild_app:init_support(exec),
    {ok, Tmp} = guild_util:make_tmp_dir(),
    Cmd = extract_cmd(Archive, Tmp),
    io:format("TODO: run ~p to get some files yo~n", [Cmd]).

extract_cmd(File, DestDir) ->
    ["/bin/tar", "-xf", File, "-C", DestDir].
