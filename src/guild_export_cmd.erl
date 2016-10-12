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

-module(guild_export_cmd).

-export([parser/0, main/2]).

parser() ->
    cli:parser(
      "guild export",
      "[OPTION]... RUNDIR LOCATION",
      "Export results from RUNDIR to an archive.\n"
      "\n"
      "LOCATION can be either a directory or a new file. If LOCATION is "
      "a directory, the command will generate a unique archive name. If "
      "LOCATION is a new file, the command will create that archive.\n"
      "\n"
      "Use 'guild list-runs' to list runs that can be used for RUNDIR.",
      export_opts(),
      [{pos_args, 2}]).

export_opts() ->
    [{force, "-f, --force", "overwrite existing archives", [flag]}].

main(Opts, [RunDir, Location]) ->
    guild_cmd_support:validate_rundir(RunDir),
    Path = export(RunDir, Location, Opts),
    guild_cli:out("Run exported to '~s'~n", [Path]).

export(RunDir, Location, Opts) ->
    Path = archive_path_for_location(Location, RunDir, Opts),
    Tar = open_tar(Path),
    add_guild_dir(Tar, RunDir),
    close_tar(Tar),
    Path.

archive_path_for_location(Location, RunDir, Opts) ->
    Force = proplists:get_bool(force, Opts),
    case {location_type(Location), Force} of
        {directory, _} -> generate_archive_path(Location, RunDir);
        {file, true}   -> Location;
        {file, false}  -> file_exists_error(Location);
        {path, _}      -> Location
    end.

location_type(Location) ->
    case {filelib:is_dir(Location), filelib:is_file(Location)} of
        {true, true}   -> directory;
        {false, true}  -> file;
        {false, false} -> path
    end.

generate_archive_path(Dir, _RunDir) ->
    Name = "guild-export-" ++ guild_util:random_name() ++ ".tar.gz",
    filename:join(Dir, Name).

file_exists_error(Location) ->
    guild_cli:cli_error(
      io_lib:format(
        "'~s' exists\nUse --force to replace it.", [Location])).

open_tar(Path) ->
    case erl_tar:open(Path, [write, compressed]) of
        {ok, Tar} -> Tar;
        {error, Err} -> open_tar_error(Err)
    end.

open_tar_error({Path, enoent}) ->
    Dir = filename:dirname(Path),
    guild_cli:cli_error(
      io_lib:format("output directory '~s' does exist", [Dir]));
open_tar_error(Err) ->
    guild_cli:cli_error(
      io_lib:format("Error creating export: ~p", [Err])).

add_guild_dir(Tar, RunDir) ->
    GuildDir = guild_rundir:guild_dir(RunDir),
    ok = erl_tar:add(Tar, GuildDir, "guild.d", []).

close_tar(Tar) ->
    ok = erl_tar:close(Tar).
