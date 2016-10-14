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

-module(guild_project_util).

-export([runroot/1, runroot/2, all_runroots/1, flags/2]).

%% ===================================================================
%% Runroot
%% ===================================================================

runroot(Project) ->
    runroot(undefined, Project).

runroot(Section, Project) ->
    {ok, Root} =
        guild_util:find_apply(
          [fun() -> section_runroot(Section) end,
           fun() -> project_runroot(Project) end,
           fun() -> user_configured_runroot() end,
           fun() -> {ok, default_runroot(Project)} end],
          []),
    filename:absname(Root, guild_project:project_dir(Project)).

section_runroot(undefined) ->
    error;
section_runroot(Section) ->
    guild_project:section_attr(Section, "runroot").

project_runroot(Project) ->
    guild_project:attr(Project, ["project"], "runroot").

user_configured_runroot() ->
    guild_user:config(["defaults"], "runroot").

default_runroot(Project) ->
    filename:join(guild_project:project_dir(Project), "runs").

all_runroots(Project) ->
    guild_util:fold_apply(
      [fun(S) -> apply_project_sections_runroot(Project, S) end,
       fun(S) -> apply_project_runroot(Project, S) end,
       fun(S) -> sets:to_list(S) end],
      sets:new()).

apply_project_sections_runroot(Project, Set) ->
    Sections = guild_project:sections(Project, []),
    ProjectDir = guild_project:project_dir(Project),
    Apply =
        fun(Section, Acc) ->
            apply_project_section_runroot(Section, ProjectDir, Acc)
        end,
    lists:foldl(Apply, Set, Sections).

apply_project_section_runroot(Section, ProjectDir, Set) ->
    case section_runroot(Section) of
        {ok, Root} ->
            AbsRoot = filename:absname(Root, ProjectDir),
            sets:add_element(AbsRoot, Set);
        error ->
            Set
    end.

apply_project_runroot(Project, Set) ->
    sets:add_element(runroot(Project), Set).

%% ===================================================================
%% Flags
%% ===================================================================

flags(Section, Project) ->
    Path = flags_path_for_section(Section),
    guild_project:section_attr_union(Project, Path).

flags_path_for_section({[_, Name|_], _}) ->
    [["flags", cmdline], ["flags", profile], ["flags", Name], ["flags"]];
flags_path_for_section({[_], _}) ->
    [["flags", cmdline], ["flags", profile], ["flags"]].
