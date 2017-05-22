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

-module(guild_project).

-export([from_dir/1, from_file/1, from_str/1, project_dir/1,
         project_file/1, attr/3, set_attr/4, section/2, sections/2,
         section_name/1, section_attrs/2, section_attrs/1,
         section_attr/2, section_attr/3, section_attr_union/2]).

%% ===================================================================
%% Init
%% ===================================================================

from_dir(Dir) ->
    from_file(filename:join(Dir, "Guild")).

from_file(Path) ->
    apply_path_attrs(load(Path), Path).

apply_path_attrs({ok, Project}, Path) ->
    Attrs =
        [{'$file', Path},
         {'$dir', filename:dirname(Path)}],
    {ok, Attrs ++ Project};
apply_path_attrs({error, Err}, _Dir) ->
    {error, Err}.

from_str(Str) ->
    inifile:parse(Str).

load(File) ->
    case inifile:load(File) of
        {ok, Project}   -> {ok, Project};
        {error, enoent} -> {error, {missing_project_file, File}};
        {error, Err}    -> {error, Err}
    end.

%% ===================================================================
%% Path API
%% ===================================================================

project_dir(Project) ->
    proplists:get_value('$dir', Project).

project_file(Project) ->
    proplists:get_value('$file', Project).

%% ===================================================================
%% Attr API
%% ===================================================================

attr(Project, SectionPath, AttrName) ->
    case section(Project, SectionPath) of
        {ok, Section} -> section_attr(Section, AttrName);
        error -> error
    end.

set_attr(Project, SectionPath, AttrName, AttrVal) ->
    Section = section_or_new(Project, SectionPath),
    set_section(Project, section_set_attr(Section, AttrName, AttrVal)).

section_or_new(Project, SectionPath) ->
    case section(Project, SectionPath) of
        {ok, Section} -> Section;
        error -> {SectionPath, []}
    end.

section_set_attr({Key, Attrs}, AttrName, AttrVal) ->
    {Key, lists:keystore(AttrName, 1, Attrs, {AttrName, AttrVal})}.

set_section(Project, {Key, _}=Section) ->
    lists:keystore(Key, 1, Project, Section).

%% ===================================================================
%% Section API
%% ===================================================================

section(Project, Path) ->
    case lists:keyfind(Path, 1, Project) of
        {_, Attrs} -> {ok, {Path, Attrs}};
        false -> error
    end.

sections(Project, PathPrefix) ->
    [Section || Section <- Project, match_section(PathPrefix, Section)].

match_section(Prefix, {Key, _}) when is_list(Key) ->
    lists:prefix(Prefix, Key);
match_section(_Prefix, _Section) ->
    false.

section_attrs(Project, SectionPath) ->
    case section(Project, SectionPath) of
        {ok, Section} -> section_attrs(Section);
        error -> []
    end.

section_name({[_, Name|_], _}) -> Name;
section_name(_) -> undefined.

section_attrs({_, Attrs}) -> Attrs.

section_attr({_Key, Attrs}, Name) ->
    case lists:keyfind(Name, 1, Attrs) of
        {_, Val} -> {ok, Val};
        false -> error
    end.

section_attr({_Key, Attrs}, Name, Default) ->
    case lists:keyfind(Name, 1, Attrs) of
        {_, Val} -> Val;
        false -> Default
    end.

section_attr_union(Project, Paths) ->
    section_attrs_acc(Project, lists:reverse(Paths), []).

section_attrs_acc(Project, [Path|Rest], Acc) ->
    section_attrs_acc(section_attrs(Project, Path), Project, Rest, Acc);
section_attrs_acc(_Project, [], Acc) ->
    Acc.

section_attrs_acc([{Name, _}=Attr|Rest], Project, RestPaths, Acc) ->
    section_attrs_acc(
      Rest, Project, RestPaths,
      lists:keystore(Name, 1, Acc, Attr));
section_attrs_acc([], Project, RestPaths, Acc) ->
    section_attrs_acc(Project, RestPaths, Acc).
