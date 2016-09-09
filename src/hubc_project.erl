-module(hubc_project).

-export([from_dir/1, from_str/1, attr/3, set_attr/4, section/2,
         sections/2, section_name/1, section_attrs/2, section_attrs/1,
         section_attr/2, section_attr/3, section_attr_union/2,
         project_file/1, project_file_for_dir/1, project_dir/1]).

-define(project_basename, "TensorHub").
-define(default_logs_root, "logs").

%% ===================================================================
%% Init
%% ===================================================================

from_dir(Dir) ->
    Project = load(project_file_for_dir(Dir)),
    maybe_apply_dir(Project, Dir).

maybe_apply_dir({ok, Project}, Dir) ->
    {ok, [{'$dir', Dir}|Project]};
maybe_apply_dir({error, Err}, _Dir) ->
    {error, Err}.

from_str(Str) ->
    inifile:parse(Str).

load(File) ->
    case inifile:load(File) of
        {ok, Project}   -> {ok, Project};
        {error, enoent} -> {error, missing_project_file};
        {error, Err}    -> {error, Err}
    end.

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

%% ===================================================================
%% Utils
%% ===================================================================

project_file(Project) ->
    project_file_for_dir(project_dir(Project)).

project_file_for_dir(Dir) ->
    filename:join(Dir, ?project_basename).

project_dir(Project) ->
    proplists:get_value('$dir', Project).
