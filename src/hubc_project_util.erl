-module(hubc_project_util).

-export([logroot/1, logroot/2, all_logroots/1, flags/2]).

%% ===================================================================
%% Logroot
%% ===================================================================

logroot(Project) ->
    logroot(undefined, Project).

logroot(Section, Project) ->
    {ok, Root} =
        hubc_util:find_apply(
          [fun() -> section_logroot(Section) end,
           fun() -> project_logroot(Project) end,
           fun() -> user_configured_logroot() end,
           fun() -> {ok, default_logroot(Project)} end],
          []),
    filename:absname(Root, hubc_project:project_dir(Project)).

section_logroot(undefined) ->
    error;
section_logroot(Section) ->
    hubc_project:section_attr(Section, "logroot").

project_logroot(Project) ->
    hubc_project:attr(Project, ["project"], "logroot").

user_configured_logroot() ->
    hubc_user:config(["defaults"], "logroot").

default_logroot(Project) ->
    filename:join(hubc_project:project_dir(Project), "logs").

all_logroots(Project) ->
    hubc_util:fold_apply(
      [fun(S) -> apply_project_sections_logroot(Project, S) end,
       fun(S) -> apply_project_logroot(Project, S) end,
       fun(S) -> sets:to_list(S) end],
      sets:new()).

apply_project_sections_logroot(Project, Set) ->
    Sections = hubc_project:sections(Project, []),
    ProjectDir = hubc_project:project_dir(Project),
    Apply =
        fun(Section, Acc) ->
            apply_project_section_logroot(Section, ProjectDir, Acc)
        end,
    lists:foldl(Apply, Set, Sections).

apply_project_section_logroot(Section, ProjectDir, Set) ->
    case section_logroot(Section) of
        {ok, Root} ->
            AbsRoot = filename:absname(Root, ProjectDir),
            sets:add_element(AbsRoot, Set);
        error ->
            Set
    end.

apply_project_logroot(Project, Set) ->
    sets:add_element(logroot(Project), Set).

%% ===================================================================
%% Flags
%% ===================================================================

flags(Section, Project) ->
    Path = flags_path_for_section(Section),
    hubc_project:section_attr_union(Project, Path).

flags_path_for_section({[_, Name|_], _}) ->
    [["flags", cmdline], ["flags", profile], ["flags", Name], ["flags"]];
flags_path_for_section({[_], _}) ->
    [["flags", cmdline], ["flags", profile], ["flags"]].
