-module(hubc_model).

-export([name_for_project_section/1, name_for_project_section/2,
         find_model_for_name/2, project_section_id/1]).

name_for_project_section({["model", Name|_], _}) ->
    {ok, Name};
name_for_project_section(Section) ->
    case hubc_project:section_attr(Section, "train") of
        {ok, TrainAttr} -> {ok, name_for_attr(TrainAttr)};
        error -> error
    end.

name_for_project_section(Section, Default) ->
    case name_for_project_section(Section) of
        {ok, Name} -> Name;
        error -> Default
    end.

name_for_attr(Attr) ->
    hd(re:split(Attr, "\\s", [{return, list}])).

find_model_for_name(Project, Name) ->
    hubc_util:find_apply(
      [fun find_model_for_section_name/2,
       fun find_model_for_train_cmd/2],
      [Project, Name]).

find_model_for_section_name(Project, Name) ->
    case hubc_project:sections(Project, ["model", Name]) of
        [Model|_] -> {ok, Model};
        [] -> error
    end.

find_model_for_train_cmd([{["model"|_], _}=Model|Rest], TrainCmd) ->
    case hubc_project:section_attr(Model, "train") of
        {ok, TrainCmd} -> {ok, Model};
        _ -> find_model_for_train_cmd(Rest, TrainCmd)
    end;
find_model_for_train_cmd([_|Rest], TrainCmd) ->
    find_model_for_train_cmd(Rest, TrainCmd);
find_model_for_train_cmd([], _TrainCmd) ->
    error.

project_section_id({["model", Name|_], _}) -> Name;
project_section_id(_) -> undefined.
