-module(guild_viewdef).

-export([init/1, viewdef_section/2, viewdef_path/2,
         generate_viewdef/2]).

-define(templates,
        [{"default.config", guild_default_viewdef}]).

-define(viewdef_section, "view").
-define(viewdef_attr, "view").
-define(default_viewdef_template, guild_default_viewdef).
-define(default_fields_lookup, "default-fields").

%% ===================================================================
%% Init
%% ===================================================================

init(Opts) ->
    compile_templates(),
    guild_app:set_env(
      recompile_viewdef_templates, recompile_templates_opt(Opts)).

recompile_templates_opt(Opts) ->
    proplists:get_bool(recompile_templates, Opts).

compile_templates() ->
    lists:foreach(fun compile_template/1, ?templates).

compile_template({Name, Module}) ->
    File = template_file(Name),
    guild_dtl_util:compile_template(File, Module, []).

template_file(Name) ->
    filename:join(guild_app:priv_dir("viewdefs"), Name).

%% ===================================================================
%% Viewdef section
%% ===================================================================

viewdef_section(Model, Project) ->
    guild_util:find_apply(
      [fun() -> model_viewdef_section(Model, Project) end,
       fun() -> project_viewdef_section(Project) end]).

model_viewdef_section({Name, _}, Project) ->
    guild_project:section(Project, [?viewdef_section, Name]).

project_viewdef_section(Project) ->
    guild_project:section(Project, [?viewdef_section]).

%% ===================================================================
%% Viewdef path
%% ===================================================================

viewdef_path(Model, Project) ->
    case viewdef_attr(Model, Project) of
        {ok, Val} -> viewdef_path(Val, Project);
        error -> error
    end.

viewdef_attr(Model, Project) ->
    guild_util:find_apply(
      [fun() -> model_viewdef_attr(Model) end,
       fun() -> project_viewdef_attr(Project) end]).

model_viewdef_attr(Model) ->
    guild_project:section_attr(Model, ?viewdef_attr).

project_viewdef_attr(Project) ->
    guild_project:attr(Project, ["project"], ?viewdef_attr).

%% ===================================================================
%% Generate viewdef
%% ===================================================================

generate_viewdef(Section, Project) ->
    Module = viewdef_template(Section),
    maybe_recompile(Module),
    Vars = viewdef_template_vars(Section, Project),
    case Module:render(Vars) of
        {ok, Bin} -> rendered_template_to_viewdef(Bin);
        {error, Err} -> error({render, Module, Vars, Err})
    end.

viewdef_template(_Section) -> ?default_viewdef_template.

maybe_recompile(Module) ->
    case guild_app:get_env(recompile_dtl, false) of
        true -> compile_template(template_for_module(Module));
        false -> ok
    end.

template_for_module(Module) ->
    case lists:keyfind(Module, 2, ?templates) of
        {_, _}=T -> T;
        false -> error({no_such_template, Module})
    end.

rendered_template_to_viewdef(Bin) ->
    Str = binary_to_list(iolist_to_binary(Bin)),
    guild_util:consult_string(Str).

viewdef_template_vars(Section, Project) ->
    [{fields, viewdef_fields(Section, Project)}].

viewdef_fields(Section, Project) ->
    case guild_project:section_attr(Section, "fields") of
        {ok, Raw} ->
            Names = parse_viewdef_fields(Raw),
            Lookup = fields_lookup(Section),
            resolve_fields(Names, Project, Lookup);
        error -> []
    end.

parse_viewdef_fields(Raw) ->
    Split = re:split(Raw, ",", [{return, list}]),
    [strip_whitespace(S) || S <- Split].

strip_whitespace(S) ->
    {match, [Stripped]} =
        re:run(
          S, "\\s*(.*?)\\s*$",
          [{capture, all_but_first, list}]),
    Stripped.

fields_lookup(_Section) ->
    Path = fields_lookup_path(?default_fields_lookup),
    {ok, Fields} = file:consult(Path),
    Fields.

fields_lookup_path(Name) ->
    filename:join(guild_app:priv_dir("viewdefs"), Name ++ ".config").

resolve_fields(Names, Project, Lookup) ->
    [resolve_field(Name, Project, Lookup) || Name <- Names].

resolve_field(Name, Project, Lookup) ->
    BaseAttrs = [{"name", Name}|field_defaults(Name, Lookup)],
    apply_project_field(Name, Project, BaseAttrs).

field_defaults(FieldName, Lookup) ->
    [{atom_to_list(AttrName), Val}
     || {AttrName, Val} <- proplists:get_value(FieldName, Lookup, [])].

apply_project_field(Name, Project, BaseAttrs) ->
    case guild_project:section(Project, ["field", Name]) of
        {ok, {_, ProjectAttrs}} ->
            merge_field_attrs(ProjectAttrs, BaseAttrs);
        error ->
            BaseAttrs
    end.

merge_field_attrs(P1, P2) -> P1 ++ P2.
    %% P1Sorted = lists:sort(P1),
    %% P2Sorted = lists:sort(P2),
    %% lists:ukeymerge(1, P1Sorted, P2Sorted).
