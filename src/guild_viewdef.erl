-module(guild_viewdef).

-behavior(erlydtl_library).

-export([init/1, viewdef_section/2, viewdef_path/2,
         generate_viewdef/3]).

-export([version/0, inventory/1]).

-export([field_class/2]).

-define(templates,
        [{"default.config", guild_default_viewdef}]).

-define(default_viewdef_template, guild_default_viewdef).

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
    Opts = [{default_libraries, [?MODULE]}],
    guild_dtl_util:compile_template(File, Module, Opts).

template_file(Name) ->
    filename:join(guild_app:priv_dir("viewdefs"), Name).

%% ===================================================================
%% Viewdef section
%% ===================================================================

viewdef_section(Model, Project) ->
    guild_util:find_apply(
      [fun() -> model_viewdef_section(Model, Project) end,
       fun() -> project_viewdef_section(Project) end]).

model_viewdef_section({["model", Name], _}, Project) ->
    guild_project:section(Project, ["view", Name]).

project_viewdef_section(Project) ->
    guild_project:section(Project, ["view"]).

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
    guild_project:section_attr(Model, "view").

project_viewdef_attr(Project) ->
    guild_project:attr(Project, ["project"], "view").

%% ===================================================================
%% Generate viewdef
%% ===================================================================

generate_viewdef(ViewSection, Model, Project) ->
    Module = viewdef_template(ViewSection),
    maybe_recompile(Module),
    Vars = viewdef_template_vars(ViewSection, Model, Project),
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

viewdef_template_vars(ViewSection, Model, Project) ->
    Fields = viewdef_fields(ViewSection, Model, Project),
    {SeriesA, SeriesB} = viewdef_series(ViewSection, Model, Project),
    CompareFields = viewdef_compare_fields(ViewSection, Project),
    [{fields, Fields},
     {series_a, SeriesA},
     {series_b, SeriesB},
     {compare_fields, CompareFields}].

%% ===================================================================
%% Viewdef fields
%% ===================================================================

viewdef_fields(ViewSection, Model, Project) ->
    Lookup = fields_lookup(Model),
    viewdef_fields_(section_attr(ViewSection, "fields"), Project, Lookup).

fields_lookup(Model) ->
    merge_lookups(
      [default_fields_lookup(),
       runtime_fields_lookup(Model)]).

default_fields_lookup() ->
    read_lookup(lookup_path("default-fields")).

runtime_fields_lookup(Model) ->
    Name = model_runtime(Model) ++ "-fields",
    read_lookup(lookup_path(Name)).

viewdef_fields_({ok, Raw}, Project, Lookup) ->
    Names = parse_names(Raw),
    [resolve_field(Name, Project, Lookup) || Name <- Names];
viewdef_fields_(error, _Project, _Lookup) ->
    [].

resolve_field(Name, Project, Lookup) ->
    BaseAttrs = [{"name", Name}|lookup_defaults(Name, Lookup)],
    apply_project_field(Name, Project, BaseAttrs).

apply_project_field(Name, Project, BaseAttrs) ->
    case guild_project:section(Project, ["field", Name]) of
        {ok, {_, ProjectAttrs}} ->
            merge_attrs(ProjectAttrs, BaseAttrs);
        error ->
            BaseAttrs
    end.

%% ===================================================================
%% Viewdef series
%% ===================================================================

viewdef_series(ViewSection, Model, Project) ->
    Lookup = series_lookup(Model),
    case section_attr(ViewSection, "series") of
        {ok, _}=A ->
            {viewdef_series_(A, Project, Lookup),
             []};
        error ->
            A = section_attr(ViewSection, "series-a"),
            B = section_attr(ViewSection, "series-b"),
            {viewdef_series_(A, Project, Lookup),
             viewdef_series_(B, Project, Lookup)}
        end.

series_lookup(Model) ->
    merge_lookups(
      [default_series_lookup(),
       runtime_series_lookup(Model)]).

default_series_lookup() ->
    read_lookup(lookup_path("default-series")).

runtime_series_lookup(Model) ->
    Name = model_runtime(Model) ++ "-series",
    read_lookup(lookup_path(Name)).

viewdef_series_({ok, Raw}, Project, Lookup) ->
    Names = parse_names(Raw),
    [resolve_series(Name, Project, Lookup) || Name <- Names];
viewdef_series_(error, _Project, _Lookup) ->
    [].

resolve_series(Name, Project, Lookup) ->
    BaseAttrs = [{"name", Name}|lookup_defaults(Name, Lookup)],
    apply_project_series(Name, Project, BaseAttrs).

apply_project_series(Name, Project, BaseAttrs) ->
    case guild_project:section(Project, ["series", Name]) of
        {ok, {_, ProjectAttrs}} ->
            merge_attrs(ProjectAttrs, BaseAttrs);
        error ->
            BaseAttrs
    end.

%% ===================================================================
%% Viewdef compare fields
%% ===================================================================

viewdef_compare_fields(ViewSection, Project) ->
    Runtimes = project_runtimes(Project),
    Lookups = fields_lookups_for_runtimes(Runtimes),
    FieldNames = compare_field_names(ViewSection),
    [viewdef_compare_field(Name, Project, Lookups) || Name <- FieldNames].

project_runtimes(Project) ->
    project_runtimes_acc(
      guild_project:sections(Project, ["model"]),
      sets:new()).

project_runtimes_acc([Model|Rest], S) ->
    project_runtimes_acc(Rest, sets:add_element(model_runtime(Model), S));
project_runtimes_acc([], S) -> sets:to_list(S).

compare_field_names(ViewSection) ->
    Names =
        guild_util:find_apply(
          [fun() -> section_attr(ViewSection, "compare") end,
           fun() -> section_attr(ViewSection, "fields") end],
          [], ""),
    parse_names(Names).

fields_lookups_for_runtimes(Runtimes) ->
    [fields_lookup_for_runtime(Runtime) || Runtime <- Runtimes].

fields_lookup_for_runtime(Runtime) ->
    merge_lookups(
      [default_fields_lookup(),
       read_lookup(lookup_path(Runtime ++ "-fields"))]).

viewdef_compare_field(Name, Project, [Lookup|ExtraLookups]) ->
    Field = resolve_field(Name, Project, Lookup),
    ExtraSources = field_extra_sources(Name, ExtraLookups),
    apply_extra_sources(Field, ExtraSources).

field_extra_sources(Name, Lookups) ->
    field_extra_sources_acc(Name, Lookups, sets:new()).

field_extra_sources_acc(Name, [Lookup|Rest], S) ->
    Field = lookup_defaults(Name, Lookup),
    field_extra_sources_acc(Name, Rest, maybe_apply_field_source(Field, S));
field_extra_sources_acc(_Name, [], S) ->
    S.

maybe_apply_field_source(Field, S) ->
    case proplists:get_value("source", Field, "") of
        "" -> S;
        Source -> sets:add_element(Source, S)
    end.

apply_extra_sources(Field, SourcesSet) ->
    Sources = sets:to_list(maybe_apply_field_source(Field, SourcesSet)),
    [{"sources", Sources}|Field].

%% ===================================================================
%% General support
%% ===================================================================

model_runtime(Model) ->
    {ok, Runtime} = section_attr(Model, "runtime"),
    Runtime.

section_attr(Section, Name) ->
    guild_project:section_attr(Section, Name).

lookup_path(Name) ->
    filename:join(guild_app:priv_dir("viewdefs"), Name ++ ".config").

read_lookup(Path) ->
    case file:consult(Path) of
        {ok, Lookup} -> Lookup;
        {error, enoent} -> []
    end.

parse_names("") -> [];
parse_names(Raw) ->
    Split = re:split(Raw, "\\s+", [{return, list}]),
    [strip_whitespace(S) || S <- Split].

strip_whitespace(S) ->
    {match, [Stripped]} =
        re:run(
          S, "\\s*(.*?)\\s*$",
          [{capture, all_but_first, list}]),
    Stripped.

lookup_defaults(FieldName, Lookup) ->
    [{atom_to_list(AttrName), Val}
     || {AttrName, Val} <- proplists:get_value(FieldName, Lookup, [])].

merge_attrs(P1, P2) -> P1 ++ P2.

merge_lookups([Working, Next|Rest]) ->
    merge_lookups([merge_lookups(Working, Next)|Rest]);
merge_lookups([Merged]) ->
    Merged.

merge_lookups(Working, New) ->
    merge_lookups_acc(New, Working, Working).

merge_lookups_acc([{Name, NewAttrs}|Rest], Working, Acc) ->
    CurAttrs = proplists:get_value(Name, Working, []),
    MergedAttrs = merge_attrs(NewAttrs, CurAttrs),
    merge_lookups_acc(Rest, Working, [{Name, MergedAttrs}|Acc]);
merge_lookups_acc([], _Working, Acc) ->
    Acc.

%% ===================================================================
%% Template support
%% ===================================================================

version() -> 1.

inventory(filters) ->
    [field_class];
inventory(tags) ->
    [].

field_class(_Field, Fields) ->
    case length(Fields) of
        1 -> "col-md-12";
        2 -> "col-md-6";
        3 -> "col-md-4";
        _ -> "col-lg-3 col-md-6 col-sm-6"
    end.
