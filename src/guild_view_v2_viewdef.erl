-module(guild_view_v2_viewdef).

-export([viewdef/2]).

%% ===================================================================
%% Viewdef
%% ===================================================================

viewdef(Model, Project) ->
    {ok, ViewSection} = view_section(Model, Project),
    #{
       pages => pages(ViewSection, Model, Project)
     }.

view_section(Model, Project) ->
    guild_util:find_apply(
      [fun() -> model_view_section(Model, Project) end,
       fun() -> project_view_section(Project) end],
      []).

model_view_section({["model", Name], _}, Project) ->
    guild_project:section(Project, ["view", Name]);
model_view_section(_Section, _Project) ->
    error.

project_view_section(Project) ->
    guild_project:section(Project, ["view"]).

%% ===================================================================
%% Pages
%% ===================================================================

pages(ViewSection, Model, Project) ->
    default_pages(ViewSection, Model, Project).

default_pages(ViewSection, Model, Project) ->
    [#{id     => <<"overview">>,
       label  => <<"Overview">>,
       icon   => <<"apps">>,
       layout => overview_layout(ViewSection, Model, Project)
      },
     #{id     => <<"compare">>,
       label  => <<"Compare">>,
       icon   => <<"av:playlist-add-check">>,
       layout => compare_layout(ViewSection, Model, Project)
     },
     #{id     => <<"tensorboard">>,
       label  => <<"TensorBoard">>,
       icon   => <<"timeline">>,
       layout => tensorboard_layout()
      }
    ].

%% ===================================================================
%% Overview layout
%% ===================================================================

overview_layout(ViewSection, Model, Project) ->
    container(
      [row(
         [col(
            <<"col-12">>,
            [component(<<"guild-run-select-page-header">>)])]),
       row(
         [col(
            <<"col-lg-8 col-xl-9">>,
            [row(field_cols(ViewSection, Model, Project)),
             row(chart_cols(ViewSection, Model, Project)),
             component(<<"guild-output">>)]),
          col(
            <<"col-lg-4 col-xl-3">>,
            [component(<<"guild-flags">>),
             component(<<"guild-attrs">>)])])]).

%% ===================================================================
%% Fields
%% ===================================================================

field_cols(ViewSection, Model, Project) ->
    [field_col(Field) || Field <- fields(ViewSection, Model, Project)].

field_col(Field) ->
    P = fun(Name) -> binary_prop(Name, Field) end,
    col(
      <<"col-12 col-sm-6 col-xl-4 col-xxl-3">>,
      [component(
         <<"guild-field">>,
         #{label         => P("label"),
           'data-source' => P("source"),
           'data-reduce' => P("reduce"),
           'data-format' => P("format"),
           color         => P("color"),
           icon          => icon(P("icon"))})]).

icon(<<"accuracy">>) -> <<"maps:my-location">>;
icon(<<"steps">>)    -> <<"av:repeat">>;
icon(<<"time">>)     -> <<"device:access-time">>;
icon(<<"loss">>)     -> <<"av:shuffle">>;
icon(Value)          -> Value.

fields(ViewSection, Model, Project) ->
    Lookup = fields_lookup(Model),
    FieldDef = guild_project:section_attr(ViewSection, "fields"),
    fields_(FieldDef, Project, Lookup).

fields_lookup(Model) ->
    merge_lookups(
      [default_fields_lookup(),
       runtime_fields_lookup(Model)]).

default_fields_lookup() ->
    read_lookup(lookup_path("default-fields")).

runtime_fields_lookup(Model) ->
    Name = model_runtime(Model) ++ "-fields",
    read_lookup(lookup_path(Name)).

fields_({ok, Raw}, Project, Lookup) ->
    Names = parse_names(Raw),
    [resolve_field(Name, Project, Lookup) || Name <- Names];
fields_(error, _Project, _Lookup) ->
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
%% Charts
%% ===================================================================

chart_cols(ViewSection, Model, Project) ->
    {Primary, Secondary} = series(ViewSection, Model, Project),
    PrimaryCols = [chart_col(S, primary) || S <- Primary],
    SecondaryCols = [chart_col(S, secondary) || S <- Secondary],
    PrimaryCols ++ SecondaryCols.

chart_col(Series, Display) ->
    P = fun(Name) -> binary_prop(Name, Series) end,
    col(
      classes_for_chart_display(Display),
      [component(
         <<"guild-chart">>,
         #{title => P("title"),
           'data-source' => P("source"),
           label => P("label"),
           format => P("format")})]).

classes_for_chart_display(primary) -> <<"col-12">>;
classes_for_chart_display(secondary) -> <<"col-12 col-xl-6">>.

%% ===================================================================
%% Series
%% ===================================================================

series(ViewSection, Model, Project) ->
    Lookup = series_lookup(Model),
    Attr = fun(Name) -> guild_project:section_attr(ViewSection, Name) end,
    case Attr("series") of
        {ok, _}=A ->
            {series_(A, Project, Lookup), []};
        error ->
            {series_(Attr("series-a"), Project, Lookup),
             series_(Attr("series-b"), Project, Lookup)}
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

series_({ok, Raw}, Project, Lookup) ->
    Names = parse_names(Raw),
    [resolve_series(Name, Project, Lookup) || Name <- Names];
series_(error, _Project, _Lookup) ->
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
%% Compare layout
%% ===================================================================

compare_layout(ViewSection, _Model, Project) ->
    Fields = compare_fields(ViewSection, Project),
    component(<<"guild-compare-page">>, [], #{fields => Fields}).

%% ===================================================================
%% Compare fields
%% ===================================================================

compare_fields(ViewSection, Project) ->
    Runtimes = project_runtimes(Project),
    Lookups = fields_lookups_for_runtimes(Runtimes),
    FieldNames = compare_field_names(ViewSection),
    [compare_field(Name, Project, Lookups) || Name <- FieldNames].

project_runtimes(Project) ->
    project_runtimes_acc(
      guild_project:sections(Project, ["model"]),
      sets:new()).

project_runtimes_acc([Model|Rest], S) ->
    project_runtimes_acc(Rest, sets:add_element(model_runtime(Model), S));
project_runtimes_acc([], S) -> sets:to_list(S).

compare_field_names(ViewSection) ->
    Attr = fun(Name) -> guild_project:section_attr(ViewSection, Name) end,
    Names =
        guild_util:find_apply(
          [fun() -> Attr("compare") end,
           fun() -> Attr("fields") end],
          [], ""),
    parse_names(Names).

fields_lookups_for_runtimes(Runtimes) ->
    [fields_lookup_for_runtime(Runtime) || Runtime <- Runtimes].

fields_lookup_for_runtime(Runtime) ->
    merge_lookups(
      [default_fields_lookup(),
       read_lookup(lookup_path(Runtime ++ "-fields"))]).

compare_field(Name, Project, [Lookup|ExtraLookups]) ->
    Field = resolve_field(Name, Project, Lookup),
    ExtraSources = field_extra_sources(Name, ExtraLookups),
    field_to_map(apply_extra_sources(Field, ExtraSources)).

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

field_to_map(Field) ->
    maps:from_list(
      [{list_to_binary(Name), list_to_binary(Val)}
       || {Name, Val} <- Field]).

%% ===================================================================
%% TensorBoard layout
%% ===================================================================

tensorboard_layout() -> component(<<"guild-tf-page">>).

%% ===================================================================
%% Layout helpers
%% ===================================================================

container(Items) ->
    #{type => container, items => Items}.

row(Items) ->
    #{type => row, items => Items}.

col(Classes, Items) ->
    #{type => col, classes => Classes, items => Items}.

component(Name) ->
    component(Name, [], #{}).

component(Name, Attrs) ->
    component(Name, Attrs, #{}).

component(Name, Attrs, Config) ->
    #{type => component,
      name => Name,
      attrs => Attrs,
      config => Config}.

%% ===================================================================
%% Shared
%% ===================================================================

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

lookup_path(Name) ->
    filename:join(guild_app:priv_dir("viewdefs"), Name ++ ".config").

read_lookup(Path) ->
    case file:consult(Path) of
        {ok, Lookup} -> Lookup;
        {error, enoent} -> []
    end.

model_runtime(Model) ->
    {ok, Runtime} = guild_project:section_attr(Model, "runtime"),
    Runtime.

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

binary_prop(Name, Props) ->
    iolist_to_binary(proplists:get_value(Name, Props, <<>>)).
