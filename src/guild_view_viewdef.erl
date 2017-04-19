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

-module(guild_view_viewdef).

-export([viewdef/2]).

%% ===================================================================
%% Viewdef
%% ===================================================================

viewdef(Model, Project) ->
    {ok, ViewSection} = view_section(Model, Project),
    Fields = fields(ViewSection, Model, Project),
    Series = series(ViewSection, Model, Project),
    Compare = compare_fields(ViewSection, Project),
    #{
       fields => proplists_to_maps(Fields),
       series => proplists_to_maps(Series),
       compare => proplists_to_maps(Compare)
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

proplists_to_maps(Ps) ->
    [proplist_to_map(P) || P <- Ps].

proplist_to_map(P) ->
    BinVals = [{iolist_to_binary(K), iolist_to_binary(V)} || {K, V} <- P],
    %% Reverse list to preseve last added as map vals
    maps:from_list(lists:reverse(BinVals)).

%% ===================================================================
%% Fields
%% ===================================================================

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
%% Series
%% ===================================================================

series(ViewSection, Model, Project) ->
    Lookup = series_lookup(Model),
    case guild_project:section_attr(ViewSection, "series") of
        {ok, Series} -> series_(Series, Project, Lookup);
        error -> []
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

series_(Raw, Project, Lookup) ->
    Names = parse_names(Raw),
    [resolve_series(Name, Project, Lookup) || Name <- Names].

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
%% Helpers
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
    guild_project:section_attr(Model, "runtime", "tensorflow").

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
