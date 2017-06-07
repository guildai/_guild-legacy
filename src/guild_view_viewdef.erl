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
    viewdef_for_section(view_section(Model, Project), Project).

viewdef_for_section({ok, ViewSection}, Project) ->
    Fields = fields(ViewSection, Project),
    Series = series(ViewSection, Project),
    Compare = compare_fields(ViewSection, Project),
    #{
       fields => proplists_to_maps(Fields),
       series => proplists_to_maps(Series),
       compare => proplists_to_maps(Compare)
     };
viewdef_for_section(error, _Project) ->
    #{
       fields => [],
       series => [],
       compare => []
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

fields(ViewSection, Project) ->
    Lookup = fields_lookup(),
    FieldDef = guild_project:section_attr(ViewSection, "fields"),
    fields_(FieldDef, Project, Lookup).

fields_lookup() ->
    read_lookup(lookup_path("fields")).

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

series(ViewSection, Project) ->
    Lookup = series_lookup(),
    case guild_project:section_attr(ViewSection, "series") of
        {ok, Series} -> series_(Series, Project, Lookup);
        error -> []
    end.

series_lookup() ->
    read_lookup(lookup_path("series")).

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
    Lookup = fields_lookup(),
    FieldNames = compare_field_names(ViewSection),
    [resolve_field(Name, Project, Lookup) || Name <- FieldNames].

compare_field_names(ViewSection) ->
    Attr = fun(Name) -> guild_project:section_attr(ViewSection, Name) end,
    Names =
        guild_util:find_apply(
          [fun() -> Attr("compare") end,
           fun() -> Attr("fields") end],
          [], ""),
    parse_names(Names).

%% ===================================================================
%% Helpers
%% ===================================================================

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
