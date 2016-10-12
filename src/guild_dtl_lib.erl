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

-module(guild_dtl_lib).

-behavior(erlydtl_library).

-export([version/0, inventory/1]).

-export([render_viewdef/1, viewdef_css/1, viewdef_js/1,
         resolve_value/1, format_value/2, resolve_icon_alias/1,
         navbar_links/1, navbar_item_active_class/2,
         navbar_item_link/2, render_page_view/2]).

version() -> 1.

inventory(filters) ->
    [render_viewdef,
     viewdef_css,
     viewdef_js,
     resolve_value,
     format_value,
     resolve_icon_alias,
     navbar_links,
     navbar_item_active_class,
     navbar_item_link,
     render_page_view];
inventory(tags) ->
    [].

%% ===================================================================
%% Render viewdef
%% ===================================================================

render_viewdef(Def) ->
    Rows = proplists:get_value(rows, Def, []),
    [render_viewdef_row(Cols) || {row, Cols} <- Rows].

render_viewdef_row(Cols) ->
    ["<div class=\"row\">",
     [render_viewdef_col(Attrs, Items) || {col, Attrs, Items} <- Cols],
     "</div>"].

render_viewdef_col(Attrs, Items) ->
    Class = proplists:get_value(class, Attrs, ""),
    ["<div class=\"", Class, "\">",
     [render_viewdef_col_item(Item) || Item <- Items],
     "</div>"].

render_viewdef_col_item({row, Cols}) ->
    render_viewdef_row(Cols);
render_viewdef_col_item({WidgetName, Attrs}) ->
    render_widget(WidgetName, apply_widget_uid(Attrs)).

apply_widget_uid(Attrs) ->
    Uid = erlang:phash2(erlang:make_ref(), 100000000),
    [{uid, Uid}|Attrs].

render_widget(Name, Attrs) ->
    try_render_widget(guild_widget:for_name(Name), Attrs, Name).

try_render_widget({ok, W}, Attrs, _Name) ->
    Template = guild_widget:template(W),
    guild_dtl:render(Template, Attrs);
try_render_widget(error, _Attrs, Name) ->
    ["ERROR: unknown widget '", Name, "'"].

%% ===================================================================
%% Viewdef CSS / JS
%% ===================================================================

viewdef_css(Def) ->
    widget_resources(fun guild_widget:css/1, widgets(Def)).

viewdef_js(Def) ->
    widget_resources(fun guild_widget:js/1, widgets(Def)).

widgets(Def) ->
    widgets_acc(Def, []).

widgets_acc([{rows, Rows}|Rest], Acc) ->
    widgets_acc(Rest, widgets_acc(Rows, Acc));
widgets_acc([{row, Cols}|Rest], Acc) ->
    widgets_acc(Rest, widgets_acc(Cols, Acc));
widgets_acc([{col, _Attrs, Children}|Rest], Acc) ->
    widgets_acc(Rest, widgets_acc(Children, Acc));
widgets_acc([{Name, _, _}|Rest], Acc) ->
    widgets_acc(Rest, try_apply_widget(Name, Acc));
widgets_acc([{Name, _}|Rest], Acc) ->
    widgets_acc(Rest, try_apply_widget(Name, Acc));
widgets_acc([], Acc) ->
    Acc.

try_apply_widget(Name, Acc) ->
    case guild_widget:for_name(Name) of
        {ok, W} -> [W|Acc];
        error -> Acc
    end.

widget_resources(Get, Widgets) ->
    widget_resources_acc(Get, Widgets, []).

widget_resources_acc(Get, [W|Rest], Acc) ->
    widget_resources_acc(Get, Rest, apply_widget_resources(Get(W), Acc));
widget_resources_acc(_Get, [], Acc) ->
    lists:reverse(Acc).

apply_widget_resources([R|Rest], Acc) ->
    case lists:member(R, Acc) of
        true  -> apply_widget_resources(Rest, Acc);
        false -> apply_widget_resources(Rest, [R|Acc])
    end;
apply_widget_resources([], Acc) ->
    Acc.

%% ===================================================================
%% Resolve value
%% ===================================================================

resolve_value(Value) ->
    try_funs(
      [fun list_to_float/1,
       fun list_to_integer/1],
      Value).

try_funs([F|Rest], Arg) ->
    try
        F(Arg)
    catch
        _:_ -> try_funs(Rest, Arg)
    end;
try_funs([], Arg) -> Arg.

try_fun(F, Arg) -> try_funs([F], Arg).

%% ===================================================================
%% Format value
%% ===================================================================

format_value(Val, undefined) ->
    Val;
format_value(Val, "percent") ->
    format_percent(Val);
format_value(Val, "duration") ->
    format_duration(Val);
format_value(Val, "number") ->
    format_number(Val);
format_value(Val, Format) ->
    try_fun(fun(_) -> io_lib:format(Format, [Val]) end, Val).

format_percent(F) when is_float(F) ->
    io_lib:format("~.2f%", [F * 100]);
format_percent(Other) ->
    Other.

format_duration(Seconds) when is_integer(Seconds), Seconds >= 0 ->
    [D, H, M, S] = split_duration(Seconds, [86400, 3600, 60]),
    format_duration_parts([{D, "d"}, {H, "h"}, {M, "m"}, {S, "s"}]);
format_duration(Other) ->
    Other.

split_duration(D, Parts) -> split_duration_acc(D, Parts, []).

split_duration_acc(D, [Part|Rest], Acc) ->
    PartD = D div Part,
    split_duration_acc(D - PartD * Part, Rest, [PartD|Acc]);
split_duration_acc(D, [], Acc) ->
    lists:reverse([D|Acc]).

format_duration_parts([{0, _}|Rest]) ->
    format_duration_parts(Rest);
format_duration_parts([]) ->
    "0s";
format_duration_parts(Parts) ->
    FormattedParts = [format_duration_part(Part) || Part <- Parts],
    string:join(FormattedParts, " ").

format_duration_part({N, Unit}) ->
    io_lib:format("~b~s", [N, Unit]).

format_number(F) when is_float(F) ->
    io_lib:format("~.2f", [F]);
format_number(I) when is_integer(I) ->
    separate_thousands(I);
format_number(Other) ->
    Other.

separate_thousands(I) ->
    %% TODO: sep needs to be i18n
    string:join(split_thousands(I), ",").

split_thousands(I) -> split_thousands_acc(I, []).

split_thousands_acc(I, Acc) ->
    Part = I rem 1000,
    case I div 1000 of
        0 -> apply_last_thousands_part(Part, Acc);
        N -> split_thousands_acc(N, apply_thousands_part(Part, Acc))
    end.

apply_last_thousands_part(N, Acc) ->
    [integer_to_list(N)|Acc].

apply_thousands_part(N, Acc) ->
    [io_lib:format("~3..0b", [N])|Acc].

%% ===================================================================
%% Resolve icon alias
%% ===================================================================

resolve_icon_alias("accuracy") -> "bullseye";
resolve_icon_alias("steps")    -> "retweet";
resolve_icon_alias("time")     -> "clock-o";
resolve_icon_alias("loss")     -> "random";
resolve_icon_alias(Value)      -> Value.

%% ===================================================================
%% Nabvar support
%% ===================================================================

navbar_links(Viewdef) ->
    Navbar = proplists:get_value(navbar, Viewdef, []),
    [Attrs || {link, Attrs} <- Navbar].

navbar_item_active_class(Item, {Active, _}) ->
    case proplists:get_value(view, Item) of
        Active -> "active";
        _ -> ""
    end.

navbar_item_link(Item, Params) ->
    View = proplists:get_value(view, Item, ""),
    case proplists:get_value("run", Params) of
        undefined -> "/" ++ View;
        RunId -> "/" ++ View ++ "?" ++ RunId
    end.

%% ===================================================================
%% Render page view
%% ===================================================================

render_page_view({_Name, View}, Context) ->
    [render_view_row(Row, Context) || {row, Row} <- View].

render_view_row(Cols, Context) ->
    ["<div class=\"row\">",
     [render_view_col(Attrs, Items, Context) || {col, Attrs, Items} <- Cols],
     "</div>"].

render_view_col(Attrs, Items, Context) ->
    ["<div class=\"", proplists:get_value(class, Attrs, ""), "\">",
     [render_view_col_item(Item, Context) || Item <- Items],
     "</div>"].

render_view_col_item({row, Cols}, Context) ->
    render_view_row(Cols, Context);
render_view_col_item({widget, WidgetName, Attrs}, Context) ->
    AllAttrs = apply_widget_uid(Attrs ++ Context),
    Vars = proplists:unfold(AllAttrs),
    render_widget(WidgetName, Vars).
