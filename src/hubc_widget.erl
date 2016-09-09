-module(hubc_widget).

-export([for_name/1, css/1, js/1, template/1]).

-record(widget, {css=[], js=[], template}).

for_name("flags")         -> {ok, default_widget(hubc_flags_widget)};
for_name("output")        -> {ok, data_table_widget(hubc_output_widget)};
for_name("placeholder")   -> {ok, default_widget(hubc_placeholder_widget)};
for_name("status")        -> {ok, default_widget(hubc_status_widget)};
for_name("timeseries")    -> {ok, c3_widget(hubc_timeseries_widget)};
for_name("value-panel")   -> {ok, default_widget(hubc_value_panel_widget)};
for_name("compare-table") -> {ok, data_table_widget(hubc_compare_table_widget)};
for_name(_)               -> error.

default_widget(Template) ->
    #widget{css=["/assets/css/widgets.css"],
            js=["/assets/js/widgets.js"],
            template=Template}.

data_table_widget(Template) ->
    #widget{css=["/assets/css/datatables.min.css",
                 "/assets/css/widgets.css"],
            js=["/assets/js/datatables.min.js",
                "/assets/js/widgets.js"],
            template=Template}.

c3_widget(Template) ->
    #widget{css=["/assets/css/c3.min.css",
                 "/assets/css/widgets.css"],
            js=["/assets/js/d3.min.js",
                "/assets/js/c3.js",
                "/assets/js/widgets.js"],
            template=Template}.

css(#widget{css=CSS}) -> CSS.

js(#widget{js=JS}) -> JS.

template(#widget{template=Template}) -> Template.
