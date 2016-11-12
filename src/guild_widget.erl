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

-module(guild_widget).

-export([for_name/1, css/1, js/1, template/1]).

-record(widget, {css=[], js=[], template}).

for_name("attrs")         -> {ok, default_widget(guild_attrs_widget)};
for_name("compare-table") -> {ok, data_table_widget(guild_compare_table_widget)};
for_name("flags")         -> {ok, default_widget(guild_flags_widget)};
for_name("output")        -> {ok, data_table_widget(guild_output_widget)};
for_name("page-header")   -> {ok, select_widget(guild_page_header_widget)};
for_name("placeholder")   -> {ok, default_widget(guild_placeholder_widget)};
for_name("run-model")     -> {ok, default_widget(guild_run_model_widget)};
for_name("status")        -> {ok, default_widget(guild_status_widget)};
for_name("timeseries")    -> {ok, c3_widget(guild_timeseries_widget)};
for_name("value-panel")   -> {ok, default_widget(guild_value_panel_widget)};
for_name(_)               -> error.

default_widget(Template) ->
    #widget{css=["/assets/css/widgets.css"],
            js=["/assets/js/numeral.min.js",
                "/assets/js/widgets.js"],
            template=Template}.

data_table_widget(Template) ->
    #widget{css=["/assets/css/datatables.min.css",
                 "/assets/css/widgets.css"],
            js=["/assets/js/datatables.min.js",
                "/assets/js/numeral.min.js",
                "/assets/js/d3.min.js",
                "/assets/js/widgets.js"],
            template=Template}.

c3_widget(Template) ->
    #widget{css=["/assets/css/c3.min.css",
                 "/assets/css/widgets.css"],
            js=["/assets/js/d3.min.js",
                "/assets/js/c3.js",
                "/assets/js/numeral.min.js",
                "/assets/js/widgets.js"],
            template=Template}.

select_widget(Template) ->
    #widget{css=["/assets/css/bootstrap-select.min.css",
                 "/assets/css/widgets.css"],
            js=["/assets/js/bootstrap-select.min.js",
                "/assets/js/widgets.js"],
            template=Template}.

css(#widget{css=CSS}) -> CSS.

js(#widget{js=JS}) -> JS.

template(#widget{template=Template}) -> Template.
