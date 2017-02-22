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

-module(guild_depot_util).

-export([count_tags/1, filter_projects/2]).

%% ===================================================================
%% Count tags
%% ===================================================================

count_tags(Projects) ->
    D = lists:foldl(fun count_project_tags_acc/2, dict:new(), Projects),
    dict:to_list(D).

count_project_tags_acc(#{tags:=Tags}, Acc) ->
    lists:foldl(fun count_tags_acc/2, Acc, Tags).

count_tags_acc(Tag, Acc) ->
    dict:update_counter(Tag, 1, Acc).

%% ===================================================================
%% Filter projects
%% ===================================================================

filter_projects(Ps, Opts) ->
    Filters = project_filters(Opts),
    lists:filter(fun(P) -> apply_project_filters(P, Filters) end, Ps).

apply_project_filters(P, Fs) ->
    lists:all(fun(F) -> F(P) end, Fs).

project_filters(Opts) ->
    lists:foldl(fun project_filter_acc/2, [], proplists:compact(Opts)).

project_filter_acc({text, undefined}, Acc) ->
    Acc;
project_filter_acc({text, T}, Acc) ->
    [project_text_filter(T)|Acc];
project_filter_acc({tags, []}, Acc) ->
    Acc;
project_filter_acc({tags, Tags}, Acc) ->
    [project_tags_filter(Tags)|Acc];
project_filter_acc(Other, _Acc) ->
    error({badarg, Other}).

project_text_filter(T) ->
    Tokens = tokenize_text_filter(T),
    fun(P) -> filter_project_text(P, Tokens) end.

tokenize_text_filter(T) ->
    Stripped = re:replace(T, "\\W", " ", [global]),
    re:split(Stripped, "\\s+", [{return, list}, trim]).

filter_project_text(#{name:=Name, description:=Desc}, Tokens) ->
    AllText = [string:to_lower(Name), string:to_lower(Desc)],
    lists:all(fun(T) -> match_re(AllText, T) end, Tokens).

match_re(Subject, Re) ->
    case re:run(Subject, Re, [{capture, none}]) of
        match -> true;
        nomatch -> false
    end.

project_tags_filter(Tags) ->
    fun(P) -> filter_project_tags(P, Tags) end.

filter_project_tags(#{tags:=Tags}, Filter) ->
    lists:all(fun(F) -> lists:member(F, Tags) end, Filter).
