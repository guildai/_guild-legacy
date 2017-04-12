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

-module(guild_dtl).

-export([init/1, render/2]).

-define(templates,
        [{"bye.html",                  guild_bye_page},
         {"project-error.html",        guild_project_error_page},
         {"project-view.html",         guild_project_view_page},
         {"view-index.html",           guild_view_index_page}
        ]).

%% ===================================================================
%% Init
%% ===================================================================

init(Opts) ->
    compile_templates(),
    guild_app:set_env(recompile_dtl, recompile_templates_opt(Opts)).

recompile_templates_opt(Opts) ->
    proplists:get_bool(recompile_templates, Opts).

compile_templates() ->
    lists:foreach(fun compile_template/1, ?templates).

compile_template({Name, Module}) ->
    File = template_file(Name),
    Opts = [{default_libraries, [guild_dtl_lib]}],
    guild_dtl_util:compile_template(File, Module, Opts).

template_file(Name) ->
    filename:join(guild_app:priv_dir("dtl"), Name).

%% ===================================================================
%% Render
%% ===================================================================

render(Module, Vars) ->
    maybe_recompile(Module),
    case Module:render(Vars) of
        {ok, Page} -> Page;
        {error, Err} -> error({render, Module, Vars, Err})
    end.

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
