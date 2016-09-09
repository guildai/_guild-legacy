-module(hubc_template).

-export([init/1, render/2]).

-define(templates,
        [{"bye.html",                  hubc_bye_page},
         {"compare-table-widget.html", hubc_compare_table_widget},
         {"flags-widget.html",         hubc_flags_widget},
         {"output-widget.html",        hubc_output_widget},
         {"placeholder-widget.html",   hubc_placeholder_widget},
         {"project-compare.html",      hubc_project_compare_page},
         {"project-index.html",        hubc_project_index_page},
         {"status-widget.html",        hubc_status_widget},
         {"timeseries-widget.html",    hubc_timeseries_widget},
         {"value-panel-widget.html",   hubc_value_panel_widget}
        ]).

init(Opts) ->
    compile_templates(),
    hubc_app:set_env(recompile_templates, recompile_templates_opt(Opts)).

recompile_templates_opt(Opts) ->
    proplists:get_bool(recompile_templates, Opts).

compile_templates() ->
    lists:foreach(fun compile_template/1, ?templates).

compile_template({Name, Module}) ->
    File = template_file(Name),
    Opts =
        [report,
         {out_dir, hubc_app:tmp_dir()},
         {default_libraries, [hubc_template_lib]}],
    handle_compile(erlydtl:compile_file(File, Module, Opts), File).

template_file(Name) ->
    filename:join(hubc_app:priv_dir("templates"), Name).

handle_compile({ok, _}, _File) -> ok;
handle_compile(error, File) ->
    error({template_compile, File}).

render(Module, Vars) ->
    maybe_recompile(recompile_templates(), Module),
    case Module:render(Vars) of
        {ok, Page} -> Page;
        {error, Err} -> error({render, Module, Vars, Err})
    end.

recompile_templates() ->
    hubc_app:get_env(recompile_templates, false).

maybe_recompile(true, Module) ->
    compile_template(template_for_module(Module));
maybe_recompile(_, _Module) ->
    ok.

template_for_module(Module) ->
    case lists:keyfind(Module, 2, ?templates) of
        {_, _}=T -> T;
        false -> error({no_such_template, Module})
    end.
