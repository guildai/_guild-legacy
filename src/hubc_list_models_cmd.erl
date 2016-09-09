-module(hubc_list_models_cmd).

-export([parser/0, main/2]).

%% ===================================================================
%% Parser
%% ===================================================================

parser() ->
    cli:parser(
      "guild list-models",
      "[OPTION]...",
      "List project models.",
      hubc_cmd_support:project_options(),
      [{pos_args, 0}]).

%% ===================================================================
%% Main
%% ===================================================================

main(Opts, []) ->
    Project = hubc_cmd_support:project_from_opts(Opts),
    print_models(hubc_project:sections(Project, ["model"])).

print_models(Models) ->
    lists:foreach(fun print_model/1, Models).

print_model(M) ->
    Name = hubc_model:name_for_project_section(M, "?"),
    hubc_cli:out(io_lib:format("~s~n", [Name])).
