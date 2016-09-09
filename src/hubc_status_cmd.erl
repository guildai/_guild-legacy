-module(hubc_status_cmd).

-export([parser/0, main/2]).

%% ===================================================================
%% Parser
%% ===================================================================

parser() ->
    cli:parser(
      "guild status",
      "[OPTION]...",
      "Print project status.",
      hubc_cmd_support:project_options(),
      [{pos_args, 0}]).

%% ===================================================================
%% Main
%% ===================================================================

main(Opts, []) ->
    Dir = hubc_cmd_support:project_dir_from_opts(Opts),
    handle_project(hubc_project:from_dir(Dir), Dir).

handle_project({ok, _Project}, Dir) ->
    print_project_status(Dir),
    ok;
handle_project({error, Err}, Dir) ->
    print_project_error(Err, Dir),
    error.

print_project_status(Dir) ->
    hubc_cli:out(
      "~s is a TensorHub project.~n",
      [hubc_cmd_support:project_dir_desc(Dir)]).

print_project_error(missing_project_file, Dir) ->
    hubc_cli:out(
      "~s is not a TensorHub project.~n"
      "Try 'guild init~s' to create one or "
      "'guild --help' for more information.~n",
      [hubc_cmd_support:project_dir_desc(Dir),
       hubc_cmd_support:project_dir_opt(Dir)]);
print_project_error(Other, Dir) ->
    hubc_cli:out(
      "~s~n",
      [hubc_cmd_support:project_error_msg(Other, Dir)]).
