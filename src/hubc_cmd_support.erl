-module(hubc_cmd_support).

-export([project_options/0, project_options/1, project_from_opts/1,
         project_dir_from_opts/1, project_error_msg/2,
         project_dir_desc/1, project_dir_opt/1, latest_logdir/1,
         model_section_for_name/2, exec_operation/3,
         operation_result/1, runtime_error_msg/1]).

%% ===================================================================
%% Project support
%% ===================================================================

project_options() ->
    project_options([]).

project_options(Opts) ->
    base_project_options() ++ maybe_flag_options(Opts).

base_project_options() ->
    [{project_dir, "-P, --project",
      "project directory (default is current directory)",
      [{metavar, "DIR"}]}].

maybe_flag_options(Opts) ->
    case proplists:get_bool(flag_support, Opts) of
        true -> flag_options();
        false -> []
    end.

flag_options() ->
    [{flags, "-F, --flag",
      "set a project flag; may be used multiple times",
      [{metavar, "NAME[=VAL]"}]},
     {profile, "-p, --profile",
      "use alternate flags profile",
      [{metavar, "NAME"}]}].

project_from_opts(Opts) ->
    Project = try_project_from_dir(project_dir_from_opts(Opts)),
    ProfileFlags = profile_flags(Opts, Project),
    CmdlineFlags = cmdline_flags(Opts),
    apply_flags_to_project(
      [{cmdline, CmdlineFlags},
       {profile, ProfileFlags}],
      Project).

project_dir_from_opts(Opts) ->
    proplists:get_value(project_dir, Opts, ".").

try_project_from_dir(Dir) ->
    case hubc_project:from_dir(Dir) of
        {ok, Project} -> Project;
        {error, Err}  -> hubc_cli:cli_error(project_error_msg(Err, Dir))
    end.

project_error_msg(missing_project_file, Dir) ->
    missing_project_file_msg(Dir);
project_error_msg({attr_line, Line, Num}, Dir) ->
    syntax_error_msg(Line, Num, Dir).

missing_project_file_msg(Dir) ->
    io_lib:format(
      "~s does not contain a TensorHub file~n"
      "Try 'guild init~s' to initialize a project or change directories.",
      [project_dir_desc(Dir), project_dir_opt(Dir)]).

syntax_error_msg(Line, Num, Dir) ->
    io_lib:format(
      "the TensorHub project file contains an error on line ~b~n"
      "~s:~b: syntax error: ~s~n"
      "Try editing the file or submit a report using "
      "'guild report-issue~s'",
      [Num, hubc_project:project_file_for_dir(Dir), Num, Line,
       project_dir_opt(Dir)]).

profile_flags(Opts, Project) ->
    case proplists:get_value(profile, Opts) of
        undefined -> [];
        Profile -> profile_flags_(Profile, Project)
    end.

profile_flags_(Profile, Project) ->
    case hubc_project:section(Project, ["flags", Profile]) of
        {ok, Section} -> hubc_project:section_attrs(Section);
        error -> bad_profile_error(Profile)
    end.

bad_profile_error(Profile) ->
    hubc_cli:cli_error(
      io_lib:format(
        "flags '~s' not defined for this project",
        [Profile])).

cmdline_flags(Opts) ->
    [split_flag_opt(Val) || {_, Val} <- proplists:lookup_all(flags, Opts)].

split_flag_opt(Flag) ->
    case re:split(Flag, "=", [{return, list}, {parts, 2}]) of
        [Name, Val] -> {Name, Val};
        [Name]      -> {Name, "true"}
    end.

apply_flags_to_project(FlagGroups, Project) ->
    lists:foldl(fun apply_flag_group_to_project/2, Project, FlagGroups).

apply_flag_group_to_project({Group, Flags}, Project) ->
    Grouped = [{Group, Key, Val} || {Key, Val} <- Flags],
    lists:foldl(fun apply_flag_to_project/2, Project, Grouped).

apply_flag_to_project({Group, Key, Val}, Project) ->
    hubc_project:set_attr(Project, ["flags", Group], Key, Val).

project_dir_desc(".") -> "This directory";
project_dir_desc(Dir) -> io_lib:format("Directory '~s'", [Dir]).

project_dir_opt(".") -> "";
project_dir_opt(Dir) -> io_lib:format(" --project-dir ~s", [escape_path(Dir)]).

escape_path(Path) -> re:replace(Path, " ", "\\\\ ", [global]).

%% ===================================================================
%% Latest logdir
%% ===================================================================

latest_logdir(Project) ->
    case hubc_run:runs_for_project(Project) of
        [] -> no_runs_error();
        [Latest|_] -> hubc_run:dir(Latest)
    end.

no_runs_error() ->
    hubc_cli:cli_error("There are no runs for this project").

%% ===================================================================
%% Model section for name
%% ===================================================================

model_section_for_name(undefined, Project) ->
    default_model(Project);
model_section_for_name(Name, Project) ->
    model_for_name(Name, Project).

default_model(Project) ->
    {ok, Model} =
        hubc_util:find_apply(
          [fun nameless_model/1,
           fun first_model/1,
           fun(_) -> no_models_error() end],
          [Project]),
    Model.

nameless_model(Project) ->
    hubc_project:section(Project, ["model"]).

first_model(Project) ->
    case hubc_project:sections(Project, ["model"]) of
        [M|_] -> {ok, M};
        [] -> error
    end.

no_models_error() ->
    hubc_cli:cli_error("project does not define any models").

model_for_name(Name, Project) ->
    case hubc_model:find_model_for_name(Project, Name) of
        {ok, Model} -> Model;
        error -> bad_model_error(Name)
    end.

bad_model_error(Name) ->
    hubc_cli:cli_error(
      io_lib:format("model '~s' is not defined for this project", [Name])).

%% ===================================================================
%% Exec operation
%% ===================================================================

exec_operation(Name, Op, Support) ->
    hubc_app:init_support(Support),
    {ok, Pid} = hubc_operation_sup:start_op(Name, Op),
    hubc_cmd_support:operation_result(Pid).

%% ===================================================================
%% Operation result
%% ===================================================================

operation_result(Pid) ->
    hubc_proc:reg(Pid),
    OpExit = hubc_proc:wait_for(proc, Pid),
    hubc_proc:wait_for(scope, optask),
    op_result_for_exit(OpExit).

op_result_for_exit({_, normal}) ->
    ok;
op_result_for_exit({_, {exit_status, Status}}) ->
    op_result_for_status(exec:status(Status));
op_result_for_exit({_, Err}) ->
    unexpected_op_error(Err).

op_result_for_status({status, N})          -> {ok, N};
op_result_for_status({signal, Sig, _Core}) -> signal_exit_error(Sig) .

unexpected_op_error(Err) ->
    {error, io_lib:format("unexpected error: ~p", [Err])}.

signal_exit_error(Signal) ->
    {error, io_lib:format("operation interrupted with ~p", [Signal])}.

%% ===================================================================
%% Runtime error message
%% ===================================================================

runtime_error_msg({no_runtime, Section}) ->
    io_lib:format("missing runtime definition for ~s", [Section]);
runtime_error_msg({no_runtime, Section, Name}) ->
    io_lib:format("missing runtime definition for ~s '~s'", [Section, Name]);
runtime_error_msg({runtime, Name}) ->
    io_lib:format("unsupported runtime '~s'", [Name]).
