-module(hubc_prepare_cmd).

-export([parser/0, main/2]).

parser() ->
    cli:parser(
      "guild prepare",
      "[OPTION]... [MODEL]",
      "Prepares MODEL for training if specified, otherwise prepares the "
      "default model.\n"
      "\n"
      "The default model is the first model defined in the project config.\n"
      "\n"
      "Models are prepared by their configured 'prepare' operation, if "
      "specified. Dataset associated with model or specified with the "
      "'--dataset' option are prepared first.\n"
      "\n"
      "If the specified model does not exist or cannot be prepared, either "
      "because it does not define a prepare operation or is not associated "
      "with a dataset, the command exits with an error message.",
      [{dataset, "-d, --dataset", "alternate dataset to prepare"}
       |hubc_cmd_support:project_options([flag_support])],
      [{pos_args, {0, 1}}]).

main(Opts, Args) ->
    error({'TODO', Opts, Args}).

%%     hubc_app:init_support([exec]),
%%     Project = hubc_cmd_support:project_from_opts(Opts),
%%     Model = hubc_cmd_support:model_from_args(Args, Project),
%%     Dataset = dataset_from_opts(Opts, Model, Project),
%%     prepare(Model, Dataset, Project).

%% dataset_from_opts(Opts, Model, Project) ->
%%     hubc_util:find_apply(
%%       [fun explicit_dataset/3,
%%        fun model_dataset/3],
%%       [Opts, Model, Project]).

%% explicit_dataset(Opts, _Model, Project) ->
%%     case proplists:get_value(dataset, Opts) of
%%         undefined -> error;
%%         Name -> {ok, dataset_for_name(Name, Project)}
%%     end.

%% dataset_for_name(Name, Project) ->
%%     case hubc_project:sections(Project, ["dataset", Name]) of
%%         [Dataset|_] -> Dataset;
%%         [] -> bad_dataset_error(Name)
%%     end.

%% model_dataset(_Opts, Model, Project) ->
%%     case hubc_project:section_attr(Model, "dataset") of
%%         {ok, Name} -> {ok, dataset_for_name(Name, Project)};
%%         error -> error
%%     end.

%% bad_dataset_error(Name) ->
%%     hubc_cli:cli_error(
%%       io_lib:format(
%%         "Dataset '~s' is not defined for this project\n"
%%         "Refer to the TensorHub configuration file for details.",
%%         [Name])).

%% prepare(Model, Dataset, Project) ->
%%     apply_prepare_steps(prepare_steps(Model, Dataset), Project).

%% prepare_steps(Model, Dataset) ->
%%     dataset_prepare_steps(Dataset) ++ model_prepare_steps(Model).

%% dataset_prepare_steps({ok, Dataset}) ->
%%     case hubc_project:section_attr(Dataset, "prepare") of
%%         {ok, Cmd} -> [{Cmd, Dataset}];
%%         error -> []
%%     end;
%% dataset_prepare_steps(error) -> [].

%% model_prepare_steps(Model) ->
%%     case hubc_project:section_attr(Model, "prepare") of
%%         {ok, Cmd} -> [{Cmd, Model}];
%%         error -> []
%%     end.

%% apply_prepare_steps(Steps, Project) when length(Steps) > 0 ->
%%     lists:foreach(fun(Step) -> apply_prepare_step(Step, Project) end, Steps);
%% apply_prepare_steps([], _Project) ->
%%     nothing_to_prepare_error().

%% apply_prepare_step({Cmd, Section}, Project) ->
%%     Op = init_prepare_op(Cmd, Section, Project),
%%     Result = run_prepare_op(Op),
%%     hubc_cmd_support:handle_op_result(Result).

%% init_prepare_op(Cmd, Section, Project) ->
%%     Mod = hubc_cmd_support:runtime_mod_for_section(Section),
%%     Mod:init_prepare_op(Cmd, Section, Project).

%% run_prepare_op(Op) ->
%%     {ok, Prepare} = hubc_operation_sup:start_op(Op, hubc_prepare_op),
%%     hubc_proc:reg(Prepare),
%%     hubc_proc:wait_for(proc, Prepare).

%% nothing_to_prepare_error() ->
%%     hubc_cli:cli_error(
%%       "Model does not support a prepare operation\n"
%%       "Try 'tensorhub prepare --help' for more information.").
