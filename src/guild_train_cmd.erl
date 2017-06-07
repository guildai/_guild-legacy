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

-module(guild_train_cmd).

-export([parser/0, main/2]).

parser() ->
    cli:parser(
      "guild train",
      "[OPTION]... [MODEL]",
      "Trains MODEL if specified, otherwise trains the default model.\n"
      "\n"
      "The default model is the first model defined in the project config.\n"
      "\n"
      "Models are trained by their configured 'train' operation, which must "
      "be specified.\n"
      "\n"
      "If the specified model does not exist or cannot be trained because "
      "it does not define a train operation, the command exits with an error.",
      train_options() ++ guild_cmd_support:project_options([flag_support]),
      [{pos_args, {0, 1}}]).

train_options() ->
    [{preview, "--preview", "print training details but do not train", [flag]}].

%% ===================================================================
%% Main
%% ===================================================================

main(Opts, Args) ->
    train_or_preview(init_op(Opts, Args), Opts).

init_op(Opts, Args) ->
    Project = guild_cmd_support:project_from_opts(Opts),
    Model = guild_cmd_support:model_section_for_args(Args, Project),
    guild_tensorflow_runtime:init_train_op(Model, Project).

train_or_preview({ok, Op}, Opts) ->
    case proplists:get_bool(preview, Opts) of
        false -> train(Op);
        true  -> preview(Op)
    end;
train_or_preview({error, Err}, _Opts) ->
    init_op_error(Err).

init_op_error(trainable) ->
    guild_cli:cli_error(
      "model does not support a train operation\n"
      "Try 'guild train --help' for more information.");
init_op_error({missing_requires, Missing}) ->
    guild_cli:cli_error(missing_requires_error(Missing)).

missing_requires_error(Missing) ->
    io_lib:format(
      "missing required '~s'\n"
      "Do you need to run 'guild prepare' first?",
      [Missing]).

train(Op) ->
    guild_cmd_support:exec_operation(guild_train_op, Op).

preview(Op) ->
    guild_cli:out_par(
      "This command will use the settings below. Note that RUNDIR is "
      "created dynamically for new runs and will be used wherever '$RUNDIR' "
      "is used below.~n~n"),
    guild_cmd_support:preview_op_cmd(Op).
