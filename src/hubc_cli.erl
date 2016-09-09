-module(hubc_cli).

-export([parser/0, main/1, cli_error/1, cli_error/2, out/1, out/2,
         out_par/1, out_par/2, warn/1, warn/2]).

-define(default_exit_code, 2).
-define(page_width, 79).

%% ===================================================================
%% Parser
%% ===================================================================

parser() ->
    cli:command_parser(
      "guild",
      "[OPTION]... COMMAND [ARG]...",
      "",
      global_opts(),
      parser_commands(),
      [{version, tensorhub_client:version()}]).

parser_commands() ->
    parser_commands(
      ["evaluate",
       "list-models",
       "list-runs",
       "list-run-stats",
       "prepare",
       "remove",
       "status",
       "train",
       "view"]).

parser_commands(Names) ->
    Info = [{Name, cmd_info(Name)} || Name <- Names],
    [{Name, Desc, M:parser()} || {Name, {M, Desc}} <- Info].

cmd_info("evaluate")       -> {hubc_eval_cmd, "evaluate a trained model"};
cmd_info("list-models")    -> {hubc_list_models_cmd, "list project models"};
cmd_info("list-runs")      -> {hubc_list_runs_cmd, "list project runs"};
cmd_info("list-run-stats") -> {hubc_list_run_stats_cmd, "list run stats"};
cmd_info("prepare")        -> {hubc_prepare_cmd, "prepare model for training"};
cmd_info("remove")         -> {hubc_remove_cmd, "remove a training run"};
cmd_info("status")         -> {hubc_status_cmd, "train a model"};
cmd_info("train")          -> {hubc_train_cmd, "show project status"};
cmd_info("view")           -> {hubc_view_cmd, "start TensorHub viewer"}.

%% ===================================================================
%% Main
%% ===================================================================

main({Cmd, Opts, Args}) ->
    apply_global_opts(Opts),
    {M, _Desc} = cmd_info(Cmd),
    handle_main_result(M:main(Opts, Args)).

handle_main_result(Result) ->
    hubc_proc:wait_for(scope, global),
    Result.

%% ===================================================================
%% Global opts
%% ===================================================================

global_opts() ->
    [{trace,    "--trace",    "trace a module/function", [hidden]},
     {observer, "--observer", "run observer",            [hidden, flag]}].

apply_global_opts(Opts) ->
    hubc_trace:init_from_global_opts(Opts),
    hubc_observer:maybe_start_from_global_opts(Opts).

%% ===================================================================
%% Generate CLI error
%% ===================================================================

cli_error(ExitCode) when is_integer(ExitCode) ->
    cli:main_error(ExitCode);
cli_error(Msg) ->
    cli:main_error(?default_exit_code, Msg).

cli_error(ExitCode, Msg) ->
   cli:main_error(ExitCode, Msg).

%% ===================================================================
%% Output
%% ===================================================================

out(Msg) ->
    out(Msg, []).

out(Msg, Data) ->
    io:format(user, Msg, Data).

out_par(Msg) ->
    out_par(Msg, []).

out_par(Msg, Data) ->
    Text = io_lib:format(Msg, Data),
    io:format(user, wrap(Text), []).

wrap(Text) ->
    prettypr:format(prettypr:text_par(Text), ?page_width, ?page_width).

warn(Msg) ->
    warn(Msg, []).

warn(Msg, Data) ->
    io:format(standard_error, "WARNING ", []),
    io:format(standard_error, Msg, Data).
