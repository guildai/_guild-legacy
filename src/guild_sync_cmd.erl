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

-module(guild_sync_cmd).

-export([parser/0, main/2]).

-define(default_port, 6555).

%% ===================================================================
%% Parser
%% ===================================================================

parser() ->
    cli:parser(
      "guild sync",
      "[OPTION]... DEPOT",
      "Sync a project to a depot.\n"
      "\n"
      "NOTE: This is an experimental feature.\n"
      "\n"
      "DEPOT must be the path to a local Guild depot.",
      sync_options() ++ guild_cmd_support:project_options(),
      [{pos_args, 1}]).

sync_options() ->
    [{account, "-a, --account", "Depot account to sync with"},
     {preview, "--preview", "display info but do not sync", [flag]}].

%% ===================================================================
%% Main
%% ===================================================================

main(Opts, [DepotDir]) ->
    Project = guild_cmd_support:project_from_opts(Opts),
    ProjectDir = guild_project:project_dir(Project),
    Account = account(Opts),
    Env = sync_env(Opts),
    guild_app:init_support([exec]),
    exec_sync(Account, ProjectDir, DepotDir, Env).

account(Opts) ->
    Methods =
        [fun account_option/1,
         fun system_account/1],
    case guild_util:find_apply(Methods, [Opts]) of
        {ok, Account} -> Account;
        error -> account_required_error()
    end.

account_option(Opts) ->
    case proplists:get_value(account, Opts) of
        undefined -> error;
        Val -> {ok, Val}
    end.

system_account(_Opts) ->
    %% TODO: implement system wide user settings, including account
    error.

account_required_error() ->
    guild_cli:cli_error(
      "system account undefined\n"
      "Try specifying an account using the --account option.").

sync_env(Opts) ->
    lists:foldl(fun sync_env_acc/2, [], proplists:compact(Opts)).

sync_env_acc(preview, Env) -> [{"PREVIEW", "1"}|Env];
sync_env_acc(debug, Env)   -> [{"DEBUG", "1"}|Env];
sync_env_acc(_, Env)       -> Env.

exec_sync(Account, ProjectDir, DepotDir, Env) ->
    Cmd = [guild_app:priv_bin("sync"), Account, ProjectDir, DepotDir],
    Opts = [{env, Env}],
    handle_sync_exec(guild_exec:run(Cmd, Opts)).

handle_sync_exec({ok, []}) ->
    ok;
handle_sync_exec({error, [{exit_status, Status}]}) ->
    {status, Code} = exec:status(Status),
    {error, Code}.
