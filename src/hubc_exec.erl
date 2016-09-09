-module(hubc_exec).

-export([init/0, run_link/2, send/2, stop_and_wait/2, apply_user_opts/2]).

init() ->
    application:set_env(erlexec, portexe, port_exe()),
    application:start(erlexec).

port_exe() ->
    filename:join([hubc_util:priv_dir(erlexec), system_arch(), "exec-port"]).

system_arch() ->
    erlang:system_info(system_architecture).

run_link(Args, Opts) ->
    exec:run_link(Args, Opts).

send(Pid, Bin) ->
    exec:send(Pid, Bin).

stop_and_wait(Pid, Timeout) ->
    exec:stop_and_wait(Pid, Timeout).

apply_user_opts(Opts, Acc) ->
    lists:foldl(fun maybe_user_opt/2, Acc, Opts).

maybe_user_opt({cwd, Dir}, Acc) -> [{cd, Dir}|Acc];
maybe_user_opt({env, Env}, Acc) -> [{env, Env}|Acc];
maybe_user_opt(Other, _Acc)     -> error({cmd_opt, Other}).
