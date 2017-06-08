-module(guild_op_util).

-export([python_cmd/2]).

python_cmd(CmdSpec, Flags) ->
    Python = guild_util:find_exe("python"),
    [First|Args] = guild_util:split_cmd(CmdSpec),
    Script = resolved_script_path(First),
    [Python, "-u", Script] ++ Args ++ flag_args(Flags).

resolved_script_path(Val) ->
    Checks =
        [fun explicit_path/1,
         fun path_missing_py_ext/1,
         fun unmodified_path/1],
    guild_util:find_apply2(Checks, [Val]).

explicit_path(Val) ->
    case filelib:is_regular(Val) of
        true -> {stop, Val};
        false -> continue
    end.

path_missing_py_ext(Val) -> explicit_path(Val ++ ".py").

unmodified_path(Val) -> {stop, Val}.

flag_args(Flags) ->
    lists:concat(
      [["--" ++ Name, Val]
       || {Name, Val} <- Flags, is_flag_arg(Name)]).

is_flag_arg("description") -> false;
is_flag_arg(_)             -> true.
