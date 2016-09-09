-module(hubc_run).

-export([new/2, id/1, dir/1, attrs/1, attr/2, attr/3, int_attr/2,
         int_attr/3, is_run/1, run_meta_dir/1, runs_for_project/1,
         runs_for_logroots/1, run_for_logdir/1, timestamp/0,
         os_pid/1, status/1]).

-record(run, {id, dir, attrs}).

-define(meta_dir_name, "run-meta.d").

%% ===================================================================
%% New
%% ===================================================================

new(Dir, Attrs) ->
    #run{id=id_for_dir(Dir), dir=Dir, attrs=Attrs}.

id_for_dir(Dir) -> erlang:crc32(Dir).

%% ===================================================================
%% Run attrs
%% ===================================================================

id(#run{id=Id}) -> Id.

dir(#run{dir=Dir}) -> Dir.

attrs(#run{attrs=Attrs}) -> Attrs.

attr(#run{attrs=Attrs}, Name) ->
    case lists:keyfind(Name, 1, Attrs) of
        {_, Val} -> {ok, Val};
        false -> error
    end.

attr(Run, Name, Default) ->
    case attr(Run, Name) of
        {ok, Val} -> Val;
        error -> Default
    end.

int_attr(Run, Name) ->
    try_convert(attr(Run, Name), fun binary_to_integer/1).

int_attr(Run, Name, Default) ->
    try_convert(attr(Run, Name), fun binary_to_integer/1, Default).

try_convert({ok, Bin}, F) ->
    try F(Bin) of Val -> {ok, Val} catch _:_ -> error end;
try_convert(error, _F) ->
    error.

try_convert(Resp, F, Default) ->
    case try_convert(Resp, F) of
        {ok, Val} -> Val;
        error -> Default
    end.

is_run(Dir) ->
    filelib:is_dir(run_meta_dir(Dir)).

run_meta_dir(LogDir) ->
    filename:join(LogDir, ?meta_dir_name).

%% ===================================================================
%% Run init
%% ===================================================================

runs_for_project(Project) ->
    runs_for_logroots(hubc_project_util:all_logroots(Project)).

runs_for_logroots(Logroots) ->
    lists:foldl(fun acc_runs/2, [], Logroots).

run_for_logdir(LogDir) ->
    MetaDir = filename:join(LogDir, ?meta_dir_name),
    case filelib:is_dir(MetaDir) of
        true -> {ok, run_for_meta_dir(MetaDir)};
        false -> error
    end.

acc_runs(LogRoot, Acc) ->
    lists:foldl(fun acc_run/2, Acc, find_runs(LogRoot)).

find_runs(LogRoot) ->
    MetaDirs = filelib:wildcard("*/" ?meta_dir_name, LogRoot),
    [filename:join(LogRoot, Dir) || Dir <- MetaDirs].

acc_run(MetaDir, Acc) ->
    [run_for_meta_dir(MetaDir)|Acc].

run_for_meta_dir(MetaDir) ->
    new(filename:dirname(MetaDir), read_attrs(MetaDir)).

read_attrs(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} -> attrs_for_files(Dir, Files);
        {error, _} -> []
    end.

attrs_for_files(Dir, Files) ->
    lists:foldl(
      fun(Name, Acc) -> acc_attr_for_file(Dir, Name, Acc) end,
      [], Files).

acc_attr_for_file(Dir, Name, Acc) ->
    case file:read_file(filename:join(Dir, Name)) of
        {ok, Bin} -> [{Name, Bin}|Acc];
        {error, _} -> Acc
    end.

timestamp() ->
    erlang:system_time(milli_seconds).

%% ===================================================================
%% OS Pid
%% ===================================================================

os_pid(Run) ->
    case attr(Run, "LOCK") of
        {ok, PidBin} ->
            Pid = binary_to_integer(PidBin),
            case hubc_util:os_pid_exists(Pid) of
                true -> {ok, Pid};
                false -> {error, {crashed, Pid}}
            end;
        error -> {error, stopped}
    end.

%% ===================================================================
%% Status
%% ===================================================================

status(Run) ->
    status_for_pid(os_pid(Run)).

status_for_pid({ok, _})               -> running;
status_for_pid({error, stopped})      -> stopped;
status_for_pid({error, {crashed, _}}) -> crashed.
