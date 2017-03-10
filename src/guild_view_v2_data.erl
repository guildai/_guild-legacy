-module(guild_view_v2_data).

-export([runs/1]).

%% ===================================================================
%% Runs
%% ===================================================================

runs(RunRoots) ->
    Runs = guild_run:runs_for_runroots(RunRoots),
    format_runs(Runs).

format_runs(Runs) ->
    sort_formatted_runs([format_run(Run) || Run <- Runs]).

format_run(Run) ->
    {[
      {id, guild_run:id(Run)},
      {dir, list_to_binary(guild_run:dir(Run))},
      {status, guild_run_util:run_status(Run)}
      |format_run_attrs(guild_run:attrs(Run))
     ]}.

format_run_attrs(Attrs) ->
    [format_run_attr(Attr) || Attr <- Attrs].

format_run_attr({Name, Val}) ->
    {list_to_binary(Name), format_attr_val(Name, Val)}.

format_attr_val("started",     Bin) -> binary_to_integer(Bin);
format_attr_val("stopped",     Bin) -> binary_to_integer(Bin);
format_attr_val("exit_status", Bin) -> binary_to_integer(Bin);
format_attr_val(_Name,         Bin) -> Bin.

sort_formatted_runs(Runs) ->
    Cmp = fun(A, B) -> run_start_time(A) > run_start_time(B) end,
    lists:sort(Cmp, Runs).

run_start_time({Attrs}) ->
    case lists:keyfind(<<"started">>, 1, Attrs) of
        {_, Val} -> Val;
        false -> 0
    end.
