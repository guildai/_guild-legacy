-module(guild_package_util).

-export([latest_package_path/1]).

latest_package_path(Name) ->
    PkgHome = guild_app:user_dir("packages"),
    Glob = filename:join(PkgHome, [Name, "-*"]),
    case filelib:wildcard(Glob) of
        [] -> error;
        Matches -> {ok, latest_package_path(Matches, PkgHome)}
    end.

latest_package_path(Paths, Dir) ->
    Names = [filename:basename(Path) || Path <- Paths],
    [Latest|_] = lists:reverse(lists:sort(Names)),
    filename:join(Dir, Latest).
