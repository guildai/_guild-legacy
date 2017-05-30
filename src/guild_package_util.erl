-module(guild_package_util).

-export([latest_package_path/1]).

latest_package_path(Name) ->
    PkgHome = guild_app:user_dir("pkg"),
    PathBase = filename:join(PkgHome, Name),
    case filelib:is_dir(PathBase) of
        true -> {ok, PathBase};
        false -> latest_from_path_base(PathBase)
    end.

latest_from_path_base(Base) ->
    latest_from_paths(filelib:wildcard([Base, "-*"])).

latest_from_paths([]) ->
    {error, package};
latest_from_paths(Paths) ->
    [Latest|_] = lists:reverse(lists:sort(Paths)),
    {ok, Latest}.
