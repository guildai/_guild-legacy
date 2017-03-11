-module(guild_view_v2_data_http).

-export([app/3]).

app("GET", {"/data/runs", _, _}, View) ->
    Runs = guild_view_v2:runs(View),
    JSON = guild_json:encode(Runs),
    guild_http:ok_json(JSON).
