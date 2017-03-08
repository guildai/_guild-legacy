-module(guild_view_v2_globals).

-export([render/1]).

render(View) ->
    Globals = globals(View),
    [<<"var GUILD=">>, guild_json:encode(Globals), <<";">>].

globals(View) ->
    #{
       viewdef => viewdef(View),
       project => project(View)
     }.

viewdef(View) ->
    default_viewdef(View).

default_viewdef(_View) ->
    #{
       pages => default_pages()
     }.

default_pages() ->
    [#{
        id    => <<"overview">>,
        label => <<"Overview">>,
        icon  => <<"icons:dashboard">>
      },
     #{
      id    => <<"compare">>,
      label => <<"Compare">>,
      icon  => <<"icons:view-list">>
     },
     #{
        id    => <<"explore">>,
        label => <<"Explore">>,
        icon  => <<"icons:pageview">>
      }
    ].

project(View) ->
    guild_project_view_v2:project(View).
