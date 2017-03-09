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
    [#{id => <<"overview">>,
       label => <<"Overview">>,
       icon => <<"icons:dashboard">>,
       layout =>
           container(
             [row(
                [col(
                   <<"col-12">>,
                   [component(<<"guild-page-header">>)])]),
              row(
                [col(
                   <<"col-md-9">>,
                   [row(sample_fields()),
                    row(sample_charts()),
                    component(<<"guild-output">>)]),
                 col(
                   <<"col-md-3">>,
                   [component(<<"guild-status">>),
                    component(<<"guild-flags">>),
                    component(<<"guild-attrs">>)])])])
      },
     #{id    => <<"compare">>,
       label => <<"Compare">>,
       icon  => <<"icons:view-list">>
     },
     #{id    => <<"explore">>,
       label => <<"Explore">>,
       icon  => <<"icons:pageview">>
      }
    ].

container(Items) ->
    #{type => container, items => Items}.

row(Items) ->
    #{type => row, items => Items}.

col(Classes, Items) ->
    #{type => col, classes => Classes, items => Items}.

component(Name) ->
    component(Name, []).

component(Name, Attrs) ->
    #{type => component, name => Name, attrs => Attrs}.

sample_fields() ->
    [fieldCol(
       <<"Validation Accuracy">>,
       <<"98.80%">>,
       <<"blue-700">>,
       <<"maps:my-location">>),
     fieldCol(
       <<"Training Accuracy">>,
       <<"100.00%">>,
       <<"green-700">>,
       <<"maps:my-location">>),
     fieldCol(
       <<"Steps">>,
       <<"5,000,000">>,
       <<"yellow-700">>,
       <<"av_repeat">>),
     fieldCol(
       <<"Time">>,
       <<"12:31:46">>,
       <<"red-700">>,
       <<"device:access-time">>),
     fieldCol(
       <<"Loss">>,
       <<"1.231">>,
       <<"orange-700">>,
       <<"av:shuffle">>)
    ].

fieldCol(Label, Value, Color, Icon) ->
    col(
      <<"col-12 col-sm-6 col-xl-4 pb-3">>,
      [component(
         <<"guild-field">>,
         #{label => Label,
           value => Value,
           color => Color,
           icon => Icon})]).

sample_charts() ->
    [chart(<<"Loss">>, <<"loss">>),
     chart(<<"Accuracy">>, <<"accuracy">>),
     chart(<<"Process CPU %">>, <<"cpu_util">>),
     chart(<<"Process memory">>, <<"op_mem">>),
     chart(<<"GPU %">>, <<"gpu_util">>),
     chart(<<"GPU memory">>, <<"gpu_mem">>),
     chart(<<"GPU power draw">>, <<"gpu_power">>)].

chart(Title, SampleType) ->
    chart(Title, SampleType, secondary).

chart(Title, SampleType, Display) ->
    col(
      classes_for_chart_display(Display),
      [component(
         <<"guild-chart">>,
         #{title => Title,
           'sample-type' => SampleType})]).

classes_for_chart_display(primary) -> <<"col-12">>;
classes_for_chart_display(secondary) -> <<"col-12 col-xl-6">>.

project(View) ->
    guild_project_view_v2:project(View).
