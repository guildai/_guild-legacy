-module(guild_view_v2_viewdef).

-export([viewdef/1]).

viewdef(Project) ->
    #{
       pages => pages(Project)
     }.

pages(_Project) ->
    default_pages().

default_pages() ->
    [#{id => <<"overview">>,
       label => <<"Overview">>,
       icon => <<"dashboard">>,
       layout => overview_layout()
      },
     #{id    => <<"compare">>,
       label => <<"Compare">>,
       icon  => <<"view-list">>
     },
     #{id    => <<"explore">>,
       label => <<"Explore">>,
       icon  => <<"pageview">>
      }
    ].

overview_layout() ->
    container(
      [row(
         [col(
            <<"col-12">>,
            [component(<<"guild-run-select-page-header">>)])]),
       row(
         [col(
            <<"col-md-9">>,
            [row(sample_fields()),
             row(sample_charts()),
             component(<<"guild-output">>)]),
          col(
            <<"col-md-3">>,
            [component(<<"guild-flags">>),
             component(<<"guild-attrs">>)])])]).

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
       <<"series/tf/validation/accuracy">>,
       <<"last">>,
       <<"0.00%">>,
       <<"blue-700">>,
       <<"maps:my-location">>),
     fieldCol(
       <<"Training Accuracy">>,
       <<"series/tf/train/accuracy">>,
       <<"last">>,
       <<"0.00%">>,
       <<"green-700">>,
       <<"maps:my-location">>),
     fieldCol(
       <<"Steps">>,
       <<"series/tf/validation/accuracy">>,
       <<"steps">>,
       <<"0,0">>,
       <<"yellow-700">>,
       <<"av:repeat">>),
     fieldCol(
       <<"Time">>,
       <<"series/tf/validation/accuracy">>,
       <<"duration">>,
       <<"00:00:00">>,
       <<"red-700">>,
       <<"device:access-time">>),
     fieldCol(
       <<"Loss">>,
       <<"series/tf/train/loss">>,
       <<"last">>,
       <<"0.000">>,
       <<"orange-700">>,
       <<"av:shuffle">>)
    ].

fieldCol(Label, Source, Reduce, Format, Color, Icon) ->
    col(
      <<"col-12 col-sm-6 col-xl-4 pb-3">>,
      [component(
         <<"guild-field">>,
         #{label => Label,
           'data-source' => Source,
           'data-reduce'=> Reduce,
           'data-format'=> Format,
           color => Color,
           icon => Icon})]).

sample_charts() ->
    [chart(<<"Loss">>, <<"series/tf/.+/loss">>),
     chart(<<"Accuracy">>, <<"series/tf/.+/accuracy">>),
     chart(<<"Process CPU %">>, <<"series/op/cpu/util">>),
     chart(<<"Process memory">>, <<"series/op/mem/rss">>),
     chart(<<"GPU %">>, <<"series/sys/gpu.+/gpu/util">>),
     chart(<<"GPU memory">>, <<"series/sys/gpu.+/mem/used">>),
     chart(<<"GPU power draw">>, <<"series/sys/gpu.+/powerdraw">>)].

chart(Title, Source) ->
    chart(Title, Source, secondary).

chart(Title, Source, Display) ->
    col(
      classes_for_chart_display(Display),
      [component(
         <<"guild-chart">>,
         #{title => Title,
           'data-source' => Source})]).

classes_for_chart_display(primary) -> <<"col-12">>;
classes_for_chart_display(secondary) -> <<"col-12 col-xl-6">>.
