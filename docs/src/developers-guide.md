---
layout: docs
title: Developers guide
title_class: hidden
description: Details on customizing Guild
group: developers-guide
---

Many Guild AI features may be customized, either by end-users or by
Guild AI contributors. Refer to the sections below for details on its
extension points.

## Contents

* Will be replaced with the ToC
{:toc}

## Custom views

{% note %}

This section describes Guild AI's low level view specification. To
customize the fields and time series charts displayed in default view,
refer to the following project reference sections:

- [Views](/project-reference/#views)
- [Fields](/project-reference/#fields-1)
- [Series](/project-reference/#series)

{% endnote %}

{% note %}

This is not a formal specification of Guild's view definitions. Use it
as a high level guide and as background information. For help with a
specific problem, open an issue
on {% ref github-issues %}.

{% endnote %}

The UI displayed
by [Guild View](/user-guide/guild-view/) is defined by
*view configuration files*. By default Guild View
uses
[`default.config`](https://github.com/guildai/guild/blob/master/priv/viewdefs/default.config) (located
in the Guild program install location).

The default view defines three pages (displayed in a navigation bar):

- Train
- Compare
- Serve

Each page is defined as a `view` element. Views consist of `row`
elements, which in turn consist of `col` elements (columns). Columns
in turn consist of `row` elements and `widget` elements. Widgets are
used to to display information to or otherwise interact with the user.

The default view is a Guild *template* that is used to generate live
views --- i.e. the views you see when you run `guild view` for a
particular project. Live views are generated using the template and
the contents of the
project
. The
default view displays fields and series defined for the project.

To create a custom view for your project, modify the default view to
suit your requirements and include it in your project. You may
reference the view config for each of the models it applies to. For
example, if your view is named `my_custom_view.config` and is located
in your project directory, associate with the default model by
modifying the {% ref guild-project-file %} as follows:

{% code %}
[model]

view = my_custom_view
{% endcode %}

{% note %}

Custom views do not support template tags and do not generate live
views. To create a custom view, provide explicit widget definitions
applicable to your project.

{% endnote %}

## Widgets

Widgets are used to render UI elements and handle user interactions in
Guild
View. Guild's
[default view](https://github.com/guildai/guild/blob/master/priv/viewdefs/default.config) defines
the pages and widgets used when a custom view is not provided. To
customize the layout and widgets for your project, create
a [custom view](#custom-views).

Widget are defined in a custom view using this syntax:

{% code erlang %}
{widget, NAME, [{ATTR_NAME, ATTR_VAL}, ...]}
{% endcode %}

`NAME` must be the name of a supported widget (see list below).

`ATTR_NAME` and `ATTR_VAL` define an attribute for the widget. Refer
to each widget section below for a list of supported attributes.

Guild provides the following widgets:

- [attrs](#attrs)
- [compare-table](#compare-table)
- [flags](#flags)
- [output](#output)
- [page-header](#page-header)
- [placeholder](#placeholder)
- [run-model](#run-model)
- [status](#status)
- [time-series](#time-series)
- [value-panel](#value-panel)

{% note %}

Creating custom widgets is a straight forward process but currently
involves modification to Guild source. This process will be simplified
in future releases of Guild AI. If you have an idea for a new widget
please let us know by opening an issue
on {% ref github-issues %}.

{% endnote %}

### attrs

Displays the attributes associated with a run.

*This widget does not have any attributes.*

### compare-table

Displays field summaries for the runs in a project.

Attributes:

| **columns** | List of columns to display in the table. |

Each column is a list of attributes in this format:

{% code erlang %}
[{ATTR_NAME, ATTR_VAL}, ...]
{% endcode %}

Column attributes:

| **title**     | Column title |
| **sources**   | Comma separated list of applicable sources for the column |
| **reduce**    | [Reduce function](/project-reference/#reduce) to apply to the applicable source |
| **attribute** | Name of the attribute to display, if the applicable source returns an attribute list |
| **format**    | [Format](/project-reference/#format) of the displayed value |

{% note %}

The compare table uses *sources* rather than *source* for a column
because it must handle values from different models, each of which may
log values using different source paths. The compare table will
display the value from the first source it finds in *sources* (the
search order is undefined).

{% endnote %}

### flags

Displays the flags associated with a run.

*This widget does not have any attributes.*

{% note %}

The `description` flag is a special case --- it is used as the table
caption and not listed with the other flags.

{% endnote %}

### output

Displays the output associated with a run.

*This widget does not have any attributes.*

### page-header

Displays the currently selected run and optionally provides a run
select dropdown list.

Attributes:

| **run_select** | Optional value indicating whether or not to display the run select dropdown. Legal values are `true` or `false` (default). |

### placeholder

Displays text --- used during custom view layout development.

Attributes:

| **value** | Text to display |

### run-model

Multi-component widget used to run widgets. This widget provides a
number of UI features:

- Text area for model input
- JSON formatted model output
- Tables listing model input and output tensors
- Button for running the model

*This widget does not have any attributes.*

### status

Displays a run's current status.

*This widget does not have any attributes.*

### time-series

Displays a single chart of multiple time series.

Attributes:

| **title** | Chart title |
| **label** | Chart y-axis label |
| **format** | [Format](/project-reference/#format-1) for time series values |
| **source** | [Source](/project-reference/#source-1) used for time series |

Source values are regular expressions that may match multiple sources.

### value-panel

Displays a single value in a panel with an icon and label.

Attributes:

| **color** | Panel [color](/project-reference/#color) |
| **icon** | Panel [icon](/project-reference/#icon) |
| **label** | Label displayed for the value |
| **source** | Value [source](/project-reference/#source) |
| **attribute** | Name of the attribute to display if source resolves to a list of attributes |
| **reduce** | [Reduce function](/project-reference/#reduce) to apply to time series to calculate a value |
| **format** | [Format](//project-reference/#format) used to display the value |

## Runtimes

*Runtimes* define the logic associated with a particular runtime
environment. Guild AI currently supports the following runtimes:

- TensorFlow

The following runtimes are under development:

- TFLearn

Runtimes consist of a number of code artifacts shipped with Guild
AI. If you're interested in using Guild AI with an unsupported
environment, please open an issue on {% ref github-issues %}.
