---
layout: docs
title: Project reference
title_class: hidden
description: Details on the Guild project file
group: project-reference
---

Guild project files contain information about your project that lets
Guild perform various operations.

## Contents

* Will be replaced with the ToC
{:toc}

## Overview

A Guild project file is named `Guild` and located in the root of the
project directory. Guild commands that apply to projects will use the
closest Guild file from the current directory, or the directory
provided with the `--project` option.

Guild files are extended {% link
https://en.wikipedia.org/wiki/INI_file%}INI files{% endlink %}. They
are organized in sections, each section having an optional
name. Sections contain attributes, which are named values.

Here's a simple Guild file with a single `model` and `flags` section.

{% code %}
[project]

name = My Sample Project

[model]

runtime = tensorflow
train = train

[flags]

rundir = $RUNDIR
{% endcode %}

A name can be used to further identifies a section. Here's a section
for a model named "expert":

{% code %}
[model "expert"]

runtime = tensorflow
train = train_expert
{% endcode %}

## Project header

Each project may contain at most one `project` section.

### Project attributes

#### name

The project name is used by Guild View whenever identifying the project.

If a project name is not specified, Guild will infer a name from the
project directory.

#### runroot

This value indicates where runs are stored. Paths are relative to the
project root. The default value is `runs`.

## Models

A project may contain one or more `model` sections. If a project
contains more than one model section, subsequent sections must be
named, though it's good practice to name each of the sections for
clarity.

The following Guild operations apply to models:

- [prepare](/user-guide/model-operations/#prepare)
- [train](/user-guide/model-operations/#prepare)
- [evaluate](/user-guide/model-operations/#prepare)

If a model is not named explicitly in an operation, Guild assumes the
operation applies to the first model defined in the project.

The primary operation applied to models is `train`. In some cases a
model may define a `train_requires` attribute, which specifies one or
more files that must exist before the model can be trained. Required
files are typically created during a `prepare` operation if they don't
already exist.

### Runtimes

Each model must define a `runtime` attribute. Runtimes tell Guild how
to perform the various operations defined by the model. Runtimes may
also specify how a trained model is served and how fields/series are
resolved.

Currently Guild only supports the `tensorflow` runtime. Support for
other runtimes will be added in future releases.

### Command specs

Attributes that define operations (e.g. *prepare*, *train*,
*evaluate*) do so using *command specs*. The syntax of a command spec
is specific to the runtime.

The TensorFlow runtime expects command specs to consist of a Python
module with zero or more command options. For example, to execute a
locally defined `train.py` module with the arguments `--datadir
./data`, the command spec would be:

{% code %}
train --datadir ./data
{% endcode %}

{% note %}
Most commands are single module names without options. Options are
typically provided via flags, [described below](#flags).
{% endnote %}

### Model attributes

Model attributes are typically defined in this order:

| [description](#description) | optional, used for documentation only |
| [runtime](#runtime) | required |
| [prepare](#prepare) | optional, if model supports a prepare operation |
| [train](#train)     | typically defined |
| [train_requires](#train_requires) | optional, used as a check before training, typically used with prepare |
| [evaluate](#evaluate) | optional, if model supports an evaluate operation |

Refer to the applicable section below for details.

#### description

Guild AI currently does not use this attribute, but it is useful
document a model with a short description, particularly if a project
has more than one model.

{% note %}

Future releases of Guild AI will make use of this attribute.

{% endnote %}

#### runtime

The `runtime` attribute is required for all models. Currently Guild AI
only supports the `tensorflow` runtime.

#### prepare

The `prepare` attribute is a command spec used to prepare a model for
training. Prepare operations are typically run once, whereas train
operations are run repeatedly.

Prepare is typically used to download and pre-process data used in
training and evaluating a model.

#### train

The `train` attribute is a command spec used to train a model. While
`train` is optional, most models will provide a value.

#### train_requires

`train_requires` is a UNIX style file pattern that is used to check
for files prior to training. This is a safe guard to prevent train
operations on unprepared models.

For example, if `prepare` is not first run on the {% ref mnist-example
%} the user will see this message when she runs `train`:

{% term %}
$ guild train
guild train: missing required './data'
Do you need to run 'guild prepare' first?
{% endterm %}

Here's the model configuration for this example:

{% code %}
[model]

runtime         = tensorflow
prepare         = intro --prepare
train           = intro
train_requires  = ./data
{% endcode %}

#### evaluate

The `evaluate` attribute is a command spec used to evaluate a trained
model. Because evaluating a model requires a run it's common to
include the `$RUNDIR` argument to the command. This may be provided
as a [flag](#flags) or specified directly in the command spec.

## Flags

Flags serve an important function in Guild operations --- they are
passed as command options to model operations *prepare*, *train*, and
*evaluate*. For example, if the flag `datadir` is defined as `./data`
for a train operation, the train command will be called with the
additional argument:

{% term %}
--datadir=./data
{% endterm %}

In this way flags are used to parameterize Guild operations.

Flags may be defined in two ways: as *project wide flags* or as *named
flags*. Project wide flags are defined in an unnamed `flags`
section. For example, his section defines three flags that apply
throughout a project:

{% code %}
[flags]

rundir  = $RUNDIR
datadir = ./data
epochs  = 200
{% endcode %}

A named flags section is associated with named models and
resources. This sample section redefines the `epochs` flag, associating
it with the name "long-train":

{% code %}
[flags "long-train"]

epochs  = 2000
{% endcode %}

Flags are applied in an order of precedence, each level adding or
redefining flag values as provided:

1. `-F, --flag` command line options
2. Flag sections applied using the `--profile` command line option
3. Named flags corresponding to the model or resource name
4. Project wide flags (i.e. unnamed flags section)

Let's consider a project with two models, *intro* and *expert*, and
two flag sections --- one project wide (unnamed) and another named
"expert":

{% code %}
[model "intro"]

train = train_intro

[model "expert"]

train = train_expert

[flags]

epochs = 100

[flags "expert"]

epochs = 200
{% endcode %}

Using the `--preview` option we can see what command Guild will run
under various scenarios.

{% term %}
$ guild train --preview

  python -m train_intro --epochs=100

$ guild train expert --preview

  python -m train_expert --epochs=200

$ guild train intro --profile expert

  python -m train_intro --epochs=200

$ guild train intro --flag epochs=300

  python -m train_intro --epochs=300
{% endterm %}

{% note %}

Guild includes all applicable flags to an operation command, whether
the command expects or requires the flag or not. For this reason,
command line parsers should use this pattern when parsing arguments:

{% code python %}
parser = argparse.ArgumentParser()
# calls to parser.add_argument
FLAGS, _ = parser.parse_known_args()
{% endcode %}

Note the use of `parse_know_args` rather than `parse_args`. This form
tolerates additional flags that Guild may provide but that aren't
defined for the script.
{% endnote %}

{% insight %}
Flags are a useful construct in Guild --- they are used to define sets
of parameterized inputs to your model operations. This documents the
set of inputs to various operations under different conditions and
reduces the chances of user error when running commands.
{% endinsight %}

## Views

View sections define how Guild View displays training results for
models.

A project must define a `view` section for Guild View to display
something for the project.

View attributes are used for the following:

- Train fields (defined by the `fields` attribute)
- Train series (defined by the `series`, `series-a`, and `series-b` attributes)
- Compare fields (defined by the `compare` attributes)

Here's view defined for the {% ref mnist-example %}.

{% code %}
[view]

fields           = validation-accuracy train-accuracy steps time

series-b         = loss accuracy op-cpu-percent op-memory \
                   gpu-percent gpu-memory gpu-powerdraw

compare          = validation-accuracy train-accuracy batch-size-flag \
                   steps time
{% endcode %}

Each view attribute is a space delimited list of fields or series to
display in the view.

For details about each attribute, refer to the sections below.

### View attributes

#### fields

The `fields` attribute is a list of fields that should appear at the
top of the **Train** page in Guild View. Each field name maps to
either a built in field, or a custom field defined for the project.

For more information on fields, refer to [Fields](#fields-1) below.

#### series, series-a, series-b

The series attributes define the time series charts that should appear
in the view.

`series` may be used as a single list of series or series may be split
between `series-a` and `series-b`.

`series` and `series-a` both display full width charts at the top of
the view.

`series-b` displays half width charts. `series-b` charts always follow
charts defined by `series` and `series-a`.

For more information on series, refer to [Series](#series) below.

#### compare

The `compare` attribute defined a list of fields to use in the Guild
View **Compare** page.

If `compare` is not provided, the value for `fields` (see above) will be used.

For more information on fields, refer to [Fields](#fields-1) below.

## Fields

Fields are used in Guild View to summarize run results. They're
displayed as values at the top of the **Train** page. For example:

{% screen guild-view-screen-5.png %}

They are also used as table values in the **Compare** page.

{% screen guild-view-screen-11.png %}

Guild defines a number of fields that may be referenced by name. Each
named field implicitly defines a number of attributes.

For details on Guild's predefined fields, refer to:

- [default-fields.config](https://github.com/guildai/guild/blob/master/priv/viewdefs/default-fields.config) (base field attributes)
- [tensorflow-fields.config](https://github.com/guildai/guild/blob/master/priv/viewdefs/tensorflow-fields.config) (TensorFlow specific attributes)

Field attributes can be defined, either as new fields, or as modified
fields, using field sections. Here's a field definition that defined
attributes for a field "accuracy":

{% code %}
[field "accuracy"]

color   = green-700
icon    = accuracy
label   = Validation Accuracy
source  = series/tf/validation/accuracy
reduce  = last
format  = 0.00%
{% endcode %}

### Field attributes

Field attributes are used to display values in Guild View:

| [source](#source) | reference to an underlying series or other Guild data source |
| [reduce](#reduce) | function that reduces a series to a single value |
| [format](#format) | format for displaying the field value |
| [label](#label)   | label for the field |
| [color](#color)   | field background color, where applicable |
| [icon](#icon)     | field icon, where applicable |

For details, refer to the field attributes below.


#### color

A field `color` attribute specifies a value from
Google's
[Material Design Color Pallete](https://material.io/guidelines/style/color.html#).

#### icon

The `icon` attribute specifies an icon to use for a field when it's
displayed on the **Train** page.

Guild provides icons from the [Font Awesome](http://fontawesome.io/)
library.

When specifying a font, omit the `fa-` prefix.

#### label

A field `label` is used to identify the field value.

#### source

A field `source` references the Guild series key containing the source
data.

Guild data sources may include:

| `series/ + SERIES` | a time series logged for a run |
| `flags` | flags associated with a run |
| `attrs` | attributes associated with a run |

The series available for a run can be listed using the `list-series`
command. To list the series available for the latest run, for example,
run:

{% term %}
$ guild list-series --latest-run
{% endterm %}

Below is a list of series typically collected for each run:

| `op/cpu/util` | operation CPU utilization |
| `op/mem/rss` | operation resident memory |
| `op/mem/vms` | operation virtual memory |
| `sys/cpu/util` | system CPU utilization |
| `sys/cpuN/util` | CPU utilization for core N |
| `sys/devDEV/rkbps` | KBs read per second for device DEV |
| `sys/devDEV/rps` | reads per second for device DEV |
| `sys/devDEV/util` | utilization for device DEV |
| `sys/devDEV/wkbps` | KBs written per second for device DEV |
| `sys/devDEV/wps` | writes per second for device DEV |
| `sys/gpuN/fanspeed` | fanspeed for GPU N |
| `sys/gpuN/gpu/util` | utilization for GPU N |
| `sys/gpuN/mem/free` | free memory for GPU N |
| `sys/gpuN/mem/total` | total memory for GPU N |
| `sys/gpuN/mem/used` | used memory for GPU N |
| `sys/gpuN/mem/util` | memory utilization for GPU N |
| `sys/gpuN/powerdraw` | power drawn in watts from GPU N |
| `sys/gpuN/pstate` | pstate for GPU N |
| `sys/gpuN/temp` | temperature for GPU N |
| `sys/mem/free` | free system memory |
| `sys/mem/total` | total system memory |
| `sys/mem/used` | used system memory |
| `sys/mem/util` | utilized system memory |
| `sys/swap/free` | free swap |
| `sys/swap/total` | total swap |
| `sys/swap/used` | used swap |
| `sys/swap/util` | utilized swap |
| `tf/SCALAR_SUMMARY` | SCALAR_SUMMARY written to TensorFlow event logs |

#### reduce

The `reduce` attribute specifies a function that reduces a series of
values to a single value.

Available reduce functions are:

| `average` | average of the series |
| `duration` | duration across the series --- taken by subtracting the timestamp of the first series event from the last event |
| `last` | the last value of the series |
| `last_5_average` | the average of the last five values of the series --- or the average of the series if there are less than five values |
| `steps` | the number of steps across the series --- taken by subtracting the step value of the first series event from the last event |

#### format

The `format` attribute indicates how the field value should be formatted.

Guild supports the formatting specification defined
by [Numeral.js](http://numeraljs.com/#format).

## Series

Series are similar to fields --- they are specification for displaying
data collected by Guild during a run. Series attributes specify how
time series are displayed and so have slightly different semantics from
fields.

Like fields, Guild defines named series with predefined
attributes. Each series can be modified by adding a corresponding
named series section.

For examples of Guild's predefined series, refer to:

- [default-series.config](https://github.com/guildai/guild/blob/master/priv/viewdefs/default-series.config) (base series attributes)
- [tensorflow-series.config](https://github.com/guildai/guild/blob/master/priv/viewdefs/tensorflow-series.config) (TensorFlow specific attributes)

### Series attributes

#### format

The `format` attribute defined how series values are formatted. Refer
to the [field format attribute](#format) for more information.

#### label

The `label` attribute is displayed on the Y axis of series charts.

#### source

The `source` attribute of a series refers to a series collected by
Guild during a run. Refer to the [field source attribute](#source) for
more information.

The `source` attribute may be a regular expression that matches
multiple sources. Each source is displayed as a separate series in a
series widget.

Refer to the series examples above for commonly used sources.

#### title

The `title` attribute is used as the chart widget title.

## Resources

Resources are project sections that define a `prepare` operation. They
can be used to perform miscellaneous operations for a project.

For an example of a resource, see the `samples` resource defined in the
{% ref mnist-example-guild %}.

### Resource attributes

#### prepare

Resources are *prepared* and so use a `prepare` command spec.

#### runtime

Like models, resource sections require a runtime attribute. Refer to
the [runtime attribute for train](#runtime) for more information.
