---
layout: docs
title: Guild View
description: Guide for using Guild view
group: user-guide
---

Guild View is a web application used to visualize and interact with a
Guild project.

## Contents

* Will be replaced with the ToC
{:toc}


## Starting Guild View

To start Guild View, change to your project directory and run the
`view` command:

{% term %}
$ guild view
{% endterm %}

Next, open your browser to:

<div class="m-3">
<a href="http://localhost:6333" target="_blank">http://localhost:6333</a>
</div>

To run Guild View on a different port, use the `--port` option. For
example, to use port 8888, run the command:

{% term %}
$ guild view --port 8888
{% endterm %}

For more information, refer to the `view` {% ref cmd:view %}.

Guild View provides a default view for your project that is configured
by the {% ref guild-project-file %}. For information on creating a
custom view for your project,
see [Custom views](/developers-guide/#custom-views).

The default view provides three pages:

* Train
* Compare
* Serve

## Train page

The train page displays a summary of a selected run. Runs are training
operations, either completed or in progress. Guild View updates
in-progress runs in real time --- you don't need to refresh the view.

The train page provides a variety of information about a run:

- Run selector
- Run status
- Field values (single value summaries)
- Flags used for the run
- System attributes at the time of the run
- Time series
- Run output

### Run selector

The run selector is used to select the current run. A *run* is a model
training operation and contains a database of flags, time series,
system attributes, and training output. The **Train** page displays
the current values for the selected run.

{% screen guild-view-screen-13.png %}

### Run status

The run status displays the status of the run. It's updated
automatically when in the *Running* state.

{% screen guild-view-screen-14.png %}

Status values may be one of:

| **Running**    | Run is in progress |
| **Completed**  | Run completed without errors |
| **Terminated** | Run was terminated early by the user |
| **Error**      | Run exited with an error (refer to Output for specific error messages) |

### Field values

Fields display single value summaries for the run. Fields are defined
in the {% ref guild-project-file %}. For details on defining fields
for your model,
see [Fields](/project-reference/#fields-1).

{% screen guild-view-screen-5.png %}

Field values are updated dynamically every 5 seconds. You may modify
this interval using the `--interval` option when running `guild view`.

### Flags

Flags are parameters that are passed to your training scripts for a
run. Guild captures these because they often play an important role in
the training and are useful for comparing results.

{% screen guild-view-screen-15.png %}

For more information on flags and how they're used in operations,
see [Flags](/project-reference/#flags).

### Attributes

Guild captures and logs various system attributes for each
run. Attributes include GPU and CPU characteristics that can effect
training performance and can be helpful when comparing runs across
different system configurations.

{% screen guild-view-screen-16.png %}

### Time series

Guild logs time series scalar values are generated during a train
operation including scalar summaries written to TensorFlow event
logs. Guild also captures system metrics as time series.

{% screen guild-view-screen-17.png %}

Time series chart widgets are used to display these values. You must
specify the series to display for a model in the Guild project
file. For more information about specifying series for a model,
see [Series](/project-reference/#series).

### Output

Log captures all output generated during a training operation. Output
is displayed at the bottom of the default view.

{% screen guild-view-screen-18.png %}

To search for text occurring in the output, use the **Filter** field.

## Compare page

The compare page displays a table of project runs with various fields
that can be helpful in assessing relative changes in training performance.

{% screen guild-view-screen-19.png %}

Compare fields must be specified in the Guild project file. For detail
on specifying compare fields, see the reference for
the
[view compare attribute](/project-reference/#compare).

To filter the table content to runs containing specific text or number
values, use the **Filter** field.

Runs may be sorted according to field values, either in ascending or
descending order. For example, this is useful for finding runs with
the highest validation accuracy.

## Serve page

The serve page is used to submit ad hoc requests to a trained model
for inference.

1. Generate JSON input for your model
2. Paste the JSON into the input field
3. Click **Run model**
4. Example the model output

{% note %}

Guild View
embeds [Guild Serve](/user-guide/guild-serve/) --- a
light weight HTTP server used to host your trained model as a web
service --- as a facility for performing ad hoc predictions using a
trained model. To run your model as a backend service, use Guild Serve
instead.

{% endnote %}

### Model run statistics

The Serve page in Guild View displays various statistics about model
inference.

{% screen guild-view-screen-20.png %}

| **Last run time** | The time it took to last run the model in milliseconds |
| **Average run time** | The average time it took to run the model since |
| **Predictions per second** | An estimate of the number of predictions the model can make in one second |
| **Last run memory** | The memory the model used for the last run |

Guild View is able to serve one model at a time. When you select a
different model in the Serve page, the statistics are reset.

### Model input

The Serve tab currently requires raw JSON as input. Guild View
displays the expected input tensors to the left of the input field.

{% screen guild-view-screen-21.png %}

Future releases of
Guild AI will simplify the process of running models, including
widgets for uploading files like images, audio, etc.

{% note %}

Some Guild examples provide a
`samples` [resource](/project-reference/#resources)
that generates JSON output that can be used to test a model. For
example, the {% ref mnist-example %} can generate a number of sample
images, including JSON corresponding JSON input, that can be used to
submit ad hoc images to the trained MNIST models.

For example, to generate samples for the MNIST example:

{% term %}
$ cd guild-examples/mnist
$ guild prepare samples
{% endterm %}

This will create a `samples` directory containing a number of images
and associated JSON inputs that may be used when predicting the
image's digit. Each JSON file is a single image object. Note that the
input in Guild View must be a list of such objects --- be sure to
enclose the object in square brackets to submit a well formed JSON
list.

{% endnote %}

{% insight %}

The interface provided in the Serve page is more suited to developers
preparing to run their model in production than to end-user
experimentation. The JSON submitted the web browser interface is
identical in format to the JSON required by the REST API.

{% endinsight %}

### Model output

When you click **Run model** your JSON input is submitted to the model
for processing. Model output is returned as JSON and displayed in the
output field.

As with input tensors, Guild displays the model's output tensors.

## Next Steps

{% next /user-guide/guild-serve/ %}Read about Guild Serve{% endnext %}
