# Command Reference

## list-models

Prints a list of models defined for the project.

## prepare

Prepares a model for training, if the model supports a prepare
operation.

It's common for a model to require some preparation before
training. This logic is commonly implemented by the training script --
e.g. downloading required files if they aren't already downloaded. In
cases however where the preparation is parameterized -- e.g. the user
can specify alternative data sources -- it's convenient to perform
this operation separately using the `prepare` command.

The prepare command is defined in the model using the `prepare`
attribute.

Here's an example of a model that defines a prepare operation for
retraining exercise:

``` conf
[model]
runtime        = tensorflow
prepare        = retrain_prepare
train          = retrain
train_requires = images
```

Note the addition of `train_requires`. This specifies a comma
separated list of files or directories that must exist before Guild
will proceed with a training operation. It's helpful specify these in
the project model to give the user a helpful error message if the
model hasn't been prepared.

In this example, the `retrain_prepares` script is responsible for
preparing an `images` directory. If that directory doesn't exist when
the `train` command is executed, Guild will print an error message
suggesting that the user run `guild prepare` first.

## train

Trains a model defined in the Guild project as specified by the
model's `train` attribute.

Commands are assumed to be relative paths to the training script,
located in the project. They may be specified without their `.py` file
extension.

Commands may also include argments.

Here's the project section of the MNIST sample model:

``` conf
[model]
runtime  = tensorflow
train    = mnist_with_summaries --data_dir MNIST_data --summaries_dir=$RUNDIR
```

When `guild train` is executed in this project, the
`mnist_with_summaries.py` will be executed with the specified
arguments as well as any additional flags defined for the model.

### Flags

Training operations support flags, which may be defined in the project
and added or redefined on the command line using the `-F, --flag`
option.

Groups of flags may be applied as profiles using the `-p, --profile`
option.

Refer to the Guild project for the list of defined flags and profiles.

### Alternative models

If the Guild project file defines multiple models, you may specify the
model to train as an argument to the command. For example, to train a
model named "inception-v2" in the project:

``` sh
$ train inception-v2
```

Use [`list-models`](#list-models) to print a list of models defined
for the project.

### Previewing a train command

To see what Guild will execute for a training command without actually
performing the training, use the `--preview` option:

``` sh
$ guild train --preview
```

This can be helpful to verify a training operation or debug your
project configuration.

### Training requirements

If a training operation requires a file or directory (e.g. containing
images) you may specify it in the project model using the
`train_requires` attributs. If a requires file or directory does not
exist when the `train` command is run, Guild will print an error
message and suggest running `guild prepare`.

See [`prepare`](#prepare) for details on preparing a model for
training.
