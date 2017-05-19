---
layout: docs
title: Working with packages
description: Using Guild packages to accelerate TensorFlow development.
group: tutorials
---

Guild packages are a convenient way to quickly bootstrap you
TensorFlow model development. Packages let you find and install
predefined models, including those that have been trained on various
datasets. Source packages may be used to generate new Guild projects
that you customize or use to fine tune trained models with your own
data.

## Contents

* Will be replaced with the ToC
{:toc}

## Package overview

Guild packages are compressed archives that contain source code and
binary files associated with TensorFlow model training and use. In
general, a package can contain any of these artifacts:

- Datasets
- Untrained models
- Trained models
- Project source code
- Guild project templates

Packages follow a consistent naming convention to help identify what
they contain:

    NAME[:DATASET]-VERSION.pkg.tar.xz

`NAME` is either a model or dataset name. If a model has been trained,
`DATASET` indicates the dataset used for training. `VERSION` is used
to indicate when a package has been updated and is either a date
timestamp or an incrementing integer.

## Finding packages

To find a package, use the `search` command:

```
$ guild search TERM
```

`TERM` may be a part of a package name (e.g. `inception`) or a regular
exception. Use an asterisk (`*`) to view all packages.

## Installing and uninstalling packages

To install a package, use the `install` command:

```
$ guild install PACKAGE
```

`PACKAGE` may be the package name with or without a version number. If
the version number is omitted, Guild will install the latest version.

Packages are installed per user in `$HOME/.guild/packages`.

You may uninstall a package using the `uninstall` command:

```
$ guild uninstall PACKAGE
```

To uninstall a paricular version, specify the package version
number. To uninstall all versions of a package, omit the version
number.

## Using installed packages

Packages are used in one of two ways:

- As sources for binary artifacts, including datasets and saved models
- As templates for generating new projects

To reference binary artifacts provided by a package, you may use the
system path `$HOME/.guild/packages/PACKAGE-VERSION`. For example, to
reference the `mnist-1` package in a script:

```
$ python train.py --dataset $HOME/.guild/packages/mnist-1
```

Use Guild's `list-packages` command to list installed packages.
