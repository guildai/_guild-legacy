---
layout: docs
title: Packaging
description: How to create and use Guild packages
group: user-guide
---

Guild packages can be used to quickly install pre-trained models,
datasets, and source packages that are used as templates for new Guild
projects.

## Contents

* Will be replaced with the ToC
{:toc}

## Overview

There are three types of packages:

- Datasets
- Models
- Source package

## Dataset packages

Dataset packages contain data that can be used to train
models. Datasets may contain files in any format but it's common to
use TensorFlow's TFRecord format for efficiency. The dataset format
must comply with the format expected by any model trained using the
dataset.
