#!/usr/bin/env bash

set -eu

Rscript scripts/setup.R
Rscript scripts/train.R
Rscript scripts/report.R
