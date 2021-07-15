get_env_or_default <- function(env, default) {
  value <- toupper(Sys.getenv(env, unset = NA))
  if (is.na(value)) default else value
}

IS_SLURM_EXECUTION <- get_env_or_default('ESRP_SLURM_EXEC', FALSE)
IS_SERIOUS_EXECUTION <- get_env_or_default('ESRP_SERIOUS_EXEC', TRUE)
N_WORKERS <- get_env_or_default('ESRP_MAX_CPUS', 6)
BASE_SEED <- 42
set.seed(BASE_SEED)

if (IS_SLURM_EXECUTION) {
  libs_dir <- '~/R/x86_64-pc-linux-gnu-library/3.5'
  if (!is.element(libs_dir, .libPaths())) {
    .libPaths(c(libs_dir, .libPaths()))
    dir.create(libs_dir, showWarnings = FALSE, recursive = TRUE)
  }
}

if (!require('pacman')) install.packages('pacman')
pacman::p_load(
  zeallot,
  ggplot2,
  viridis,
  hrbrthemes,
  future,
  magrittr,
  parallel,
  doParallel,
  rsample,
  tictoc,
  tidyverse,
  tibble,
  haven,
  ranger,
  glmnet,
  naivebayes,
  caret,
  recipes,
  themis,
  #DMwR,
  MLmetrics,
  mlr,
  e1071,
  adabag,
  RSNNS,
  multiROC
)

future::plan(future::multicore(workers = N_WORKERS))

source("scripts/funcs/data.R")
source("scripts/funcs/model.R")
source("scripts/funcs/io.R")
source("scripts/funcs/cross_validation.R")
source("scripts/funcs/summary.R")
source("scripts/funcs/aggregation.R")