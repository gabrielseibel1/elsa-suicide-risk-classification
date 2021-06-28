# methods used to train the model (use getModelInfo("my_method")[[1]]$grid to get the default tunning grid)
METHOD_MLP <- "mlpML" # Tuning parameters: layer1 (#Hidden L1), layer2 (#Hidden L1), layer3 (#Hidden L3)
METHOD_RF <- "ranger" # Tuning parameters: mtry (#Randomly Selected Predictors), splitrule (Splitting Rule), min.node.size (Minimal Node Size)
METHOD_GLMNET <- "glmnet" # Tuning parameters: alpha (Mixing Percentage), lambda (Regularization Parameter)
METHOD_ADABOOST <- "AdaBoost.M1" # Tuning parameters: mfinal (#Trees), maxdepth (Max Tree Depth), coeflearn (Coefficient Type)
METHOD_SVM_COST <- "svmLinearWeights" # Tuning parameters: cost (Cost), weight (Class Weight)
METHOD_NAIVE_BAYES <- "naive_bayes" # Tuning parameters: laplace (Laplace Correction), usekernel (Distribution Type), adjust (Bandwidth Adjustment)
METHOD_ENSEMBLE <- "aggregation"
MODEL_METHODS <- c(METHOD_MLP, METHOD_RF, METHOD_GLMNET, METHOD_ADABOOST, METHOD_SVM_COST, METHOD_NAIVE_BAYES, METHOD_ENSEMBLE)
METRIC <- "F2"
MAXIMIZE <- TRUE

RFE_CV_K <- if (IS_SERIOUS_EXECUTION) 5 else 2
RFE_CV_N <- if (IS_SERIOUS_EXECUTION) 2 else 1
INNER_CV_K <- if (IS_SERIOUS_EXECUTION) 5 else 2
INNER_CV_N <- if (IS_SERIOUS_EXECUTION) 2 else 1
RFE_PREDICTOR_SETS_SIZES <- if (IS_SERIOUS_EXECUTION) 2^(3:9) else 8 # 8 16 32 64 128 256 512
HYPERPARAMS_SEARCH <- 'grid' # 'random'
HYPERPARAMS_TUNE_LENGTH <- if (IS_SERIOUS_EXECUTION) switch(HYPERPARAMS_SEARCH, 'random' = 25, 'grid' = 5) else 2
SMOTE_NEIGHBORS <- 5

# fix repeated cross validation seeds for reproducibility

seeds_repeated_cv_rfe_ctrl <- function()
  seeds_repeated_cv(RFE_CV_K, RFE_CV_N, length(RFE_PREDICTOR_SETS_SIZES) + 1)

seeds_repeated_cv_train <- function(method)
  seeds_repeated_cv(
    INNER_CV_K, INNER_CV_N,
    switch(
      HYPERPARAMS_SEARCH,
      'random' = HYPERPARAMS_TUNE_LENGTH,
      'grid' = switch(
        method,
        "ranger" = HYPERPARAMS_TUNE_LENGTH^3,
        "mlpML" = HYPERPARAMS_TUNE_LENGTH^2, # using only 2/3 params
        "glmnet" = HYPERPARAMS_TUNE_LENGTH^2,
        "naive_bayes" = nrow(hyperparameters_grid(METHOD_NAIVE_BAYES)),
      )
    )
  )

hyperparameters_grid <- function(method) {
  if (HYPERPARAMS_SEARCH == "grid" & method == METHOD_NAIVE_BAYES) {
    expand.grid(usekernel = c(TRUE, FALSE), laplace = c(0, 1), adjust = 1)
  } else {
    NULL # default value for caret::train
  }
}

seeds_repeated_cv <- function(k, n, n_models) {
  set.seed(BASE_SEED)
  n_seeds <- k * n + 1
  seeds <- vector(mode = "list", length = n_seeds)
  for (i in 1:n_seeds) seeds[[i]] <- sample.int(1000, n_models)
  seeds[[n_seeds]] <- sample.int(1000, 1)
  seeds
}

# define preprocessing pipeline to be used within resampling

step_preprocess <- function(recipe, method) { recipe %>%
  themis::step_downsample(Class, seed = BASE_SEED, under_ratio = 2) %>%
  recipes::step_meanimpute(all_predictors()) %>%
  recipes::step_nzv(all_predictors()) %>%
  recipes::step_corr(all_predictors()) %>%
  recipes::step_center(all_predictors()) %>%
  recipes::step_scale(all_predictors()) %>%
  themis::step_smote(Class, seed = BASE_SEED, neighbors = SMOTE_NEIGHBORS, over_ratio = 1)
}

train_tune <- function(train_data, method) {
  # train_with_caret_train(train_data, method)
  train_with_caret_rfe(train_data, method)
}

# RFE with cross-validation to select sets of predictors

train_with_caret_rfe <- function(train_data, method) {
  funcs <- caretFuncs
  funcs$summary <- f2_summary
  funcs$fit <- inner_train

  rfe_folds <- rsample2caret(vfold_cv(train_data, v = RFE_CV_K, repeats = RFE_CV_N, strata = Class))
  rfe_seeds <- seeds_repeated_cv_rfe_ctrl()
  set.seed(BASE_SEED)

  caret::rfe(
    recipe(Class ~ ., data = train_data) %>% step_preprocess(method), data = train_data,
    method = method,
    metric = METRIC,
    maximize = MAXIMIZE,
    sizes = RFE_PREDICTOR_SETS_SIZES,
    rfeControl = rfeControl(
      rerank = TRUE,
      method = "repeatedcv", number = RFE_CV_K, repeats = RFE_CV_N,
      index = rfe_folds$index, indexOut = rfe_folds$indexOut, functions = funcs,
      seeds = rfe_seeds, verbose = TRUE, allowParallel = FALSE
    ),
    algorithm = method # passing parameter to inner training function
  )
}

# basic training function to fit model inside the folds of RFE

inner_train <- function(x, y, first, last, ...) {
  train_with_caret_train(x, y, list(...)$algorithm)
}

train_with_caret_train <- function(x, y, method) {
  inner_folds <- createDataPartition(y, times = INNER_CV_N, p = (INNER_CV_K - 1) / INNER_CV_K)
  inner_seeds <- seeds_repeated_cv_train(method)
  set.seed(BASE_SEED)

  caret::train(
    x, y,
    method = method,
    metric = METRIC,
    maximize = MAXIMIZE,
    tuneLength = HYPERPARAMS_TUNE_LENGTH,
    tuneGrid = hyperparameters_grid(method),
    trControl = trainControl(
      method = "repeatedcv", number = INNER_CV_K, repeats = INNER_CV_N,
      index = inner_folds$index, indexOut = inner_folds$indexOut,
      seeds = inner_seeds, summaryFunction = f2_summary, search = HYPERPARAMS_SEARCH,
      verboseIter = TRUE, classProbs = TRUE, trim = TRUE
    ),
    importance = 'impurity' # so that we can use varImp with ranger
  )
}

# evaluate a model using test data

get_test_for_evaluation <- function(test_data, model) {
  test <- predict(model, test_data, type = "prob")
  test$obs <- test_data$Class
  test
}

get_test_from_saved_model <- function(name, data_split, i, max_test_instances = -1) {
  if (name == METHOD_ENSEMBLE) {
    get_aggregation_prediction(data_split, i, max_test_instances)
  }
  else {
    test_data <- assessment(data_split)
    model <- readRDS(paste0('reports/results/models_and_evals/', name, ':::MODE_PRESENCE:::LABEL_H:::SYMP_TRUE:::CV_TRUE/sample', i, '/model.rds'))
    if (max_test_instances > 0 & nrow(test_data) > max_test_instances) {
      test_data %<>% head(max_test_instances - nrow(test_data)) # cut excess
    }
    get_test_for_evaluation(test_data, model)
  }
}

evaluate_from_tests <- function(test, model) {
  cm <- caret::confusionMatrix(test$pred, test$obs, CLASS_LABELS[1], mode = "prec_recall")
  if (cm$positive != CLASS_LABELS[1]) {
    stop("confusion matrix used wrong level as positive!")
  }
  metrics <- enframe(c(
    F2 = f2_score(test),
    prSummary(test, CLASS_LABELS, model),
    twoClassSummary(test, CLASS_LABELS, model),
    brier = measureBrier(test$POSITIVE, test$obs, CLASS_LABELS[2], CLASS_LABELS[1])
  ))
  metrics[metrics$"name" == "AUC",]$name <- "PRAUC"
  metrics[metrics$"name" == "ROC",]$name <- "ROCAUC"

  metrics <- pivot_wider(metrics, names_from = name, values_from = value)
  metrics$true_positives <- cm$table[[1]]
  metrics$false_negatives <- cm$table[[2]]
  metrics$false_positives <- cm$table[[3]]
  metrics$true_negatives <- cm$table[[4]]
  colnames(metrics) <- tolower(colnames(metrics))

  metrics
}

f2_summary <- function(data, lev = NULL, model = NULL) c(F2 = f2_score(data))

f2_score <- function(data) {
  f2 <- F_meas(data = data$pred, reference = data$obs, relevant = CLASS_LABELS[1], beta = 2)
  if (is.finite(f2)) f2 else 0
}
