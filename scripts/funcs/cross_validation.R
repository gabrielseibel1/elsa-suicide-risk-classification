EVAL_CV_K <- if (IS_SERIOUS_EXECUTION) 10 else 2
EVAL_CV_N <- if (IS_SERIOUS_EXECUTION) 3 else 1
SPLIT_PROP <- 9 / 10

# nested cross validation to tune hyperparameters then evaluate best models

train_tune_evaluate_cv <- function(all_data, method, id) {
  tic()

  print_section("Sample Data", id)
  set.seed(BASE_SEED)
  sampling <- vfold_cv(data = all_data, v = EVAL_CV_K, repeats = EVAL_CV_N, strata = Class)
  sampling$full_id <- seq(nrow(sampling)) %>% map(function(n) paste0(id, "/sample", n))
  make_config_dirs(sampling$full_id)
  print(sampling)
  save_data_info(sampling, id)

  print_section("Train models", id)
  map2(sampling$splits, sampling$full_id, train_tune_cv_wrapper, method)

  summary <- save_performance_summary_single_model(id, sampling$full_id)
  print(summary, n = nrow(summary))
  print_section("Done!", id)

  toc()
}

train_tune_cv_wrapper <- function(splits, id, method) {
  # split data in training/test
  train_data <- analysis(splits)
  test_data <- assessment(splits)
  save_data_info(splits, id)

  train_eval_save(train_data, test_data, method, id)
}

train_tune_evaluate_split <- function(all_data, method, id) {
  # split data in training/test
  set.seed(BASE_SEED)
  split <- initial_split(all_data, prop = SPLIT_PROP, strata = Class)
  train_data <- training(split)
  test_data <- testing(split)
  print(split)
  save_data_info(split, id)

  train_eval_save(train_data, test_data, method, id)
}


train_eval_save <- function(train_data, test_data, method, id) {
  tic()
  print_section("Train model", id)

  # train model
  model <- train_tune(train_data, method)
  save_model(model, id)

  # evaluate model performance on held-back instances
  performance <- get_test_for_evaluation(test_data, model) %>% evaluate_from_tests(model)
  print("Model performance:\n")
  print(performance)
  save_evaluation(performance, id)

  toc()
}