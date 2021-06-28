AGGREGATION_POSITIVE_THRESHOLD <- 0.5

get_aggregation_prediction <- function(data_split, i, max_test_instances = -1) {
  test_glmnet <- get_test_from_saved_model(METHOD_GLMNET, data_split, i, max_test_instances)
  test_ranger <- get_test_from_saved_model(METHOD_RF, data_split, i, max_test_instances)
  test_nb <- get_test_from_saved_model(METHOD_NAIVE_BAYES, data_split, i, max_test_instances)

  positive <- (test_glmnet$POSITIVE +
    test_nb$POSITIVE +
    test_ranger$POSITIVE) / 3
  negative <- 1 - positive
  pred <- (positive > AGGREGATION_POSITIVE_THRESHOLD) %>%
    factor(levels = CLASS_LEVELS, labels = CLASS_LABELS)

  test <- data.frame(pred = pred, POSITIVE = positive, NEGATIVE = negative, obs = test_glmnet$obs)
  test
}


save_aggregation_evaluations <- function() {
  data_info <- readRDS('reports/results/models_and_evals/glmnet:::MODE_PRESENCE:::LABEL_H:::SYMP_TRUE:::CV_TRUE/data_info.rds')

  id <- "aggregation:::MODE_PRESENCE:::LABEL_H:::SYMP_TRUE:::CV_TRUE"

  data_info$full_id <- seq(nrow(data_info)) %>%
    map(function(n) paste0(id, "/sample", n))
  make_config_dirs(data_info$full_id)

  purrr::walk(seq_len(nrow(data_info)), function(i) {
    get_aggregation_prediction(data_info$splits[[i]], i) %>%
      evaluate_from_tests(model = NULL) %>%
      save_evaluation(paste0(id, "/sample", i))
  })

  save_performance_summary_single_model(id, data_info$full_id)
}
