N_RESAMPLES <- EVAL_CV_K * EVAL_CV_N
MAX_RFE_BEST_SET_SIZE <- RFE_PREDICTOR_SETS_SIZES[length(RFE_PREDICTOR_SETS_SIZES)]
ONLY_WITH_TMC <- TRUE

get_all_configs <- function() {
  configs <- get_all_data_configs(with_data = FALSE) %>%
    cross_df_list(MODEL_METHODS, "method") %>%
    cross_df_list(c(TRUE, FALSE), "cv")
  configs$id <- purrr::pmap_chr(list(configs$method, configs$mode, configs$label, configs$symptoms, configs$cv), get_model_id)
  configs
}

config_filter_by_training_done <- function(configs, training_done) {
  configs$has_file <- configs$id %>% purrr::map_lgl(
    ~file.exists(id_to_path(., MODEL_RDS)) |
      file.exists(id_to_path(., PERFORMANCE_RDS)) |
      file.exists(id_to_path(., PERFORMANCE_CSV))
  )
  configs <- configs %>% dplyr::filter(has_file == training_done)
  configs$has_file <- NULL
  configs
}

save_performance_summary_single_model <- function(model_id, resamples_ids) {
  evaluations <- collect_evaluations_from_ids(resamples_ids)
  save_evaluation_csv(evaluations, model_id)
  evaluations
}

collect_evaluations_from_ids <- function(ids) {
  ids %>%
    map(read_csv_, PERFORMANCE_CSV) %>%
    bind_rows() %>%
    as_tibble()
}

get_plot_color_option <- function(method) {
  switch(
    method,
    #"glmnet" = "magma",
    #"ranger" = "plasma",
    #"mlpML" = "inferno",
    #"aggregation" = "viridis",
    #"cividis"
    "viridis" #"plasma"
  )
}

get_method_true_name <- function(method) {
  purrr::map_chr(method, ~switch(
    .,
    "glmnet" = "Elastic Net",
    "ranger" = "Random Forest",
    "mlpML" = "Multi-Layer Perceptron",
    "naive_bayes" = "Naive Bayes",
    "aggregation" = "Averaging Ensemble",
  ))
}

get_models_paths <- function(method) {
  filenames <- list.files(
    paste0("reports/results/models_and_evals/", method, ":::MODE_PRESENCE:::LABEL_H:::SYMP_TRUE:::CV_TRUE"),
    pattern = ".*model.*",
    recursive = T,
    full.names = T
  )
  tibble(path = filenames)
}

init_var_imps <- function(df = NULL) {
  if (is.null(df)) {
    var_imps <- tibble(order = 1:N_RESAMPLES)
  } else {
    var_imps <- df
    var_imps$order <- 1:N_RESAMPLES
  }
  features <- colnames(get_features(symptoms = T))
  features <- features[-match("idelsa", features)] # why not select(-all_of(IDELSA))?
  var_imps[, features] <- 0.0
  var_imps
}

get_var_imps <- function(models) {
  models <- init_var_imps(models)

  models$best_vars <- models$path %>%
    furrr::future_map(~readRDS(.)$optVariables)

  models %<>% dplyr::arrange(order)
  models %<>% select(-path)
  models %<>% select(order, best_vars, everything())

  for (model_index in seq_len(nrow(models))) {
    size <- length(models$best_vars[[model_index]])
    for (var_index in 1:size) {
      # y(m, a) = (#A - I(m,a))/ #A
      var <- models$best_vars[[model_index]][[var_index]]
      rank <- var_index - 1
      imp <- (size - rank) / size
      models[[model_index, var]] <- imp
    }
  }

  models %<>% select(-best_vars)

  models
}

get_var_imps_summary <- function(var_imps) {
  var_imps <- var_imps_filter_important(var_imps)

  importances <- tibble(feature = character(), importance = numeric())

  for (feature in colnames(var_imps)) {
    if (feature == "order") next

    importance <- 0.0
    for (model_index in seq_len(nrow(var_imps))) {
      importance <- importance + var_imps[[model_index, feature]]
    }
    importance <- importance / nrow(var_imps)

    importances %<>% add_row(feature = feature, importance = importance)
  }

  importances %>% dplyr::arrange(-importance)
}

var_imps_filter_important <- function(var_imps) {
  features_are_useless <- var_imps %>%
    colnames() %>%
    purrr::modify(~purrr::every(var_imps[[.]], ~. <= 0))
  unimportant_features <- colnames(var_imps)[features_are_useless == TRUE]
  var_imps[unimportant_features] <- NULL
  var_imps
}

get_metric_box_plot <- function(evaluations, metric_unquoted, metric_str) {
  ggplot(data = evaluations, mapping = aes(get_method_true_name(method), !!enquo(metric_unquoted), fill = method)) +
    geom_boxplot(show.legend = FALSE) +
    ggtitle(paste(metric_str, "per method")) +
    xlab("Method") +
    ylab(metric_str) +
    ylim(0.5, 1) +
    theme_ipsum_rc()
}

get_var_imp_heatmap <- function(var_imps, features_to_include, method) {
  data <- var_imps_filter_important(var_imps) %>%
    dplyr::select(c(order, all_of(features_to_include))) %>%
    tidyr::pivot_longer(cols = !order, names_to = "feature", values_to = "importance")
  data$order_char <- as.character(data$order)

  descrs <- read_csv("reports/results/models_and_evals/summary/best_variables_manual_descr.csv")
  data$label <- left_join(x = data, y = descrs, by = "feature")$label

  ggplot(data, aes(x = reorder(order_char, order), y = label, fill = importance)) +
    geom_tile() +
    ggtitle(paste("Feature importance per resample -", get_method_true_name(method))) +
    xlab("Resample") +
    ylab("Feature") +
    labs(fill = "Importance") +
    scale_fill_viridis(
      discrete = FALSE,
      option = get_plot_color_option(method),
      breaks = c(0, 0.5, 1),
      limits = c(0, 1)
    ) +
    theme_ipsum_rc()
}

get_var_imps_summary_col_plot <- function(var_imps_summary, method) {
  descrs <- read_csv("reports/results/models_and_evals/summary/best_variables_manual_descr.csv")
  var_imps_summary$label <- left_join(x = var_imps_summary, y = descrs, by = "feature")$label

  ggplot(var_imps_summary, aes(x = importance, y = reorder(label, importance), fill = importance)) +
    geom_col() +
    ggtitle(paste("Feature importance overall -", get_method_true_name(method))) +
    xlab("Importance") +
    ylab("Feature") +
    labs(fill = "Importance") +
    scale_fill_viridis(
      discrete = FALSE,
      option = get_plot_color_option(method),
      breaks = c(0, 0.5, 1),
      limits = c(0, 1)
    ) +
    xlim(0, 1) +
    theme_ipsum_rc()
}

get_save_and_plot_performance_summary_all_models <- function() {
  configs <- get_all_configs() %>% config_filter_by_training_done(TRUE)
  evaluations <- collect_evaluations_from_ids(configs$id)

  make_summary_dir()
  save_evaluation_csv(evaluations, SUMMARY_ID)

  # remove `/sampleXYZ` part from ids
  evaluations %<>% dplyr::mutate(id = sub("/.*", "", id))

  # separate ids in method, mode, label and symptoms columns
  evaluations$method <- evaluations$id %>% purrr::modify(function(id) split_model_id(id)$method)
  evaluations$mode <- evaluations$id %>% purrr::modify(function(id) split_model_id(id)$mode)
  evaluations$label <- evaluations$id %>% purrr::modify(function(id) split_model_id(id)$label)
  evaluations$symptoms <- evaluations$id %>% purrr::modify(function(id) split_model_id(id)$symptoms)

  # save box plots for f2, roc, sens and spec
  print(get_metric_box_plot(evaluations, f2, "F2-Score"))
  print(get_metric_box_plot(evaluations, rocauc, "AUCROC"))
  print(get_metric_box_plot(evaluations, sens, "Sensibility"))
  print(get_metric_box_plot(evaluations, spec, "Specificity"))

  # save a summary csv with a row for each model variation
  summary <- evaluations %>%
    dplyr::group_by(method, mode, label, symptoms) %>%
    dplyr::summarise(
      f2_mean = mean(f2),
      f2_sd = sd(f2),
      prauc_mean = mean(prauc),
      prauc_sd = sd(prauc),
      prec_mean = mean(precision),
      prec_sd = sd(precision),
      rec_mean = mean(recall),
      rec_sd = sd(recall),
      roc_mean = mean(rocauc),
      roc_sd = sd(rocauc),
      sens_mean = mean(sens),
      sens_sd = sd(sens),
      spec_mean = mean(spec),
      spec_sd = sd(spec)
    )
  readr::write_csv(summary, SUMMARY_FILE_NAME)

  summary
}

plot_var_imps <- function(methods = c(METHOD_GLMNET, METHOD_RF, METHOD_NAIVE_BAYES)) {
  best_vars_table <- tibble(feature = character(), importance = double())

  # plot var imp heatmap and column chart for trained models
  aggregation_var_imps <- init_var_imps()
  for (method in methods) {
    models <- get_models_paths(method)
    var_imps <- get_var_imps(models)
    var_imps_summary <- get_var_imps_summary(var_imps)
    var_imps_short_summary <- head(var_imps_summary, N_RESAMPLES)
    var_imps_heatmap <- get_var_imp_heatmap(var_imps, var_imps_short_summary$feature, method)
    var_imps_cols <- get_var_imps_summary_col_plot(var_imps_short_summary, method)

    print(var_imps_heatmap)
    print(var_imps_cols)

    best_vars_table <- bind_rows(best_vars_table, var_imps_short_summary)
    aggregation_var_imps <- aggregation_var_imps + var_imps
  }

  # plots for aggregation
  var_imps <- aggregation_var_imps / length(methods)
  var_imps$order <- 1:N_RESAMPLES
  var_imps_summary <- get_var_imps_summary(var_imps)
  var_imps_short_summary <- head(var_imps_summary, N_RESAMPLES)
  var_imps_heatmap <- get_var_imp_heatmap(var_imps, var_imps_short_summary$feature, METHOD_ENSEMBLE)
  var_imps_cols <- get_var_imps_summary_col_plot(var_imps_short_summary, METHOD_ENSEMBLE)
  best_vars_table <- bind_rows(best_vars_table, var_imps_short_summary)

  best_vars_descriptions <- get_features_descriptions(best_vars_table$feature)

  # print and save artifacts
  print_section("Best variables:")
  print(best_vars_descriptions, n = nrow(best_vars_descriptions))
  readr::write_csv(best_vars_descriptions, BEST_VARS_DESCR_FILE_NAME)

  print(var_imps_heatmap)
  print(var_imps_cols)
}

plot_rfe_results <- function(methods = c(METHOD_GLMNET, METHOD_RF, METHOD_NAIVE_BAYES)) {
  for (method in methods) {
    results <- get_models_paths(method)$path %>%
      map(~readRDS(.)$results) %>%
      bind_rows() %>%
      as_tibble()

    results$Variables_chr <- results$Variables %>%
      purrr::map_chr(~if (. > MAX_RFE_BEST_SET_SIZE) "Original Size" else as.character(.))
    results$Variables %<>% purrr::map_int(as.integer)
    results$F2 %<>% purrr::map_dbl(~if (is.na(.)) 0 else .)

    p <- ggplot(results, aes(x = reorder(Variables_chr, Variables), y = F2, fill = Variables_chr)) +
      geom_boxplot(show.legend = FALSE) +
      ggtitle(paste("F2-Score per RFE size -", get_method_true_name(method))) +
      xlab("Best variables set size") +
      ylab("F2-Score") +
      ylim(0.5, 0.75) +
      #scale_fill_viridis(discrete = TRUE) +
      theme_ipsum_rc()

    print(p)
  }
}

plot_hyperparameters_glmnet <- function(model) {
  lambda <- model$fit$results$lambda
  model$fit$results$lambda_factor <- factor(
    model$fit$results$lambda,
    levels = lambda,
    labels = scales::scientific(lambda, digits = 3)
  )
  p <- ggplot(
    data = model$fit$results,
    aes(x = alpha, y = F2, group = lambda_factor, color = lambda_factor)) +
    ggtitle(paste("Hyperparameter tuning -", get_method_true_name(METHOD_GLMNET))) +
    labs(color = "Lambda\n(regularization\nparameter)") +
    xlab("Alpha (mixing parameter)")

  print(std_hyperparameters_plot(p))
}

plot_hyperparameters_mlp <- function(model) {
  p <- ggplot(
    data = model$fit$results,
    aes(x = layer1, y = F2, group = layer2, color = as.factor(layer2))) +
    ggtitle(paste("Hyperparameter tuning -", get_method_true_name(METHOD_MLP))) +
    labs(color = "#Hidden Units\n in Layer 2") +
    xlab("#Hidden Units\n in Layer 1")

  print(std_hyperparameters_plot(p))
}

plot_hyperparameters_nb <- function(model) {
  p <- ggplot(
    data = model$fit$results,
    aes(x = usekernel, y = F2, group = laplace, color = as.factor(laplace))) +
    ggtitle(paste("Hyperparameter tuning -", get_method_true_name(METHOD_NAIVE_BAYES))) +
    labs(color = "Laplace Correction") +
    xlab("Distribution Type")

  print(std_hyperparameters_plot(p))
}

plot_hyperparameters_rf <- function(model) {
  p <- ggplot(
    data = model$fit$results,
    aes(x = mtry, y = F2, group = splitrule, color = splitrule)) +
    ggtitle(paste("Hyperparameter tuning -", get_method_true_name(METHOD_RF))) +
    labs(color = "Splitting Rule") +
    xlab("#Randomly Selected Predictors")

  print(std_hyperparameters_plot(p))
}

std_hyperparameters_plot <- function(p) {
  p +
    geom_line() +
    geom_point() +
    ylab("F2-Score") +
    ylim(0.65, 0.9) +
    theme_ipsum_rc()
}

plot_hyperparameters <- function() {
  model_glmnet <- readRDS(paste0('reports/results/models_and_evals/', METHOD_GLMNET, ':::MODE_PRESENCE:::LABEL_H:::SYMP_TRUE:::CV_TRUE/sample1/model.rds'))
  #model_mlp <- readRDS(paste0('reports/results/models_and_evals/', METHOD_MLP, ':::MODE_PRESENCE:::LABEL_H:::SYMP_TRUE:::CV_TRUE/sample1/model.rds'))
  model_rf <- readRDS(paste0('reports/results/models_and_evals/', METHOD_RF, ':::MODE_PRESENCE:::LABEL_H:::SYMP_TRUE:::CV_TRUE/sample1/model.rds'))
  model_nb <- readRDS(paste0('reports/results/models_and_evals/', METHOD_NAIVE_BAYES, ':::MODE_PRESENCE:::LABEL_H:::SYMP_TRUE:::CV_TRUE/sample1/model.rds'))

  plot_hyperparameters_glmnet(model_glmnet)
  plot_hyperparameters_rf(model_rf)
  #plot_hyperparameters_mlp(model_mlp)
  plot_hyperparameters_nb(model_nb)
}

roc_pr_data <- function(method) {
  t <- get_tests(method)
  list(roc = as.data.frame(roc_data(t)), pr = as.data.frame(pr_data(t)))
}

plot_all_multi_ROC <- function() {
  glmnet <- roc_pr_data(METHOD_GLMNET)
  #mlp <- roc_pr_data(METHOD_MLP)
  rf <- roc_pr_data(METHOD_RF)
  nb <- roc_pr_data(METHOD_NAIVE_BAYES)
  ensemble <- roc_pr_data(METHOD_ENSEMBLE)

  #bind dfs appending rows and using a group column
  roc <- glmnet[["roc"]] %>%
    mutate(group = get_method_true_name(METHOD_GLMNET)) %>%
    #bind_rows(mlp[["roc"]] %>% mutate(group = get_method_true_name(METHOD_MLP))) %>%
    bind_rows(rf[["roc"]] %>% mutate(group = get_method_true_name(METHOD_RF))) %>%
    bind_rows(nb[["roc"]] %>% mutate(group = get_method_true_name(METHOD_NAIVE_BAYES))) %>%
    bind_rows(ensemble[["roc"]] %>% mutate(group = get_method_true_name(METHOD_ENSEMBLE)))
  pr <- glmnet[["pr"]] %>%
    mutate(group = get_method_true_name(METHOD_GLMNET)) %>%
    #bind_rows(mlp[["pr"]] %>% mutate(group = get_method_true_name(METHOD_MLP))) %>%
    bind_rows(rf[["pr"]] %>% mutate(group = get_method_true_name(METHOD_RF))) %>%
    bind_rows(nb[["pr"]] %>% mutate(group = get_method_true_name(METHOD_NAIVE_BAYES))) %>%
    bind_rows(ensemble[["pr"]] %>% mutate(group = get_method_true_name(METHOD_ENSEMBLE)))

  #plot ROC
  roc_plot <- fissure_plot(roc) +
    geom_abline(intercept = 0, slope = 1) +
    xlab('1 - Sensitivity') +
    ylab('Specificity') +
    ggtitle('ROC mean and s.d. per model', subtitle = 'Reference line: ROC = 0.5')
  print(roc_plot)

  #plot PR
  f2 <- function(x, y) 5 * x * y / (4 * x + y)
  f2_ref <- seq(0, 1, length = 100) %>%
    expand.grid(x = ., y = .) %>%
    mutate(z = f2(x, y), group = NA)
  pr_plot <- fissure_plot(pr) +
    xlab('Recall') +
    ylab('Precision') +
    stat_contour(
      data = f2_ref,
      mapping = aes(x = x, y = y, z = z),
      breaks = 0.5,
      show.legend = TRUE,
      color = 'black'
    ) +
    ggtitle('Precision x Recall mean and s.d. per model', subtitle = 'Reference curve: F2 = 0.5')
  print(pr_plot)
}

roc_data <- function(tests) multi_roc(tests) %>%
  plot_roc_data() %>%
  tidy_roc_pr_data('Sensitivity', 'Specificity') %>%
  mutate( #sens to TPR
    metric1_mean = 1 - metric1_mean,
    metric1_sd = 1 - metric1_sd
  )

pr_data <- function(tests) multi_pr(tests) %>%
  plot_pr_data() %>%
  tidy_roc_pr_data('Recall', 'Precision')

tidy_roc_pr_data <- function(data, col1, col2) {
  groups <- data %>%
    select(Group) %>%
    distinct() %>%
    filter(Group %>% startsWith("RISK"))

  for_mean <- tibble(temp = numeric(404))
  for (group in groups$Group) {
    d <- data %>% filter(Group == group)
    for_mean[paste0(col1, "_", group)] <- d[col1]
    for_mean[paste0(col2, "_", group)] <- d[col2]
  }
  for_mean$temp <- NULL

  tidy <- tibble(metric1_mean = 0, metric2_mean = 0, metric1_sd = 0, metric2_sd = 0)
  for (i in seq(nrow(for_mean))) {
    metric1 <- as.numeric(for_mean[i,] %>% select(starts_with(col1)))
    metric2 <- as.numeric(for_mean[i,] %>% select(starts_with(col2)))
    tidy %<>% add_row(
      metric1_mean = mean(metric1), metric2_mean = mean(metric2),
      metric1_sd = sd(metric1), metric2_sd = sd(metric2)
    )
  }
  tidy %>% tail(-1)
}

fissure_plot <- function(data) ggplot(data, aes(x = metric1_mean, y = metric2_mean, color = group)) +
  geom_point() +
  geom_ribbon(aes(ymin = pmax(metric2_mean - metric2_sd, 0), ymax = pmin(metric2_mean + metric2_sd, 1),
                  fill = group),
              alpha = 0.3, linetype = 0) +
  ylim(0, 1) +
  xlim(0, 1) +
  theme_ipsum_rc()

get_tests <- function(method) {
  data_info <- readRDS("reports/results/models_and_evals/glmnet:::MODE_PRESENCE:::LABEL_H:::SYMP_TRUE:::CV_TRUE/data_info.rds")

  class_to_num_p1_n0 <- function(label) if (label == 'POSITIVE') 1 else 0

  ts <- seq_len(nrow(data_info)) %>% map(
    function(i) get_test_from_saved_model(method, data_info$splits[[i]], i, 403) %>%
      mutate(RISK_pred_model = POSITIVE) %>%
      mutate(RISK_true = map(obs, class_to_num_p1_n0))
  )

  tests <- data.frame(ts[[1]])
  for (i in seq_len(nrow(data_info))) {
    risk_pred <- paste("RISK", i, "pred", method, sep = "_")
    risk_true <- paste("RISK", i, "true", sep = "_")
    t <- data.frame(ts[[i]])
    tests[risk_pred] <- t["RISK_pred_model"]
    tests[risk_true] <- t["RISK_true"]
  }
  tests %>% select(-c("pred", "POSITIVE", "NEGATIVE", "obs", "RISK_pred_model", "RISK_true"))
}