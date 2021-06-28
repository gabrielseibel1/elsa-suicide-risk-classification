get_configs_to_train <- function() {
  configs <- tibble(method = c(
    METHOD_NAIVE_BAYES,
    METHOD_GLMNET,
    METHOD_RF
  ))
  configs$mode <- MODE_PRESENCE
  configs$label <- LABEL_H
  configs$symptoms <- TRUE
  configs$cv <- TRUE
  configs$id <- pmap(list(configs$method, configs$mode, configs$label, configs$symptoms, configs$cv), get_model_id)
  configs
}

train_and_save_models_from_configs <- function(configs)
  pmap(list(configs$method, configs$mode, configs$label, configs$symptoms, configs$cv), train_model)

train_model <- function(method, mode, label, symptoms, cv, id) {
  id <- get_model_id(method, mode, label, symptoms, cv)
  data <- get_data(mode, label, symptoms)
  if (cv) { train_tune_evaluate_cv(data, method, id) }
  else { train_tune_evaluate_split(data, method, id) }
}

# parallel setup
cluster <- makeForkCluster(N_WORKERS, outfile = "")
doParallel::registerDoParallel(cl = cluster, cores = N_WORKERS)

# training
configs <- get_configs_to_train()
make_config_dirs(configs$id)
print_section("Models to train")
print(configs, n = nrow(configs))
train_and_save_models_from_configs(configs)

# parallel teardown
stopCluster(cluster)
