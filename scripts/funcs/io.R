MODELS_DIR <- "reports/results/models_and_evals"
MODEL_RDS <- "model.rds"
SUMMARY_ID <- "summary"
PERFORMANCE_RDS <- "performance.rds"
PERFORMANCE_CSV <- "performance.csv"
DATA_INFO_RDS <- "data_info.rds"
SUMMARY_FOLDER_NAME <- "reports/results/models_and_evals/summary"
PERFORMANCE_FILE_NAME <- file.path(SUMMARY_FOLDER_NAME, "performance.csv")
SUMMARY_FILE_NAME <- file.path(SUMMARY_FOLDER_NAME, "summary.csv")
BEST_VARS_DESCR_FILE_NAME <- file.path(SUMMARY_FOLDER_NAME, "best_variables.csv")
SIGFIG <- 5

# make tibbles display 5 significant digits
options(pillar.sigfig = SIGFIG)

make_config_dirs <- function(ids) {
  ids %>% map(~dir.create(file.path(MODELS_DIR, .), recursive = TRUE))
  return
}

make_summary_dir <- function() make_config_dirs(SUMMARY_ID)

get_model_id <- function(method, mode, label, symptoms, cv) paste(method, mode, label, paste0("SYMP_", symptoms), paste0("CV_", cv), sep = ":::")

split_model_id <- function(id) {
  words <- stringr::str_split(id, ":::")[[1]]
  method <- words[1]
  mode <- words[2]
  label <- words[3]
  symptoms <- as.logical(stringr::str_split(words[4], pattern = "_")[[1]][2])
  cv <- as.logical(stringr::str_split(words[5], pattern = "_")[[1]][2])
  tibble(method = method, mode = mode, label = label, symptoms = symptoms, cv = cv)
}

id_to_path <- function(id, name) file.path(MODELS_DIR, id, name)

save_csv <- function(obj, id, name) write_csv(obj, id_to_path(id, name))

read_csv_ <- function(id, name) read_csv(id_to_path(id, name)) #coltypes = cols()

save_rds <- function(obj, id, name) saveRDS(obj, id_to_path(id, name))

read_rds <- function(id, name) readRDS(id_to_path(id, name))

save_model <- function(model, id) save_rds(model, id, MODEL_RDS)

read_model <- function(id) read_rds(id, MODEL_RDS)

save_data_info <- function(data_info, id) save_rds(data_info, id, DATA_INFO_RDS)

read_data_info <- function(id) read_rds(id, DATA_INFO_RDS)

read_best_vars_manual_descrs <- function() read_csv(paste0(SUMMARY_FOLDER_NAME, "/best_variables_manual_descr.csv"))

save_evaluation_csv <- function(summary, id) save_csv(summary, id, PERFORMANCE_CSV)

save_evaluation <- function(evaluation, id) {
  evaluation$id <- id
  evaluation <- evaluation %>% select(id, everything()) # put id first
  save_csv(evaluation, id, PERFORMANCE_CSV)
}

save_and_plot_var_imps <- function(method, var_imps_cols, var_imps_heatmap, var_imps_short_summary) {
  readr::write_rds(var_imps_short_summary, paste0(SUMMARY_FOLDER_NAME, "/var_imp_", method, ".rds"))

  print(var_imps_heatmap)
      ggsave(paste0(SUMMARY_FOLDER_NAME, "/var_imp_resample_", method, ".pdf"), units = "px", width = 2000, height = 1100, dpi = 250)

  print(var_imps_cols)
  ggsave(paste0(SUMMARY_FOLDER_NAME, "/var_imp_", method, ".pdf"), units = "px", width = 2000, height = 1100, dpi = 250)
}

print_section <- function(message, id = "") {
  cat("\n\n\n\n")
  cat("--> -------------------------------------------------------------------------------------------------------------\n")
  cat("--> ----------", paste0(message, ":::", id, sep = " "), "\n")
  cat("--> -------------------------------------------------------------------------------------------------------------\n")
}