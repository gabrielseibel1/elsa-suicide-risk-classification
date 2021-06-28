library(rsample)
library(caret)
source("../funcs/data.R")

ext_folds_number <- 3
ext_folds_repeat <- 2
int_folds_number <- 5
int_folds_repeat <- 2

train_data <- get_data(MODE_INCIDENCE, LABEL_H9, FALSE)

ext_folds_rsample <- vfold_cv(train_data, v = ext_folds_number, repeats = ext_folds_repeat, strata = Class)
ext_folds_caret <- rsample2caret(ext_folds_rsample)

nested_folds <- nested_cv(train_data,
                          outside = ext_folds_rsample,
                          inside = vfold_cv(v = int_folds_number, repeats = int_folds_repeat, strata = Class))

ext_splits <- nested_folds
for (i in seq(nrow(ext_splits))) {
  ext_folds <- ext_splits$splits[[i]]
  int_splits <- ext_splits$inner_resamples[[i]]
  ext_train <- analysis(ext_folds)
  ext_test <- assessment(ext_folds)
  cat("Outside:", "<#train/#test> =", nrow(ext_train), nrow(ext_test), "\n")


  for (j in seq(nrow(int_splits))) {
    int_folds <- int_splits$splits[[j]]
    int_train <- training(int_folds)
    int_test <- testing(int_folds)
    int_folds_caret <- rsample2caret(int_splits)
    cat("\t\tInside:", "<#train/#test> =", nrow(int_train), nrow(int_test), "\n")


  }
}
