questions_cisr_w1_file <- "data/questions_cisr_wave1.dta"
questions_cisr_w2_file <- "data/questions_cisr_wave2.dta"
baseline_features_file <- "data/baseline_features.dta"
baseline_meds_file <- "data/base_med_o1_c_pa_150619.dta"

# constants
IDELSA <- "idelsa"
ANTIDEPRESSANTS <- "toma_antidepressivos"
LABEL_H6 <- "LABEL_H6" # for now, it's important to have the same value as the var name
LABEL_H8 <- "LABEL_H8" # for now, it's important to have the same value as the var name
LABEL_H9 <- "LABEL_H9" # for now, it's important to have the same value as the var name
LABEL_H <- "LABEL_H" # for now, it's important to have the same value as the var name
LABELS <- c(LABEL_H6, LABEL_H8, LABEL_H9, LABEL_H)
CLASS_LEVELS <- c(TRUE, FALSE)
CLASS_LABELS <- c("POSITIVE", "NEGATIVE")
MODE_PRESENCE <- "MODE_PRESENCE" # for now, it's important to have the same value as the var name
MODE_INCIDENCE <- "MODE_INCIDENCE" # for now, it's important to have the same value as the var name
MODE_PERSISTENCE <- "MODE_PERSISTENCE" # for now, it's important to have the same value as the var name
MODE_REMISSION <- "MODE_REMISSION" # for now, it's important to have the same value as the var name
MODES <- MODE_PRESENCE
NA_THRESHOLD <- 0.1

cross_df_list <- function(df, list, list_name) {
  mult <- NULL
  for (item in list) {
    temp_df <- df
    temp_df[list_name] <- item
    mult <- rbind(mult, temp_df)
  }
  mult
}

get_all_data_configs <- function(with_data = FALSE) {
  config <- tibble(mode = MODES)
  config <- cross_df_list(config, LABELS, "label")
  config <- cross_df_list(config, c(TRUE, FALSE), "symptoms")
  if (with_data) {
    config$data <- pmap(list(config$mode, config$label, config$symptoms), get_data)
  }
  config
}

get_data <- function(mode, label, symptoms) {
  data <- get_features(symptoms) %>%
    inner_join(get_outcomes(mode, label), by = IDELSA) %>%
    select(-all_of(IDELSA))
}

get_all_data_labels <- function() {
  features <- read_dta(baseline_features_file, encoding = "latin1")
  tibble(
    feature = colnames(features),
    label = features %>%
      purrr::map_chr(function(column) {
        label <- attr(column, 'label')
        if (!is.null(label)) label else '?'
      })
  )
}

get_features_descriptions <- function(names) {
  labels <- get_all_data_labels()

  descrs <- tibble(feature = names) %>%
    dplyr::distinct(feature) %>%
    dplyr::arrange(feature) %>%
    dplyr::left_join(labels, by = "feature") %>%
    dplyr::mutate(label = as.character(label))

  descrs
}


get_outcomes <- function(mode, label) {
  questions_w1 <- read_dta(questions_cisr_w1_file)
  questions_w2 <- read_dta(questions_cisr_w2_file)
  names(questions_w2) <- tolower(names(questions_w2))

  if (mode == MODE_PRESENCE) { # consider only questions from wave 1
    outcomes <- questions_w1 %>%
      mutate(
        !!LABEL_H6 := cisah6 != 0,
        !!LABEL_H8 := cisah8 != 0,
        !!LABEL_H9 := cisah9a != 0
      ) %>%
      replace_na(
        list(
          LABEL_H6 = FALSE,
          LABEL_H8 = FALSE,
          LABEL_H9 = FALSE
        )
      ) %>%
      mutate(
        !!LABEL_H := LABEL_H6 | LABEL_H8 | LABEL_H9
      )
  }
  else { # bind questions from wave 1 and 2
    outcomes <- questions_w1 %>%
      right_join(
        questions_w2, by = IDELSA
      ) %>%
      mutate(
        h6_w1 = cisah6 != 0,
        h6_w2 = cisbh6 != 0,
        h8_w1 = cisah8 != 0,
        h8_w2 = cisbh8 != 0,
        h9_w1 = cisah9a != 0,
        h9_w2 = cisbh9a != 0
      ) %>%
      replace_na(
        list(
          h6_w1 = FALSE,
          h6_w2 = FALSE,
          h8_w1 = FALSE,
          h8_w2 = FALSE,
          h9_w1 = FALSE,
          h9_w2 = FALSE
        )
      )

    if (mode == MODE_INCIDENCE) { # get only instances negative on wave 1
      outcomes <- switch(
        label,
        LABEL_H6 = outcomes %>% filter(!h6_w1),
        LABEL_H8 = outcomes %>% filter(!h8_w1),
        LABEL_H9 = outcomes %>% filter(!h9_w1),
        LABEL_H = outcomes %>% filter(!(h6_w1 | h8_w1 | h9_w1))
      ) %>%
        mutate( # see if labels appeared in wave 2
          !!LABEL_H6 := h6_w2,
          !!LABEL_H8 := h8_w2,
          !!LABEL_H9 := h9_w2,
          !!LABEL_H := h6_w2 | h8_w2 | h9_w2
        )
    }

    if (mode == MODE_PERSISTENCE | mode == MODE_REMISSION) { # get only instances positive on wave 1
      outcomes <- switch(
        label,
        LABEL_H6 = outcomes %>% filter(h6_w1),
        LABEL_H8 = outcomes %>% filter(h8_w1),
        LABEL_H9 = outcomes %>% filter(h9_w1),
        LABEL_H = outcomes %>% filter(h6_w1 | h8_w1 | h9_w1)
      )
      outcomes <- switch(
        mode,
        MODE_PERSISTENCE = outcomes %>% mutate( # see if label continued in wave 2
          !!LABEL_H6 := h6_w2,
          !!LABEL_H8 := h8_w2,
          !!LABEL_H9 := h9_w2,
          !!LABEL_H := h6_w2 | h8_w2 | h9_w2
        ),
        MODE_REMISSION = outcomes %>% mutate(
          !!LABEL_H6 := !h6_w2,
          !!LABEL_H8 := !h8_w2,
          !!LABEL_H9 := !h9_w2,
          !!LABEL_H := !(h6_w2 | h8_w2 | h9_w2)
        )
      )
    }
  }

  rm(questions_w1)
  rm(questions_w2)
  gc()

  outcomes %<>%
    rename(Class := !!label) %>%
    select(all_of(IDELSA), Class)

  outcomes$Class <- factor(outcomes$Class, levels = CLASS_LEVELS, labels = CLASS_LABELS)
  outcomes
}

get_features <- function(symptoms) {

  features <- read_dta(baseline_features_file, encoding = "latin1") %>%
    # remove features that would cause information leakage
    select(-c(
      mentalvar_A_ESCORETOTAL, # will be recomputed later
      # mentalvar_A_TMC, will be removed later
      mentalvar_A_TMCGRAV,
      mentalvar_a_DEP,
      mentalvar_A_DEPLEVSSINT,
      mentalvar_A_DEPLEVCSINT,
      mentalvar_A_DEPMODSSINT,
      mentalvar_A_DEPMODCSINT,
      mentalvar_A_DEPGRAVE,
      mentalvar_A_SINTIDEIADEP,
      mentalvar__TMAD,
      mentalvar_MDD_trajectories,
      mentalvar_only_incident,
      mentalvar_only_remitted,
      mentalvar_only_persistent
    )) %>%
    # add antidepressants information
    left_join(by = IDELSA, y = read_dta(baseline_meds_file)
      %>%
      select(
        all_of(IDELSA),
        all_of(ANTIDEPRESSANTS)
      )) %>%
    # add CIS-R variables except H6, H8 and H9
    left_join(by = IDELSA, y = read_dta(questions_cisr_w1_file) %>%
      select( # exclude outcomes
        -(cisah6:cisah9b)
      ) %>%
      select(-c( # exclude variables we can't impute
        cisaa8, # CIS-R A
        cisab3, cisab3a, cisab9, # CIS-R B
        cisac8, # CIS-R C
        cisad4, cisad4a, cisad10, # CIS-R D
        cisae4, cisae10, # CIS-R E
        cisag8a1:cisag8a12, cisag8b, cisag10, # CIS-R G
        cisai3a1:cisai3a12, cisai3b, cisai10, # CIS-R I
        cisaj9a1:cisaj9a7, cisaj11, # CIS-R J
        cisak3a1:cisak3a8, cisak3b1:cisak3b8, cisak5a1:cisak5a7, cisak8, # CIS-R K
        cisal5, cisal7, # CIS-R L
        cisam3, cisam6, cisam8, # CIS-R M
        cisan3, cisan8 # CIS-R N
      )) %>%
      mutate( # impute variables with HIGHEST option
        cisaa4 = replace_na(cisaa4, 3),
        cisaa6 = replace_na(cisaa6, 3),
        cisab4 = replace_na(cisab4, 3),
        cisac4 = replace_na(cisac4, 3),
        cisac5 = replace_na(cisac5, 1),
        cisad3 = replace_na(cisad3, 3),
        cisad6 = replace_na(cisad6, 3),
        cisad9 = replace_na(cisad9, 3),
        cisae3 = replace_na(cisae3, 3),
        cisae8 = replace_na(cisae8, 1),
        cisae9 = replace_na(cisae9, 1),
        cisaf3 = replace_na(cisaf3, 3),
        cisaf5 = replace_na(cisaf5, 3),
        cisaf6 = replace_na(cisaf6, 1),
        cisag2 = replace_na(cisag2, 1),
        cisag5 = replace_na(cisag5, 1),
        cisag6 = replace_na(cisag6, 3),
        cisag9 = replace_na(cisag9, 1),
        cisah1 = replace_na(cisah1, 1),
        cisah2 = replace_na(cisah2, 3),
        cisai6 = replace_na(cisai6, 3),
        cisai8 = replace_na(cisai8, 3),
        cisaj5 = replace_na(cisaj5, 2),
        cisaj6 = replace_na(cisaj6, 3),
        cisaj8 = replace_na(cisaj8, 3),
        cisak4 = replace_na(cisak4, 3),
        cisak7 = replace_na(cisak7, 3),
        cisal2 = replace_na(cisal2, 3),
        cisal3 = replace_na(cisal3, 3),
        cisam2 = replace_na(cisam2, 3),
        cisam7 = replace_na(cisam7, 3),
        cisan2 = replace_na(cisan2, 2),
        cisan4 = replace_na(cisan4, 3),
        cisan7 = replace_na(cisan7, 2)
      ) %>%
      modify2( # impute other variables with LOWEST option
        colnames(.),
        function(col, name) {
          if (name == IDELSA) col
          else replace_na(col, min(col, na.rm = TRUE))
        }
      ) %>% # recalculate mentalvar_A_ESCORETOTAL without h6, h8, h9
      mutate( # backup some variables we'll alter
        backup_cisac5 = cisac5,
        backup_cisae8 = cisae8,
        backup_cisae9 = cisae9,
        backup_cisaf6 = cisaf6,
        backup_cisag5 = cisag5,
        backup_cisag9 = cisag9
      ) %>%
      mutate_at(vars( # create inverted variables so we can sum with them
        cisac5, cisae8, cisae9, cisaf6, cisag5, cisag9
      ), ~(. - 1) * -1  # vector with 0s and 1s ==> [0 -> -1 -> 1, 1 -> 0 -> 0]
      ) %>%
      mutate( # for each row, the score is the sum of the values of some variables
        mentalvar_a_ESCORETOTAL = pmap_dbl(
          .f = sum,
          .l = select(
            .,
            cisaa4, cisaa5, cisaa6, cisaa7,
            cisab4, cisab5, cisab6, cisab7, cisab8,
            cisac4, cisac5, cisac6, cisac7,
            cisad5, cisad6, cisad8, cisad9,
            cisae3, cisae5, cisae6, cisae8, cisae9,
            cisaf3, cisaf4, cisaf6,
            cisag5, cisag6, cisag7, cisag9,
            cisah4, cisah5,
            cisai6, cisai7, cisai8, cisai9,
            cisaj6, cisaj8, cisaj9, cisaj10,
            cisak4, cisak5, cisak7,
            cisal2, cisal3, cisal4,
            cisam2, cisam4, cisam5, cisam7,
            cisan4, cisan5, cisan6, cisan7,
          )
        )
      ) %>%
      mutate( # restore from backup variables
        cisac5 = backup_cisac5,
        cisae8 = backup_cisae8,
        cisae9 = backup_cisae9,
        cisaf6 = backup_cisaf6,
        cisag5 = backup_cisag5,
        cisag9 = backup_cisag9
      ) %>%
      select( # remove backup variables
        -(backup_cisac5:backup_cisag9)
      )
    )

  # remove features above NA threshold
  nas <- as_tibble(map(features, ~sum(is.na(.)))) %>% gather(key = "variable", value = "NAs")
  nas$NAs <- nas$NAs / nrow(features)
  features <- features[, filter(nas, NAs <= NA_THRESHOLD)$variable]

  # see why removed non-numeric in scripts/exploration/factor_analysis.R
  num_or_idelsa <- sapply(features, is.numeric)
  num_or_idelsa[IDELSA] <- TRUE
  features[, num_or_idelsa] %>%
  { # remove rows that do not have TMC if restricting population
    if (symptoms) .[.$mentalvar_A_TMC == 1,]
    else .
  } %>%
    select(-mentalvar_A_TMC)
}

get_table1_data <- function() {
  outcome <- get_outcomes(MODE_PRESENCE, LABEL_H)
  data_t1 <- read_dta(baseline_features_file, encoding = "latin1") %>%
    filter(mentalvar_A_TMC == 1) %>%
    inner_join(outcome, by = IDELSA) %>%
    select(
      Class,
      sociodem_rcta8,
      sociodem_derived_A_SITCONJ,
      sociodem_idadea,
      sociodem_vifa30,
      sociodem_derived_A_ESCOLAR,
      derived_A_IMC1,
      sociodem_vifa29,
      discrimination_disa1ae, discrimination_disa2ae, discrimination_disa3ae, discrimination_disa4ae, discrimination_disa5ae,
      alcohol_cala2, sociodem_derived_A_FUMANTE,
      mentalvar_A_SINTDEP, mentalvar_A_SINTIDEIADEP, mentalvar_A_SINTPREOCUP, mentalvar_A_SINTANSIE, mentalvar_A_SINTFOBIAS,
      mentalvar_A_SINTPANICO, mentalvar_A_SINTCOMPUL, mentalvar_A_SINTOBSES
    ) %>%
    mutate(
      discrimination_disa1ae = replace_na(discrimination_disa1ae, 0),
      discrimination_disa2ae = replace_na(discrimination_disa2ae, 0),
      discrimination_disa3ae = replace_na(discrimination_disa3ae, 0),
      discrimination_disa4ae = replace_na(discrimination_disa4ae, 0),
      discrimination_disa5ae = replace_na(discrimination_disa5ae, 0)
    )

  summary(data_t1)
  saveRDS(data_t1, "data_t1.rds")

  data_t1
}