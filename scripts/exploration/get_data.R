library(tidyverse)
library(tibble)
library(haven)

features <- read_dta("data/baseline_features.dta")
questions_w1 <- read_dta("data/questions_cisr_wave1.dta")
questions_w2 <- read_dta("data/questions_cisr_wave2.dta")
features
questions_w1
questions_w2

names(questions_w2) <- tolower(names(questions_w2))

outcomes <- questions_w1 %>%
  right_join(
    questions_w2, by = "idelsa"
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
  ) %>%
  select(
    c(idelsa, h6_w1, h8_w1, h9_w1, h6_w2, h8_w2, h9_w2)
  )
outcomes
summary(outcomes)

outcomes %<>%
  mutate(
    h6 = h6_w1 | h6_w2,
    h8 = h8_w1 | h8_w2,
    h9 = h9_w1 | h9_w2,
    ih6 = !h6_w1 & h6_w2,
    ih8 = !h8_w1 & h8_w2,
    ih9 = !h9_w1 & h9_w2
  ) %>%
  select(
    c(idelsa, h6, h8, h9, ih6, ih8, ih9)
  )
summary(outcomes)

ggplot(data = outcomes) +
  geom_point(mapping = aes(x = idelsa, y = h6), size = 8, color = "deepskyblue") +
  geom_point(mapping = aes(x = idelsa, y = h8), size = 4, color = "darkorchid") +
  geom_point(mapping = aes(x = idelsa, y = h9), size = 2, color = "darkorange") +
  theme_minimal()

ggplot(data = outcomes) +
  geom_point(mapping = aes(x = idelsa, y = ih6), size = 8, color = "green") +
  geom_point(mapping = aes(x = idelsa, y = ih8), size = 4, color = "blue") +
  geom_point(mapping = aes(x = idelsa, y = ih9), size = 2, color = "red") +
  theme_minimal()

outcomes %>%
  gather(variable, value, -idelsa) %>%
  filter(value == TRUE) %>%
  ggplot(aes(idelsa, variable, col = variable)) +
  geom_point() +
  theme_minimal()

outcomes$h6 <- factor(outcomes$h6, levels = c(TRUE, FALSE), labels = c("H6_TRUE", "H6_FALSE"))
outcomes$h8 <- factor(outcomes$h8, levels = c(TRUE, FALSE), labels = c("H8_TRUE", "H8_FALSE"))
outcomes$h9 <- factor(outcomes$h9, levels = c(TRUE, FALSE), labels = c("H9_TRUE", "H9_FALSE"))
outcomes$ih6 <- factor(outcomes$ih6, levels = c(TRUE, FALSE), labels = c("IH6_TRUE", "IH6_FALSE"))
outcomes$ih8 <- factor(outcomes$ih8, levels = c(TRUE, FALSE), labels = c("IH8_TRUE", "IH8_FALSE"))
outcomes$ih9 <- factor(outcomes$ih9, levels = c(TRUE, FALSE), labels = c("IH9_TRUE", "IH9_FALSE"))

analyseNAs <- function(df) {
  nas <- as_tibble(map(df, ~sum(is.na(.)))) %>%
    gather(key = "variable", value = "NAs")
  nas$NAs <- nas$NAs / nrow(df)
  nasPlot <- ggplot(data = nas, mapping = aes(x = variable, y = NAs)) +
    geom_bar(stat = "identity") +
    theme_minimal()
  print(nasPlot)
  return(nas)
}

cutNAs <- function(df, nas, threshold = 0.1) {
  sink("../../reports/NA_discarded")
  nas %>%
    filter(NAs > threshold) %>%
    print(n = nrow(.))
  sink()
  (df <- df[, filter(nas, NAs <= threshold)$variable])
  return(df)
}

nas <- analyseNAs(features)
features <- cutNAs(features, nas)
nas <- analyseNAs(features)

# see why removed non-numeric in scripts/factor_analysis.R
num_or_idelsa <- sapply(features, is.numeric)
num_or_idelsa["idelsa"] <- TRUE
features <- features[, num_or_idelsa]

features <- features %>% select(-"mentalvar_MDD_trajectories") # cant be predicted (?)

elsaH6 <- features %>%
  inner_join(outcomes[, c("idelsa", "h6")], by = "idelsa") %>%
  select(-"idelsa")
elsaH8 <- features %>%
  inner_join(outcomes[, c("idelsa", "h8")], by = "idelsa") %>%
  select(-"idelsa")
elsaH9 <- features %>%
  inner_join(outcomes[, c("idelsa", "h9")], by = "idelsa") %>%
  select(-"idelsa")
elsaIH6 <- features %>%
  inner_join(outcomes[, c("idelsa", "ih6")], by = "idelsa") %>%
  select(-"idelsa")
elsaIH8 <- features %>%
  inner_join(outcomes[, c("idelsa", "ih8")], by = "idelsa") %>%
  select(-"idelsa")
elsaIH9 <- features %>%
  inner_join(outcomes[, c("idelsa", "ih9")], by = "idelsa") %>%
  select(-"idelsa")