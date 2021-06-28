features <- features %>% mutate_if(is.character, as.factor)
factorFeatures <- features[, sapply(features, is.factor)]
factorLevels <- as_tibble(as.list(sapply(factorFeatures, nlevels))) %>%
  gather(key = "factor", value = "levels")
sink("../../reports/factor_levels_discarded")
factorLevels %>%
  filter(levels > 25) %>%
  print(n = nrow(.))
sink()
(factorLevels <- factorLevels %>% filter(levels <= 25))