# Packages ----------------------------------------------------------------
p <- c("data.table", "readr", "dplyr", "tidyr", "stringr", 
       "tibble", "purrr", "sf", "iNEXT", "ggplot2", "vegan",
       "lme4", "lmerTest", "marginaleffects", "performance",
       "osmdata", "ggmap", "ggspatial", "patchwork")
lapply(p, library, character.only = T)
