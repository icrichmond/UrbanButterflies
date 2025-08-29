# Packages ----------------------------------------------------------------
p <- c("data.table", "readr", "dplyr", "tidyr", "stringr", 
       "tibble", "purrr", "sf", "iNEXT", "ggplot2", "vegan",
       "sjPlot", "marginaleffects", "performance", "forcats", "MASS",
       "osmdata", "ggmap", "ggspatial", "patchwork", "modelsummary")
lapply(p, library, character.only = T)
