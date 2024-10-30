# Packages ----------------------------------------------------------------
p <- c("data.table", "readr", "dplyr", "tidyr", "stringr", 
       "tibble", "purrr", "sf", "iNEXT", "ggplot2", "vegan",
       "sjPlot", "marginaleffects", "performance", "forcats",
       "osmdata", "ggmap", "ggspatial", "patchwork", "modelsummary")
lapply(p, library, character.only = T)
