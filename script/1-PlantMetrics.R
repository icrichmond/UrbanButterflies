#### PACKAGES #### 
p <- c("readr", "dplyr", "tidyr", "stringr")
lapply(p, library, character.only = T)

#### DATA ####
plantraw <- read_csv("input/PlantRawData.csv")

# goals: plant species richness, native plant species richness, average percent area in bloom, average percent area native in bloom, native:non-native (abundance), percent native species

#### CLEAN-UP ####
# remove rows where all Q1->Q20 are NA 
plant <- filter(plantraw, rowSums(is.na(plantraw)) != ncol(plantraw[,9:28])) %>%
  rename(Pond = X1) 
plant$Pond <- as.factor(plant$Pond)
plant$scientific_name <- as.factor(plant$scientific_name)
# reclassify no blooming category to all match (NA, No blooming, no blooming, none blooming)
plant$scientific_name <- recode(plant$scientific_name, "No blooming" = "NoBlooming")
plant$scientific_name <- replace_na(plant$scientific_name, "NoBlooming")

#### SPECIES RICHNESS #### 
# remove PATH and NoBlooming for this calculation since they are not real species 
plantclean <- plant %>% 
  filter(!str_detect(scientific_name, "PATH|NoBlooming")) %>%
  group_by(Pond) %>%
  count(scientific_name)
# counting the same factor levels multiple times?