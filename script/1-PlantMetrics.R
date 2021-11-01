#### PACKAGES #### 
p <- c("readr", "dplyr", "tidyr", "stringr")
lapply(p, library, character.only = T)

#### DATA ####
plantraw <- read_csv("input/PlantRawData.csv")

# goals: average percent area in bloom, average percent area native in bloom, native:non-native (abundance), percent native species

#### CLEAN-UP ####
# remove rows where all Q1->Q20 are NA 
plantraw <- rename(plantraw, Pond = X1)
plant <- filter(plantraw, rowSums(is.na(plantraw)) != ncol(plantraw[,9:28]))
plant$Pond <- as.factor(plant$Pond)
plant$scientific_name <- as.factor(plant$scientific_name)
# reclassify no blooming category to all match (NA, No blooming, no blooming, none blooming)
plant$scientific_name <- recode(plant$scientific_name, "No blooming" = "NoBlooming")
plant$scientific_name <- replace_na(plant$scientific_name, "NoBlooming")

#### SPECIES RICHNESS #### 
## Total Species Richness 
# remove PATH and NoBlooming for this calculation since they are not real species 
plantclean <- plant %>% 
  filter(!str_detect(scientific_name, "PATH|NoBlooming")) %>%
  group_by(Pond) %>%
  summarise(nspecies = n_distinct(scientific_name))

## Native Species Richness 
plant <- plantraw %>% 
  filter(str_detect(Native, "n")) %>% 
  group_by(Pond) %>%
  summarise(nnative = n_distinct(scientific_name))
plantclean <- inner_join(plant, plantclean)

#### PERCENT BLOOMING ####
## Average percent bloom 
plant <- plantraw %>% 
  filter(!str_detect(scientific_name, "PATH|NoBlooming")) %>% 
  group_by(Pond, year, month, day) %>%
  summarise(bloom = rowMeans(9:28)) 
  
