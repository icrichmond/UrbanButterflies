#### PACKAGES #### 
p <- c("readr", "dplyr", "tidyr", "stringr")
lapply(p, library, character.only = T)

#### DATA ####
plantraw <- read_csv("input/PlantRawData.csv")

#### CLEAN-UP ####
plantraw <- rename(plantraw, Pond = X1)
plantraw <- plantraw %>%
  mutate(across(q1:q20, as.numeric))
# remove rows where all Q1->Q20 are NA 
plant <- plantraw %>% 
  filter(if_any(q1:q20, ~ !is.na(.)))
# remaining zeroes are NAs, replace them as such 
plant <- plant %>% 
  mutate(across(q1:q20, na_if, 0))
# assign appropriate data types
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
  group_by(Pond, year, month, day) %>%
  summarise(nspecies = n_distinct(scientific_name))

## Native Species Richness 
pnsp <- plant %>% 
  filter(str_detect(Native, "n")) %>% 
  group_by(Pond, year, month, day) %>%
  summarise(nnative = n_distinct(scientific_name))
plantclean <- inner_join(pnsp, plantclean)

## Percent Native Species
plantclean <- plantclean %>% 
  mutate(pernatsp = (nnative/nspecies))

#### PERCENT BLOOMING ####
## Average Percent Bloom
# average by quadrat
papb <- plant %>% 
  filter(!str_detect(scientific_name, "PATH|NoBlooming")) %>% 
  group_by(Pond, year, month, day) %>%
  summarise(across(q1:q20, ~sum(.x, na.rm = TRUE))) %>%
  mutate(avgbloom = rowMeans(across(q1:q20), na.rm = TRUE)) %>%
  select(c(Pond, year, month, day, avgbloom))
plantclean <- inner_join(plantclean, papb)  

## Average Percent Native Bloom
papnb <- plant %>% 
  filter(!str_detect(scientific_name, "PATH|NoBlooming")) %>% 
  group_by(Pond, year, month, day) %>%
  summarise(across(q1:q20, ~ sum(.x[which(Native == 'n')], na.rm = TRUE))) %>%
  mutate(avgnatbloom = rowMeans(across(q1:q20), na.rm = TRUE)) %>%
  select(c(Pond, year, month, day, avgnatbloom))
plantclean <- inner_join(plantclean, papnb)  

## Ratio of Native Blooming:Non-Native Blooming
plantclean <- plantclean %>%
  mutate(pernatbloom = (avgnatbloom/avgbloom))

#### SITE ONLY #### 
# make dataset that takes the average across dates for each site 
plantclean_site <- plant %>% 
  filter(!str_detect(scientific_name, "PATH|NoBlooming")) %>%
  group_by(Pond) %>%
  summarise(nspecies = n_distinct(scientific_name), nnative = length(unique(scientific_name[Native == 'n'])))  

plantcleansum <- plantclean %>% 
  group_by(Pond) %>%
  summarise_at(vars(pernatsp:pernatbloom), mean) 

plantclean_site <- inner_join(plantclean_site, plantcleansum)

#### SAVE #### 
write_csv(plantclean_site, "output/PlantCleanbySite.csv")
write_csv(plantclean, "output/PlantCleanbyDate.csv")
