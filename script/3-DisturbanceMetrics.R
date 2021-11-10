#### PACKAGES #### 
p <- c("readr", "dplyr")
lapply(p, library, character.only = T)

#### DATA ####
dist <- read_csv("input/SamplingArea.csv")

#### CLASSIFICATION #### 
# set NA = 0
dist$PerCut[is.na(dist$PerCut)] <- 0 
# ponds with >= 30% mowed, classify them as disturbed 
# ponds with < 30% mowed, classify them as undisturbed 
dist <- dist %>% 
  mutate(Disturbance = if_else(PerCut >= 30, "Disturbed", "Undisturbed"))


# save 
write_csv(dist, "output/SamplingAreaClean.csv")
