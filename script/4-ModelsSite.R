# This script is for the models with the data pooled across dates and separated only by site 

#### PACKAGES #### 
p <- c("readr", "dplyr", "lme4", "AICcmodavg")
lapply(p, library, character.only = T)

#### DATA #### 
buttsit <- read_csv("output/ButterflyCleanbySite.csv")
plantsit <- read_csv("output/PlantCleanbySite.csv")
dist <- read_csv("output/SamplingAreaClean.csv")

#### DATA PREP ####
# match pond names 
buttsit <- buttsit %>% 
  rename(Pond = SWP) %>% 
  mutate(Pond = paste0("SWF-", "", Pond))
dist <- mutate(dist, Pond = paste0("SWF-", "", Pond))
# join datasets
sitefull <- inner_join(buttsit, plantsit)
sitefull <- inner_join(sitefull, dist)
# assign factor variables 
sitefull <- sitefull %>%
  mutate(across(c(Pond, Disturbance), as.factor))


#### COLLINEARITY ####
# check for collinearity between numeric independent variables
expl <- dplyr::select(sitefull, c(SamplingArea, nnative, nspecies, pernatsp, avgbloom, avgnatbloom, pernatbloom))
corr <- cor(expl, method = c("spearman"))
# highly correlated (r > 0.60) are nspecies-nnative, nnative-pernatsp, pernatsp-pernatbloom, avgbloom-avgnatbloom, avgnatbloom-pernatbloom
# the multiple measures of native species are correlated, make sure they are not in the same models

#### SPATIAL AUTOCORRELATION ####


#### MODELS #### 
## Shannon Diversity ##
# disturbance model 
shan.dist <- lm(Shannon ~ Disturbance + SamplingArea, data = sitefull)
# native species model 
shan.nat <- lm(Shannon ~ nnative + avgnatbloom, data = sitefull)
# full species model 
shan.plant <- lm(Shannon ~ nspecies + avgbloom, data = sitefull)
# null 
shan.null <- lm(Shannon ~ 1, data = sitefull)

shan.mods <- list(shan.dist, shan.nat, shan.plant, shan.null)
aictab(shan.mods)
