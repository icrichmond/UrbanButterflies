# This script is for the models with the data separated by site AND date 

#### PACKAGES #### 
p <- c("readr", "dplyr", "anytime", "lme4", "AICcmodavg")
lapply(p, library, character.only = T)

#### DATA #### 
buttdat <- read_csv("output/ButterflyCleanbyDate.csv")
plantdat <- read_csv("output/PlantCleanbyDate.csv")
area <- read_csv("output/SamplingAreaClean.csv")

#### DATA PREP ####
# change pond names to match plant dataset
area <- mutate(area, Pond = paste0("SWF-", "", Pond))
buttdat <- buttdat %>% 
  rename(Pond = SWP) %>% 
  mutate(Pond = paste0("SWF-", "", Pond))
# convert date columns into correct format 
buttdat$Date <- gsub("-18", "-2018", buttdat$Date)
buttdat$Date <- anydate(buttdat$Date)

plantdat <- plantdat %>%
  mutate(Date = paste0(year, "-", month, "-", day)) %>% 
  mutate(Date = anydate(Date))
# join by pond and date - will lose first round of butterfly data because there is no plant data on those dates 
datfull <- inner_join(buttdat, plantdat, by = c("Pond", "Date"))
datfull <- inner_join(datfull, area, by = "Pond")
datfull <- datfull %>%
  mutate(across(c(Pond, Disturbance), as.factor))

#### COLLINEARITY ####
# check for collinearity between numeric independent variables
expl <- dplyr::select(datfull, c(SamplingArea, nnative, nspecies, pernatsp, avgbloom, avgnatbloom, pernatbloom))
corr <- cor(expl, method = c("spearman"))
# highly correlated (r > 0.60) are nnative-pernatsp, pernatsp-pernatbloom, avgnatbloom-pernatbloom
# the multiple measures of native species are correlated, make sure they are not in the same models

#### SPATIAL AUTOCORRELATION ####


#### MODELS #### 
# note: only have one disturbance measurement for the whole sampling period, so can't break it down by date
## Shannon Diversity ##


## Abundance ## 


## Species Richness ## 

