# This script is for the models with the data pooled across dates and separated only by site 

#### PACKAGES #### 
p <- c("readr", "dplyr", "purrr", "sf", "lme4", "AICcmodavg")
lapply(p, library, character.only = T)

#### DATA #### 
buttsit <- read_csv("output/ButterflyCleanbySite.csv")
plantsit <- read_csv("output/PlantCleanbySite.csv")
dist <- readRDS("large/AnthroFull.rds")

#### DATA PREP ####
# match pond names 
buttsit <- buttsit %>% 
  rename(Pond = SWP) %>% 
  mutate(Pond = paste0("SWF-", "", Pond))
dist <- purrr::map(.x = dist, .f = function(x){x %>% mutate(Pond = paste0("SWF-", "", Pond)) %>% select(-geometry.y) %>% rename(geometry = geometry.x)})
# join datasets
sitefull <- inner_join(buttsit, plantsit)
b <- c(20, 50, 100, 200, 500, 1000, 2000, 5000)
sitefull <- purrr::map(.x = dist, .f = function(x){inner_join(x, sitefull, by = "Pond")}) %>% 
  purrr::set_names(., nm = paste0("full",b))
# assign factor variables 
sitefull <- purrr::map(.x = sitefull, .f = function(x){x %>% mutate(across(c(Pond, Disturbance), as.factor))})
# assign numeric variables 
sitefull <- purrr::map(.x = sitefull, .f = function(x){x %>% mutate(across(c(totarea, settarea, roadarea, anthroarea, settper, roadper, anthroper, PerCut), as.numeric))})
# remove geometry
sitefull <- purrr::map(.x = sitefull, .f = function(x){st_set_geometry(x, NULL)})

#### COLLINEARITY ####
# check for collinearity between numeric independent variables
expl <- purrr::map(.x = sitefull, .f = function(x){x %>% dplyr::select(c(nnative, nspecies, pernatsp, avgbloom, avgnatbloom, pernatbloom, PerCut, settarea, roadarea, anthroarea, settper, roadper, anthroper))})
corr <- purrr::map(.x = expl, .f = function(x){cor(x, method = c("spearman"))})
# highly correlated (r > 0.60) are nspecies-nnative, nnative-pernatsp, pernatsp-pernatbloom, avgbloom-avgnatbloom, avgnatbloom-pernatbloom
# road and settlement are always highly correlated with overall anthropogenic land-use measures
# road and settlement become highly correlated at 500 m + buffers

#### SPATIAL AUTOCORRELATION ####


#### MODELS #### 
## Abundance ## 
# disturbance model 
ab.dist <- purrr::map(.x = sitefull, .f = function(x){lm(abund ~ PerCut * anthroper, data = x)}) %>% 
  purrr::set_names(., nm = paste0("AbDist", b))
# native species model 
ab.nat <- purrr::map(.x = sitefull, .f = function(x){lm(abund ~ nnative * avgnatbloom, data = x)}) %>% 
  purrr::set_names(., nm = paste0("AbNat", b))
# full species model 
ab.sp <- purrr::map(.x = sitefull, .f = function(x){lm(abund ~ nspecies * avgbloom, data = x)}) %>% 
  purrr::set_names(., nm = paste0("AbSp", b))
# null 
ab.null <- purrr::map(.x = sitefull, .f = function(x){lm(abund ~ 1, data = x)}) %>% 
  purrr::set_names(., nm = paste0("AbNull", b))

## Species Richness ## 
# disturbance model
sr.dist <- purrr::map(.x = sitefull, .f = function(x){lm(SpeciesRichness ~ PerCut * anthroper, data = x)}) %>% 
  purrr::set_names(., nm = paste0("SRDist", b))
# native species model 
sr.nat <- purrr::map(.x = sitefull, .f = function(x){lm(SpeciesRichness ~ nnative * avgnatbloom, data = x)}) %>% 
  purrr::set_names(., nm = paste0("SRNat", b))
# full species model 
sr.sp <- purrr::map(.x = sitefull, .f = function(x){lm(SpeciesRichness ~ nspecies * avgbloom, data = x)}) %>% 
  purrr::set_names(., nm = paste0("SRSp", b))
# null 
sr.null <- purrr::map(.x = sitefull, .f = function(x){lm(SpeciesRichness ~ 1, data = x)}) %>% 
  purrr::set_names(., nm = paste0("SRNull", b))

## Shannon Diversity ##
sh.dist <- purrr::map(.x = sitefull, .f = function(x){lm(Shannon ~ PerCut * anthroper, data = x)}) %>% 
  purrr::set_names(., nm = paste0("ShDist", b))
# native species model 
sh.nat <- purrr::map(.x = sitefull, .f = function(x){lm(Shannon ~ nnative * avgnatbloom, data = x)}) %>% 
  purrr::set_names(., nm = paste0("ShNat", b))
# full species model 
sh.sp <- purrr::map(.x = sitefull, .f = function(x){lm(Shannon ~ nspecies * avgbloom, data = x)}) %>% 
  purrr::set_names(., nm = paste0("ShSp", b))
# null 
sh.null <- purrr::map(.x = sitefull, .f = function(x){lm(Shannon ~ 1, data = x)}) %>% 
  purrr::set_names(., nm = paste0("ShNull", b))

## Diagnostics ## 
source("script/function-ResidPlots.R")
# abundance
ab <- list(ab.dist, ab.nat, ab.sp, ab.null)
ab <- purrr::map(.x = ab, .f = function(x){imap(x, resid_plots)})
pdf("figures/Diagnostics/abundance_glm_diagnostics.pdf")
ab
dev.off()

# species richness 
sr <- list(sr.dist, sr.nat, sr.sp, sr.null)
sr <- purrr::map(.x = sr, .f = function(x){imap(x, resid_plots)})
pdf("figures/Diagnostics/richness_glm_diagnostics.pdf")
sr
dev.off()

# Shannon diversity
sh <- list(sh.dist, sh.nat, sh.sp, sh.null)
sh <- purrr::map(.x = sh, .f = function(x){imap(x, resid_plots)})
pdf("figures/Diagnostics/shannon_glm_diagnostics.pdf")
sh
dev.off()

# all diagnostics look good 

## Model Ranking ##
