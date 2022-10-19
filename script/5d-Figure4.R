#### PACKAGES #### 
p <- c("ggplot2", "purrr", "dplyr", "tidyr", "stringr")
lapply(p, library, character.only = T)

#### DATA ##### 
# abundance models 
ab <- readRDS("large/AbundanceTopModels.rds")
# species richness models 
sr <- readRDS("large/SpeciesRichnessTopModels.rds")
# shannon models 
sh <- readRDS("large/ShannonTopModels.rds")

#### CLEANING #### 
# extract R^2 values 
abr2 <- purrr::map_dfr(.x = ab, .f = function(x){summary(x)$r.squared})
srr2 <- purrr::map_dfr(.x = sr, .f = function(x){summary(x)$r.squared})
shr2 <- purrr::map_dfr(.x = sh, .f = function(x){summary(x)$r.squared})
# pivot datasets    
abr2 <- pivot_longer(abr2, cols = everything(), names_to = "model", values_to = "rsquared")
srr2 <- pivot_longer(srr2, cols = everything(), names_to = "model", values_to = "rsquared")
shr2 <- pivot_longer(shr2, cols = everything(), names_to = "model", values_to = "rsquared")
# bind
r2 <- rbind(abr2, srr2, shr2)
r2 <- r2%>% 
  mutate(modeltype = case_when(str_detect(model, 'Ab') ~ 'Abundance',
                               str_detect(model, 'SR') ~ 'SpeciesRichness',
                               str_detect(model, 'Sh') ~ 'Shannon')) %>% 
  mutate(distance = str_replace_all(model, c("AbDist" = "", "SRDist" = "", "ShDist" = "")) )
r2$distance <- as.numeric(r2$distance)
r2$distance <- as.factor(r2$distance)

#### PLOT #### 
ggplot(r2, aes(x = distance, y = rsquared, group = modeltype)) +
  geom_point(aes(colour = modeltype)) + 
  geom_line(aes(colour = modeltype)) + 
  scale_colour_viridis_d(option = 'plasma', name = "Model Type")+
  theme_classic() + 
  labs(y = "R^2 of top model", x = "Buffer Distance (m)")
ggsave("figures/Rsquared.jpg", width = 4542, height = 2882, units = "px", dpi = 450)

