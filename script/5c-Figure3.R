#### PACKAGES #### 
p <- c("ggplot2", "visreg", "patchwork")
lapply(p, library, character.only = T)

#### DATA #### 
# species richness models 
ab <- readRDS("large/AbundanceDistModels.rds")
ab50 <- ab$AbDist50
sr <- readRDS("large/SpeciesRichnessDistModels.rds")
sr1000 <- sr$SRDist1000
# shannon models
sh <- readRDS("large/ShannonDistModels.rds")
sh1000 <- sh$ShDist1000

#### PLOT #### 
## Abundance
ab50a <- visreg(ab50, xvar = "anthroper", line.par = list(col = "black"), gg = T) +
  geom_point(size = 2) +
  labs(y = "Abundance", x = "Urbanization (%)")+
  theme_classic()
ab50b <- visreg(ab50, xvar = "Disturbance", line.par = list(col = "black"), gg = T) +
  labs(y = "Abundance", x = "")+
  theme_classic()
ab50c <- visreg(ab50, xvar = "anthroper", by= "Disturbance", overlay=TRUE, band = T, gg = T) +
  geom_point(size = 3, aes(colour = Disturbance))+
  labs(y = "Abundance", x = "Urbanization (%)") + 
  scale_colour_viridis_d(name = "Disturbance") +
  scale_fill_viridis_d(name = "Disturbance", alpha = 0.3)+
  theme_classic() + 
  theme(legend.position = c(0.8, 0.9))

p0 <- (ab50a / ab50b) | ab50c + plot_annotation(tag_levels = 'A')

## Species Richness 
sr1000a <- visreg(sr1000, xvar = "anthroper", line.par = list(col = "black"), gg = T) +
  geom_point(size = 2) +
  labs(y = "Species Richness", x = "Urbanization (%)")+
  theme_classic()
sr1000m <- visreg(sr1000, xvar = "Disturbance", line.par = list(col = "black"), gg = T) +
  labs(y = "Species Richness", x = "")+
  theme_classic()

p1 <- sr1000m + sr1000a + plot_annotation(tag_levels = 'A')

## Shannon Diversity
sh1000m <- visreg(sh1000, xvar = "Disturbance", line.par = list(col = "black"), gg = T) +
  labs(y = "Shannon diversity", x = "")+
  theme_classic()
sh1000a <- visreg(sh1000, xvar = "anthroper", line.par = list(col = "black"), gg = T) +
  geom_point(size = 2) +
  labs(y = "Shannon diversity", x = "Urbanization (%)")+
  theme_classic()
sh1000i <- visreg(sh1000, xvar = "anthroper", by= "Disturbance", overlay=TRUE, band = T, gg = T) +
  geom_point(size = 3, aes(colour = Disturbance))+
  labs(y = "Shannon diversity", x = "Urbanization (%)") + 
  scale_colour_viridis_d(name = "Disturbance") +
  scale_fill_viridis_d(name = "Disturbance", alpha = 0.3)+
  theme_classic() + 
  theme(legend.position = c(0.8, 0.9))

p3 <- ((sh1000m/sh1000a) | sh1000i) + plot_annotation(tag_levels = 'A')

sh100a <- visreg(sh100, xvar = "anthroper", line.par = list(col = "black"), gg = T) +
  geom_point(size = 2) +
  labs(y = "Shannon diversity", x = "Urbanization (%)")+
  theme_classic()
sh100i <- visreg(sh100, xvar = "anthroper", by= "Disturbance", overlay=TRUE, band = T, gg = T) +
  geom_point(size = 3, aes(colour = Disturbance))+
  labs(y = "Shannon diversity", x = "Urbanization (%)") + 
  scale_colour_viridis_d(name = "Disturbance") +
  scale_fill_viridis_d(name = "Disturbance", alpha = 0.3)+
  theme_classic() + 
  theme(legend.position = c(0.8, 0.9))

p4 <- sh1000a + sh1000i + plot_annotation(tag_levels = 'A')


#### SAVE ####
png("figures/AbundanceModels.png", width = 6000, height = 5000, units = "px", res = 1000)
p0
dev.off()

png("figures/SpeciesRichnessModels.png", width = 6000, height = 3000, units = "px", res = 1000)
p1
dev.off()

png("figures/Shannon1000mModel.png", width = 6000, height = 5000, units = "px", res = 1000)
p3
dev.off()

png("figures/Shannon100mModel.png", width = 6000, height = 3000, units = "px", res = 1000)
p4
dev.off()
