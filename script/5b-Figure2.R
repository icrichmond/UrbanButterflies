#### PACKAGES #### 
p <- c("ggplot2", "visreg", "patchwork")
lapply(p, library, character.only = T)

#### DATA #### 
# species richness models 
sr <- readRDS("large/SpeciesRichnessDistModels.rds")
sr1000 <- sr$SRDist1000
# shannon models
sh <- readRDS("large/ShannonDistModels.rds")
sh1000 <- sh$ShDist1000

#### PLOT #### 
## Species Richness 
sr1000m <- visreg(sr1000, xvar = "anthroper", line.par = list(col = "black"), gg = T) +
  geom_point(size = 2) +
  labs(y = "Species Richness", x = "Anthropogenic Area (%)")+
  theme_classic()

sr2000m <- visreg(sr2000, xvar = "anthroper", line.par = list(col = "black"), gg = T) +
  geom_point(size = 2) +
  labs(y = "", x = "Anthropogenic Area (%)")+
  theme_classic()

p2 <- sr1000m + sr2000m + plot_annotation(tag_levels = 'A')

## Shannon Diversity
sh1000m <- visreg(sh1000, xvar = "Disturbance", line.par = list(col = "black"), gg = T) +
  labs(y = "Shannon diversity", x = "")+
  theme_classic()
sh1000a <- visreg(sh1000, xvar = "anthroper", line.par = list(col = "black"), gg = T) +
  geom_point(size = 2) +
  labs(y = "Shannon diversity", x = "Anthropogenic Area (%)")+
  theme_classic()
sh1000i <- visreg(sh1000, xvar = "anthroper", by= "Disturbance", overlay=TRUE, band = T, gg = T) +
  geom_point(size = 3, aes(colour = Disturbance))+
  labs(y = "Shannon diversity", x = "Anthropogenic Land-Use (%)") + 
  scale_colour_viridis_d(name = "Disturbance") +
  scale_fill_viridis_d(name = "Disturbance", alpha = 0.3)+
  theme_classic() + 
  theme(legend.position = c(0.8, 0.9))

p3 <- ((sh1000m/sh1000a) | sh1000i) + plot_annotation(tag_levels = 'A')

sh100a <- visreg(sh100, xvar = "anthroper", line.par = list(col = "black"), gg = T) +
  geom_point(size = 2) +
  labs(y = "Shannon diversity", x = "Anthropogenic Area (%)")+
  theme_classic()
sh100i <- visreg(sh100, xvar = "anthroper", by= "Disturbance", overlay=TRUE, band = T, gg = T) +
  geom_point(size = 3, aes(colour = Disturbance))+
  labs(y = "Shannon diversity", x = "Anthropogenic Land-Use (%)") + 
  scale_colour_viridis_d(name = "Disturbance") +
  scale_fill_viridis_d(name = "Disturbance", alpha = 0.3)+
  theme_classic() + 
  theme(legend.position = c(0.8, 0.9))

p4 <- sh100a + sh100i + plot_annotation(tag_levels = 'A')


#### SAVE ####
png("figures/AbundanceModels.png", width = 6000, height = 3000, units = "px", res = 1000)
p1
dev.off()

png("figures/SpeciesRichnessModels.png", width = 6000, height = 3000, units = "px", res = 1000)
p2
dev.off()

png("figures/Shannon1000mModel.png", width = 6000, height = 5000, units = "px", res = 1000)
p3
dev.off()

png("figures/Shannon100mModel.png", width = 6000, height = 3000, units = "px", res = 1000)
p4
dev.off()
