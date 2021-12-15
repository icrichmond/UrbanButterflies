#### PACKAGES #### 
p <- c("ggplot2", "visreg", "patchwork")
lapply(p, library, character.only = T)

#### DATA #### 
# top model - abundance ~ PerCut * anthroper at 50 m buffer
mod <- readRDS("large/ModAbundanceDist50.rds")

#### PLOT #### 
# interaction term
i <- visreg(mod, "PerCut", by="anthroper", overlay=TRUE, band = F, gg = T) + 
  geom_point(size = 3, aes(colour=anthroper))+
  labs(y = "Abundance", x = "Mowed (%)") + 
  scale_colour_viridis_d(name = "Anthropogenic Land-Use (%)") +
  theme_classic() + 
  theme(legend.position = c(0.8, 0.9))
# interaction indicates that higher anthropogenic percentage the greater the effect of percent mowed on abundance

# main effects 
p <- visreg(mod, xvar = "PerCut", line.par = list(col = 'black'), gg = T) + 
  geom_point(size = 2) + 
  labs(y = "Abundance", x = "Mowed (%)")+
  theme_classic()
a <- visreg(mod, xvar = "anthroper", line.par = list(col = 'black'), gg = T) +
  geom_point(size = 2) + 
  labs(y = "Abundance", x = "Anthropogenic Land-Use at 50 m (%)") + 
  theme_classic()

# together 
i | (p/a)
