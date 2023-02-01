#### PACKAGES #### 
p <- c("ggplot2", "visreg", "patchwork")
lapply(p, library, character.only = T)

#### DATA #### 
# species richness models 
sr <- readRDS("large/SpeciesRichnessDistModels.rds")
sr2000 <- sr$SRDist2000
sr1000 <- sr$SRDist1000
sr5000 <- sr$SRDist5000
sr500 <- sr$SRDist500
# shannon models
sh <- readRDS("large/ShannonDistModels.rds")
sh1000 <- sh$ShDist1000
sh2000 <- sh$ShDist2000
sh500 <- sh$ShDist500
sh5000 <- sh$ShDist5000

#### PLOT #### 
## Species Richness 
sr2000a <- visreg(sr2000, xvar = "anthroper", line.par = list(col = "black"), gg = T) +
  geom_point(size = 2) +
  annotate("text", x = 0.6, y = 10, label = bquote("Estimate = " ~ .(round(summary(sr2000)$coefficients[2,1], 2)) ~ "+/-" ~ .(round(summary(sr2000)$coefficients[2,2], 2)))) + 
  annotate("text", x = 0.6, y = 9.5, label = bquote("Adj." ~ R^2 ~ " = " ~ .(round(summary(sr2000)$adj.r.squared, 2)))) + 
  labs(y = "Species Richness", x = "Urbanization (%) at 2,000 m")+
  theme_classic()

sr1000a <- visreg(sr1000, xvar = "anthroper", line.par = list(col = "black"), gg = T) +
  geom_point(size = 2) +
  annotate("text", x = 0.6, y = 10, label = bquote("Estimate = " ~ .(round(summary(sr1000)$coefficients[2,1], 2)) ~ "+/-" ~ .(round(summary(sr1000)$coefficients[2,2], 2)))) + 
  annotate("text", x = 0.6, y = 9.5, label = bquote("Adj." ~ R^2 ~ " = " ~ .(round(summary(sr1000)$adj.r.squared, 2)))) + 
  labs(y = "", x = "Urbanization (%) at 1,000 m")+
  theme_classic()

sr5000a <- visreg(sr5000, xvar = "anthroper", line.par = list(col = "black"), gg = T) +
  geom_point(size = 2) +
  annotate("text", x = 0.6, y = 10, label = bquote("Estimate = " ~ .(round(summary(sr5000)$coefficients[2,1], 2)) ~ "+/-" ~ .(round(summary(sr5000)$coefficients[2,2], 2)))) + 
  annotate("text", x = 0.6, y = 9.5, label = bquote("Adj." ~ R^2 ~ " = " ~ .(round(summary(sr5000)$adj.r.squared, 2)))) + 
  labs(y = "Species Richness", x = "Urbanization (%) at 5,000 m")+
  theme_classic()

sr500a <- visreg(sr500, xvar = "anthroper", line.par = list(col = "black"), gg = T) +
  geom_point(size = 2) +
  annotate("text", x = 0.6, y = 10, label = bquote("Estimate = " ~ .(round(summary(sr500)$coefficients[2,1], 2)) ~ "+/-" ~ .(round(summary(sr500)$coefficients[2,2], 2)))) + 
  annotate("text", x = 0.6, y = 9.5, label = bquote("Adj." ~ R^2 ~ " = " ~ .(round(summary(sr500)$adj.r.squared, 2)))) + 
  labs(y = "", x = "Urbanization (%) at 500 m")+
  theme_classic()

p1 <- (sr2000a + sr1000a)/(sr5000a + sr500a) + plot_annotation(tag_levels = 'A')

## Shannon Diversity
sh1000a <- visreg(sh1000, xvar = "anthroper", line.par = list(col = "black"), gg = T) +
  geom_point(size = 2) +
  annotate("text", x = 0.6, y = 10, label = bquote("Estimate = " ~ .(round(summary(sh1000)$coefficients[2,1], 2)) ~ "+/-" ~ .(round(summary(sh1000)$coefficients[2,2], 2)))) + 
  annotate("text", x = 0.6, y = 9.5, label = bquote("Adj." ~ R^2 ~ " = " ~ .(round(summary(sh1000)$adj.r.squared, 2)))) + 
  labs(y = "Shannon Diversity", x = "Urbanization (%) at 1,000 m")+
  theme_classic()

sh2000a <- visreg(sh2000, xvar = "anthroper", line.par = list(col = "black"), gg = T) +
  geom_point(size = 2) +
  annotate("text", x = 0.6, y = 10, label = bquote("Estimate = " ~ .(round(summary(sh2000)$coefficients[2,1], 2)) ~ "+/-" ~ .(round(summary(sh2000)$coefficients[2,2], 2)))) + 
  annotate("text", x = 0.6, y = 9.5, label = bquote("Adj." ~ R^2 ~ " = " ~ .(round(summary(sh2000)$adj.r.squared, 2)))) + 
  labs(y = "Shannon Diversity", x = "Urbanization (%) at 2,000 m")+
  theme_classic()

sh500a <- visreg(sh500, xvar = "anthroper", line.par = list(col = "black"), gg = T) +
  geom_point(size = 2) +
  annotate("text", x = 0.6, y = 10, label = bquote("Estimate = " ~ .(round(summary(sh500)$coefficients[2,1], 2)) ~ "+/-" ~ .(round(summary(sh500)$coefficients[2,2], 2)))) + 
  annotate("text", x = 0.6, y = 9.5, label = bquote("Adj." ~ R^2 ~ " = " ~ .(round(summary(sh500)$adj.r.squared, 2)))) + 
  labs(y = "Shannon Diversity", x = "Urbanization (%) at 500 m")+
  theme_classic()

sh5000a <- visreg(sh5000, xvar = "anthroper", line.par = list(col = "black"), gg = T) +
  geom_point(size = 2) +
  annotate("text", x = 0.6, y = 10, label = bquote("Estimate = " ~ .(round(summary(sh5000)$coefficients[2,1], 2)) ~ "+/-" ~ .(round(summary(sh5000)$coefficients[2,2], 2)))) + 
  annotate("text", x = 0.6, y = 9.5, label = bquote("Adj." ~ R^2 ~ " = " ~ .(round(summary(sh5000)$adj.r.squared, 2)))) + 
  labs(y = "Shannon Diversity", x = "Urbanization (%) at 5,000 m")+
  theme_classic()

p2 <- (sh1000a + sh2000a)/(sh500a + sh5000a) + plot_annotation(tag_levels = 'A')


#### SAVE ####
png("figures/SpeciesRichnessModels.png", width = 8000, height = 7000, units = "px", res = 1000)
p1
dev.off()

png("figures/ShannonDiversityModels.png", width = 8000, height = 7000, units = "px", res = 1000)
p2
dev.off()
