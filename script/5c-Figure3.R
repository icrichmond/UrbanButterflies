#### PACKAGES #### 
p <- c("ggplot2", "marginaleffects", "patchwork")
lapply(p, library, character.only = T)

#### DATA #### 
# abundance model + data
sitefull <- readRDS("large/SiteFull.rds")
ab_dat <- sitefull$full20
ab_sp <- readRDS("large/AbundanceTopModel1.rds")
ab_nat <- readRDS("large/AbundanceTopModel2.rds")

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
## Abundance

ab_sp1 <- plot_predictions(ab_sp, condition = "nspecies") + 
  geom_point(aes(x = nspecies, y = abund), data = ab_dat) + 
  annotate("text", x = 30, y = 125, label = bquote("Estimate = " ~ .(round(summary(ab_sp)$coefficients[2,1], 2)) ~ "+/-" ~ .(round(summary(ab_sp)$coefficients[2,2], 2)))) + 
  annotate("text", x = 31, y = 115, label = bquote("p-value = " ~ .(round(summary(ab_sp)$coefficients[2,4], 2)))) + 
  labs(x = "Number of Flowering Species", y = "Abundance") + 
  theme_classic()

ab_sp2 <- plot_predictions(ab_sp, condition = "avgbloom") +
  geom_point(aes(x = avgbloom, y = abund), data = ab_dat) + 
  annotate("text", x = 19, y = 125, label = bquote("Estimate = " ~ .(round(summary(ab_sp)$coefficients[3,1], 2)) ~ "+/-" ~ .(round(summary(ab_sp)$coefficients[3,2], 2)))) + 
  annotate("text", x = 20, y = 115, label = bquote("p-value = " ~ .(round(summary(ab_sp)$coefficients[3,4], 2)))) + 
  labs(x = "Average % Blooming Cover", y = "Abundance") + 
  theme_classic()

ab_sp3 <- plot_slopes(ab_sp, variables = "nspecies", condition = "avgbloom") +
  labs(x = "Average % Blooming Cover", y = "Slope of Abundance wrt \nNumber of Flowering Species") + 
  theme_classic()

p1 <- (ab_sp1 + ab_sp2)/ab_sp3



ab_nat1 <- plot_predictions(ab_nat, condition = "nnative") + 
  geom_point(aes(x = nnative, y = abund), data = ab_dat) + 
  annotate("text", x = 11, y = 125, label = bquote("Estimate = " ~ .(round(summary(ab_nat)$coefficients[2,1], 2)) ~ "+/-" ~ .(round(summary(ab_sp)$coefficients[2,2], 2)))) + 
  annotate("text", x = 12, y = 115, label = bquote("p-value = " ~ .(round(summary(ab_nat)$coefficients[2,4], 2)))) + 
  labs(x = "Number of Native Flowering Species", y = "Abundance") + 
  theme_classic()

ab_nat2 <- plot_predictions(ab_nat, condition = "avgnatbloom") +
  geom_point(aes(x = avgnatbloom, y = abund), data = ab_dat) + 
  annotate("text", x = 9, y = 125, label = bquote("Estimate = " ~ .(round(summary(ab_nat)$coefficients[3,1], 2)) ~ "+/-" ~ .(round(summary(ab_sp)$coefficients[3,2], 2)))) + 
  annotate("text", x = 10, y = 115, label = bquote("p-value = " ~ .(round(summary(ab_nat)$coefficients[3,4], 2)))) + 
  labs(x = "Average % Native Blooming Cover", y = "Abundance") + 
  theme_classic()

ab_nat3 <- plot_slopes(ab_nat, variables = "nnative", condition = "avgnatbloom") +
  labs(x = "Average % Native Blooming Cover", y = "Slope of Abundance wrt Number \nof Native Flowering Species") + 
  theme_classic()


p1b <- (ab_nat1 + ab_nat2)/ab_nat3


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

p2 <- (sr2000a + sr1000a)/(sr5000a + sr500a) + plot_annotation(tag_levels = 'A')

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

p3 <- (sh1000a + sh2000a)/(sh500a + sh5000a) + plot_annotation(tag_levels = 'A')


#### SAVE ####
png("figures/AbundanceModels.png", width = 10000, height = 8000, units = "px", res = 1000)
p1
dev.off()


png("figures/SpeciesRichnessModels.png", width = 8000, height = 7000, units = "px", res = 1000)
p2
dev.off()

png("figures/ShannonDiversityModels.png", width = 8000, height = 7000, units = "px", res = 1000)
p3
dev.off()
