#### PACKAGES #### 
p <- c("ggplot2", "marginaleffects", "patchwork")
lapply(p, library, character.only = T)

#### DATA #### 
# raw data
sitefull <- readRDS("large/SiteFull.rds")

# abundance model 
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
  theme_classic() + 
  theme(axis.text = element_text(size = 10))

ab_sp2 <- plot_predictions(ab_sp, condition = "avgbloom") +
  geom_point(aes(x = avgbloom, y = abund), data = ab_dat) + 
  annotate("text", x = 19, y = 125, label = bquote("Estimate = " ~ .(round(summary(ab_sp)$coefficients[3,1], 2)) ~ "+/-" ~ .(round(summary(ab_sp)$coefficients[3,2], 2)))) + 
  annotate("text", x = 20, y = 115, label = bquote("p-value = " ~ .(round(summary(ab_sp)$coefficients[3,4], 2)))) + 
  labs(x = "Average % Blooming Cover", y = "Abundance") + 
  theme_classic() +
  theme(axis.text = element_text(size = 10))

ab_sp3 <- plot_slopes(ab_sp, variables = "nspecies", condition = "avgbloom") +
  labs(x = "Average % Blooming Cover", y = "Slope of Abundance wrt \nNumber of Flowering Species") + 
  annotate("text", x = 19, y = 6, label = bquote("Estimate = " ~ .(round(summary(ab_sp)$coefficients[4,1], 2)) ~ "+/-" ~ .(round(summary(ab_sp)$coefficients[4,2], 2)))) + 
  annotate("text", x = 20, y = 5, label = bquote("p-value = " ~ .(round(summary(ab_sp)$coefficients[4,4], 2)))) + 
  theme_classic() + 
  theme(axis.text = element_text(size = 10))

p1 <- (ab_sp1 + ab_sp2)/ab_sp3


## Species Richness 
sr_2000 <- plot_predictions(sr2000, condition = "anthroper") + 
  geom_point(aes(x = anthroper, y = SpeciesRichness), data = sitefull$full2000) + 
  annotate("text", x = 0.7, y = 10, label = bquote("Estimate = " ~ .(round(summary(sr2000)$coefficients[2,1], 2)) ~ "+/-" ~ .(round(summary(sr2000)$coefficients[2,2], 2)))) + 
  annotate("text", x = 0.7, y = 9.5, label = bquote("p-value = " ~ .(round(summary(sr2000)$coefficients[2,4], 2)))) + 
  labs(x = "Urbanization (%) at 2,000 m", y = "Species Richness") + 
  theme_classic() + 
  theme(axis.text = element_text(size = 10))


sr_1000 <- plot_predictions(sr1000, condition = "anthroper") + 
  geom_point(aes(x = anthroper, y = SpeciesRichness), data = sitefull$full1000) + 
  annotate("text", x = 0.7, y = 10, label = bquote("Estimate = " ~ .(round(summary(sr1000)$coefficients[2,1], 2)) ~ "+/-" ~ .(round(summary(sr1000)$coefficients[2,2], 2)))) + 
  annotate("text", x = 0.7, y = 9.5, label = bquote("p-value = " ~ .(round(summary(sr1000)$coefficients[2,4], 2)))) + 
  labs(x = "Urbanization (%) at 1,000 m", y = "Species Richness") + 
  theme_classic() + 
  theme(axis.text = element_text(size = 10))


sr_5000 <- plot_predictions(sr5000, condition = "anthroper") + 
  geom_point(aes(x = anthroper, y = SpeciesRichness), data = sitefull$full5000) + 
  annotate("text", x = 0.7, y = 10, label = bquote("Estimate = " ~ .(round(summary(sr5000)$coefficients[2,1], 2)) ~ "+/-" ~ .(round(summary(sr5000)$coefficients[2,2], 2)))) + 
  annotate("text", x = 0.7, y = 9.5, label = bquote("p-value = " ~ .(round(summary(sr5000)$coefficients[2,4], 2)))) + 
  labs(x = "Urbanization (%) at 5,000 m", y = "Species Richness") + 
  theme_classic() + 
  theme(axis.text = element_text(size = 10))

sr_500 <- plot_predictions(sr500, condition = "anthroper") + 
  geom_point(aes(x = anthroper, y = SpeciesRichness), data = sitefull$full500) + 
  annotate("text", x = 0.7, y = 10, label = bquote("Estimate = " ~ .(round(summary(sr500)$coefficients[2,1], 2)) ~ "+/-" ~ .(round(summary(sr500)$coefficients[2,2], 2)))) + 
  annotate("text", x = 0.7, y = 9.5, label = bquote("p-value = " ~ .(round(summary(sr500)$coefficients[2,4], 2)))) + 
  labs(x = "Urbanization (%) at 500 m", y = "Species Richness") + 
  theme_classic() + 
  theme(axis.text = element_text(size = 10))

p2 <- (sr_2000 + sr_1000)/(sr_5000 + sr_500) + plot_annotation(tag_levels = 'A')

## Shannon Diversity
sh_2000 <- plot_predictions(sh2000, condition = "anthroper") + 
  geom_point(aes(x = anthroper, y = Shannon), data = sitefull$full2000) + 
  annotate("text", x = 0.7, y = 10, label = bquote("Estimate = " ~ .(round(summary(sh2000)$coefficients[2,1], 2)) ~ "+/-" ~ .(round(summary(sh2000)$coefficients[2,2], 2)))) + 
  annotate("text", x = 0.7, y = 9.5, label = bquote("p-value = " ~ .(round(summary(sh2000)$coefficients[2,4], 2)))) + 
  labs(x = "Urbanization (%) at 2,000 m", y = "Shannon Diversity") + 
  theme_classic() + 
  theme(axis.text = element_text(size = 10))


sh_1000 <- plot_predictions(sh1000, condition = "anthroper") + 
  geom_point(aes(x = anthroper, y = Shannon), data = sitefull$full1000) + 
  annotate("text", x = 0.7, y = 10, label = bquote("Estimate = " ~ .(round(summary(sh1000)$coefficients[2,1], 2)) ~ "+/-" ~ .(round(summary(sh1000)$coefficients[2,2], 2)))) + 
  annotate("text", x = 0.7, y = 9.5, label = bquote("p-value = " ~ .(round(summary(sh1000)$coefficients[2,4], 2)))) + 
  labs(x = "Urbanization (%) at 1,000 m", y = "Shannon Diversity") + 
  theme_classic() + 
  theme(axis.text = element_text(size = 10))


sh_5000 <- plot_predictions(sh5000, condition = "anthroper") + 
  geom_point(aes(x = anthroper, y = Shannon), data = sitefull$full5000) + 
  annotate("text", x = 0.7, y = 10, label = bquote("Estimate = " ~ .(round(summary(sh5000)$coefficients[2,1], 2)) ~ "+/-" ~ .(round(summary(sh5000)$coefficients[2,2], 2)))) + 
  annotate("text", x = 0.7, y = 9.5, label = bquote("p-value = " ~ .(round(summary(sh5000)$coefficients[2,4], 2)))) + 
  labs(x = "Urbanization (%) at 5,000 m", y = "Shannon Diversity") + 
  theme_classic() + 
  theme(axis.text = element_text(size = 10))

sh_500 <- plot_predictions(sh500, condition = "anthroper") + 
  geom_point(aes(x = anthroper, y = Shannon), data = sitefull$full500) + 
  annotate("text", x = 0.7, y = 10, label = bquote("Estimate = " ~ .(round(summary(sh500)$coefficients[2,1], 2)) ~ "+/-" ~ .(round(summary(sh500)$coefficients[2,2], 2)))) + 
  annotate("text", x = 0.7, y = 9.5, label = bquote("p-value = " ~ .(round(summary(sh500)$coefficients[2,4], 2)))) + 
  labs(x = "Urbanization (%) at 500 m", y = "Shannon Diversity") + 
  theme_classic() + 
  theme(axis.text = element_text(size = 10))

p3 <- (sh_1000 + sh_2000)/(sh_5000 + sh_500 ) + plot_annotation(tag_levels = 'A')


#### SAVE ####
ggsave('figures/AbundanceModels.png', p1, width = 10, height = 10, units = "in")

ggsave('figures/SpeciesRichnessModels.png', p2, width = 10, height = 10, units = "in")

ggsave("figures/ShannonDiversityModels.png", p3, width = 10, height = 10, units = "in")

