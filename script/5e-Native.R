source('script/0-Packages.R')
source('script/function-BasicPlot.R')
source('script/function-IntPlot.R')



# Data --------------------------------------------------------------------
plant <- read.csv('output/PlantCleanbySite.csv') %>% 
  mutate(Pond = str_remove(Pond, "SWF-"),
         Pond = as.numeric(Pond))
anthro <- st_read("output/AnthroFull.gpkg")
niche <- read.csv('output/ButterflyNiche.csv')
butt <- read.csv('output/ButterflyCleanbySite.csv')

an <- left_join(niche, anthro, by = "Pond")
anp <- left_join(an, plant, by = "Pond")
ab <- inner_join(anthro, butt, by = join_by("Pond" == "SWP"))
abp <- inner_join(ab, plant, by = "Pond")


# Models ------------------------------------------------------------------
nat_ab <- readRDS('large/NatAbund.rds')
nat_n_sr <- readRDS('large/NatNicheSR.rds')
nat_sh <- readRDS('large/NatShann.rds')

# Native Bloom ------------------------------------------------------------

# Abundance 
ab_nat_n <- basic_plot(nat_ab, condition = "nnative", 
                     dat = anp, x = nnative, y = Abundance,
                     xlab = "Number of Native Flowering Species", ylab = "Butterfly Abundance") + 
  annotate("text", x = 12, y = 94, label = bquote("IRR = " ~ .(round(exp(summary(nat_ab)$coefficients[2,1]), 2)) ~ "+/-" ~ .(round(exp(summary(nat_ab)$coefficients[2,2]), 2)))) + 
  annotate("text", x = 12, y = 90, label = bquote("p-value = " ~ .(round(summary(nat_ab)$coefficients[2,4], 2)))) +
  annotate("text", x = 12, y = 86, label = bquote(R^2 ~ " = "  ~ .(round((with(summary(nat_ab), 1 - deviance/null.deviance)), 2)))) 

ab_nat_avg <- basic_plot(nat_ab, condition = "avgnatbloom", 
                       dat = anp, x = avgnatbloom, y = Abundance,
                       xlab = "Average Native Bloom Cover", ylab = "Butterfly Abundance") + 
  annotate("text", x = 12, y = 94, label = bquote("IRR = " ~ .(round(exp(summary(nat_ab)$coefficients[3,1]), 2)) ~ "+/-" ~ .(round(exp(summary(nat_ab)$coefficients[3,2]), 2)))) + 
  annotate("text", x = 12, y = 90, label = bquote("p-value = " ~ .(round(summary(nat_ab)$coefficients[3,4], 2)))) +
  annotate("text", x = 12, y = 86, label = bquote(R^2 ~ " = "  ~ .(round((with(summary(nat_ab), 1 - deviance/null.deviance)), 2)))) 


ab_nat_int_n <- int_plot(nat_ab, list("nnative" = 5), condition = c("Niche.Breadth"), 
                         xlab = "", ylab = "Change in butterfly abundance with 5 species \nincrease in number of native flowering species")

ab_nat_int_avg <- int_plot(nat_ab, list("avgnatbloom" = 10), condition = c("Niche.Breadth"), 
                   xlab = "", ylab = "Change in butterfly abundance with 10% \nincrease in average native bloom cover")


# Species Richness
sr_nat <- basic_plot(nat_n_sr, condition = "nnative", 
                     dat = anp, x = nnative, y = Species.Richness,
                     xlab = "Number of Native Flowering Species", ylab = "Butterfly Species Richness") + 
  annotate("text", x = 12, y = 15, label = bquote("IRR = " ~ .(round(exp(summary(nat_n_sr)$coefficients[2,1]), 2)) ~ "+/-" ~ .(round(exp(summary(nat_n_sr)$coefficients[2,2]), 2)))) + 
  annotate("text", x = 12, y = 14.5, label = bquote("p-value = " ~ .(round(summary(nat_n_sr)$coefficients[2,4], 2)))) +
  annotate("text", x = 12, y = 14, label = bquote(R^2 ~ " = "  ~ .(round((with(summary(nat_n_sr), 1 - deviance/null.deviance)), 2)))) 



ab_nat <- (ab_nat_n + ab_nat_avg) / (ab_nat_int_n + ab_nat_int_avg) +
  plot_annotation(tag_levels = 'a', tag_suffix = ")")


# Save --------------------------------------------------------------------

ggsave('figures/NativeBloom_Abundance.png', ab_nat, height = 10, width = 10, units = 'in', dpi = 450)
ggsave('figures/NativeBloom_SR.png', sr_nat, height = 8, width = 8, units = 'in', dpi = 450)
  