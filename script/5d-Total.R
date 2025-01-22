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
tot_ab <- readRDS('large/TotAbund.rds')
tot_n_sr <- readRDS('large/TotNicheSR.rds')
tot_sh <- readRDS('large/TotShann.rds')



# Total Bloom -------------------------------------------------------------

# Abundance 
ab_tot_avg <- basic_plot(tot_ab, condition = c("avgbloom"), colour = Niche.Breadth,
                         dat = anp, x = avgbloom, y = Abundance,
                       xlab = "Average Total Bloom Cover", ylab = "Butterfly Abundance") + 
  annotate("text", x = 19, y = 95, label = bquote("IRR = " ~ .(round(exp(summary(tot_ab)$coefficients[3,1]), 2)) ~ "+/-" ~ .(round(exp(summary(tot_ab)$coefficients[3,2]), 2)))) + 
  annotate("text", x = 19, y = 90, label = bquote("p-value = " ~ .(round(summary(tot_ab)$coefficients[3,4], 21)))) +
  annotate("text", x = 19, y = 86, label = bquote(R^2 ~ " = "  ~ .(round((with(summary(tot_ab), 1 - deviance/null.deviance)), 2))))


ab_tot_n <- basic_plot(tot_ab, condition = c("nspecies"), colour = Niche.Breadth,
                       dat = anp,x = nspecies, y = Abundance,
                       xlab = "Number of Flowering Plant Species", ylab = "Butterfly Abundance") + 
  annotate("text", x = 30, y = 95, label = bquote("IRR = " ~ .(round(exp(summary(tot_ab)$coefficients[2,1]), 2)) ~ "+/-" ~ .(round(exp(summary(tot_ab)$coefficients[2,2]), 2)))) + 
  annotate("text", x = 30, y = 90, label = bquote("p-value = " ~ .(round(summary(tot_ab)$coefficients[2,4], 3)))) +
  annotate("text", x = 30, y = 86, label = bquote(R^2 ~ " = "  ~ .(round((with(summary(tot_ab), 1 - deviance/null.deviance)), 2))))



# Shannon
shan_tot <- basic_plot(tot_sh, condition = "nspecies", 
                       dat = abp, x = nspecies, y = Shannon,
                       xlab = "Number of Flowering Plant Species", ylab = "Butterfly Shannon Diversity") + 
  annotate("text", x = 31, y = 8.5, label = bquote("Estimate = " ~ .(round(summary(tot_sh)$coefficients[2,1], 2)) ~ "+/-" ~ .(round(summary(tot_sh)$coefficients[2,2], 2)))) + 
  annotate("text", x = 31, y = 8.2, label = bquote("p-value = " ~ .(round(summary(tot_sh)$coefficients[2,4], 2)))) +
  annotate("text", x = 31, y = 7.9, label = bquote(R^2 ~ " = "  ~ .(round(summary(tot_sh)$adj.r.squared , 2)))) 

tot <- ab_tot_n + ab_tot_avg + shan_tot + guide_area() + 
  plot_annotation(tag_levels = 'a', tag_suffix = ")") +
  plot_layout(guides = 'collect', nrow = 2)

ggsave('figures/TotalBloom.png', tot, height = 10, width = 10, units = 'in', dpi = 450)

