source('script/0-Packages.R')
source('script/function-BasicPlot.R')
source('script/function-IntPlot.R')


# Data --------------------------------------------------------------------
plant <- read.csv('output/PlantCleanbySite.csv') %>% 
  mutate(Pond = str_remove(Pond, "SWF-"),
         Pond = as.numeric(Pond))
anthro <- st_read("output/AnthroFull.gpkg")
niche <- read.csv('input/ButterflyNicheBreadth.csv')
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
ab_tot_n <- basic_plot(tot_ab, condition = c("avgbloom", "Niche.Breadth"), dat = anp, 
                       x = avgbloom, y = Abundance, colour = Niche.Breadth,
                       xlab = "Average Total Bloom Cover", ylab = "Butterfly Abundance") + 
  annotate("text", x = 19, y = 95, label = bquote("IRR (WA) = " ~ .(round(exp(summary(tot_ab)$coefficients[4,1]), 2)) ~ "+/-" ~ .(round(exp(summary(tot_ab)$coefficients[4,2]), 2)))) + 
  annotate("text", x = 19, y = 90, label = bquote("p-value = " ~ .(round(summary(tot_ab)$coefficients[4,4], 4)))) +
  annotate("text", x = 19, y = 86, label = bquote(R^2 ~ " = "  ~ .(round((with(summary(tot_ab), 1 - deviance/null.deviance)), 2)))) +
  scale_colour_viridis_d() + 
  scale_fill_viridis_d()

ab_tot <- int_plot(tot_ab, list("avgbloom" = 10), condition = c("Niche.Breadth"), 
                   xlab = "", ylab = "Change in butterfly abundance with 10% \nincrease in average total bloom cover")

# Shannon
shan_tot <- basic_plot(tot_sh, condition = "nspecies", 
                       dat = abp, x = nspecies, y = Shannon,
                       xlab = "Number of Flowering Plant Species", ylab = "Butterfly Shannon Diversity") + 
  annotate("text", x = 31, y = 8.5, label = bquote("IRR = " ~ .(round(summary(tot_sh)$coefficients[2,1], 2)) ~ "+/-" ~ .(round(summary(tot_sh)$coefficients[2,2], 2)))) + 
  annotate("text", x = 31, y = 8, label = bquote("p-value = " ~ .(round(summary(tot_sh)$coefficients[2,4], 2)))) +
  annotate("text", x = 31, y = 7.5, label = bquote(R^2 ~ " = "  ~ .(round(summary(tot_sh)$adj.r.squared , 2)))) 

tot <- (ab_tot_n + ab_tot) / shan_tot + 
  plot_annotation(tag_levels = 'a', tag_suffix = ")") & 
  plot_layout(guides = 'collect') & 
  theme(legend.position = 'top') 

ggsave('figures/TotalBloom.png', tot, height = 10, width = 10, units = 'in', dpi = 450)

