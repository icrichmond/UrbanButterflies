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
urb_ab_400 <- readRDS('large/UrbAbund_400.rds')
urb_n_sr_400 <- readRDS('large/UrbNicheSR_400.rds')
urb_sh_400 <- readRDS('large/UrbShann_400.rds')


# Urbanization ------------------------------------------------------------

# Abundance 

ab_an <- basic_plot(urb_ab_400, condition = "anthroper_400", 
           dat = anp, x = anthroper_400, y = Abundance,
           xlab = "Anthropogenic Land Cover (%)", ylab = "Butterfly Abundance") + 
  annotate("text", x = 0.75, y = 100, label = bquote("IRR = " ~ .(round(exp(summary(urb_ab_400)$coefficients[2,1]), 2)) ~ "+/-" ~ .(round(exp(summary(urb_ab_400)$coefficients[2,2]), 2)))) + 
  annotate("text", x = 0.75, y = 93, label = bquote("p-value = " ~ .(round(summary(urb_ab_400)$coefficients[2,4], 4)))) +
  annotate("text", x = 0.75, y = 88, label = bquote(R^2 ~ " = "  ~ .(round((with(summary(urb_ab_400), 1 - deviance/null.deviance)), 2)))) 



# Species Richness 

sr_an <- basic_plot(urb_n_sr_400, condition = "anthroper_400", 
                    dat = anp, x = anthroper_400, y = Species.Richness,
                    xlab = "Anthropogenic Land Cover (%)", ylab = "Butterfly Species Richness") + 
  annotate("text", x = 0.75, y = 16, label = bquote("IRR = " ~ .(round(exp(summary(urb_n_sr_400)$coefficients[2,1]), 2)) ~ "+/-" ~ .(round(exp(summary(urb_n_sr_400)$coefficients[2,2]), 2)))) + 
  annotate("text", x = 0.75, y = 15, label = bquote("p-value = " ~ .(round(summary(urb_n_sr_400)$coefficients[2,4], 3)))) +
  annotate("text", x = 0.75, y = 14, label = bquote(R^2 ~ " = "  ~ .(round((with(summary(urb_n_sr_400), 1 - deviance/null.deviance)), 2)))) 

# Shannon
sh_an <- basic_plot(urb_sh_400, condition = "anthroper_400", 
                    dat = abp, x = anthroper_400, y = Shannon,
                    xlab = "Anthropogenic Land Cover (%)", ylab = "Butterfly Shannon Diversity") + 
  annotate("text", x = 0.65, y = 8.5, label = bquote("Estimate = " ~ .(round(summary(urb_sh_400)$coefficients[2,1], 2)) ~ "+/-" ~ .(round(summary(urb_sh_400)$coefficients[2,2], 2)))) + 
  annotate("text", x = 0.65, y = 8.1, label = bquote("p-value = " ~ .(round(summary(urb_sh_400)$coefficients[2,4], 3)))) +
  annotate("text", x = 0.65, y = 7.7, label = bquote(R^2 ~ " = "  ~ .(round(summary(urb_sh_400)$adj.r.squared , 2)))) 


urb <- (ab_an + sr_an) / (sh_an + plot_spacer()) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')')
  
# Save --------------------------------------------------------------------
ggsave('figures/Urbanization.png', urb, height = 8, width = 10, units = 'in', dpi = 450)

