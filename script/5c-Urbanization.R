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
urb_ab_400 <- readRDS('large/UrbAbund_400.rds')
urb_n_sr_400 <- readRDS('large/UrbNicheSR_400.rds')
urb_sh_400 <- readRDS('large/UrbShann_400.rds')


# Urbanization ------------------------------------------------------------

# Abundance 

ab_an <- basic_plot(urb_ab_400, condition = "anthroper_400", 
           dat = anp, x = anthroper_400, y = Abundance,
           xlab = "Anthropogenic Land Cover (%)", ylab = "Butterfly Abundance") + 
  annotate("text", x = 0.8, y = 95, label = bquote("IRR = " ~ .(round(exp(summary(urb_ab_400)$coefficients[2,1]), 2)) ~ "+/-" ~ .(round(exp(summary(urb_ab_400)$coefficients[2,2]), 2)))) + 
  annotate("text", x = 0.8, y = 90, label = bquote("p-value = " ~ .(round(summary(urb_ab_400)$coefficients[2,4], 4)))) +
  annotate("text", x = 0.8, y = 85, label = bquote(R^2 ~ " = "  ~ .(round((with(summary(urb_ab_400), 1 - deviance/null.deviance)), 2)))) 

  

ab_ni <- basic_plot(urb_ab_400, condition = c("anthroper_400", "Niche.Breadth"), dat = anp, 
           x = anthroper_400, y = Abundance, colour = Niche.Breadth,
           xlab = "Anthropogenic Land Cover (%)", ylab = "Butterfly Abundance") + 
  annotate("text", x = 0.8, y = 95, label = bquote("IRR (WA) = " ~ .(round(exp(summary(urb_ab_400)$coefficients[3,1]), 2)) ~ "+/-" ~ .(round(exp(summary(urb_ab_400)$coefficients[3,2]), 2)))) + 
  annotate("text", x = 0.8, y = 90, label = bquote("p-value = " ~ .(round(summary(urb_ab_400)$coefficients[3,4], 14)))) +
  annotate("text", x = 0.8, y = 85, label = bquote(R^2 ~ " = "  ~ .(round((with(summary(urb_ab_400), 1 - deviance/null.deviance)), 2)))) +
  scale_colour_viridis_d() + 
  scale_fill_viridis_d()


an_int <- int_plot(urb_ab_400, list("anthroper_400" = 0.5), condition = c("Niche.Breadth"), 
                   xlab = "", ylab = "Change in butterfly abundance with 50% increase \nin anthropogenic land cover")


ab_urb <- (ab_an + ab_ni) / an_int + 
  plot_annotation(tag_levels = 'a', tag_suffix = ")") & 
  plot_layout(guides = 'collect') & 
  theme(legend.position = 'top')


# Species Richness 

sr_an <- basic_plot(urb_n_sr_400, condition = "anthroper_400", 
                    dat = anp, x = anthroper_400, y = Species.Richness,
                    xlab = "Anthropogenic Land Cover (%)", ylab = "Butterfly Species Richness") + 
  annotate("text", x = 0.75, y = 8.5, label = bquote("IRR = " ~ .(round(exp(summary(urb_n_sr_400)$coefficients[2,1]), 2)) ~ "+/-" ~ .(round(exp(summary(urb_n_sr_400)$coefficients[2,2]), 2)))) + 
  annotate("text", x = 0.75, y = 8, label = bquote("p-value = " ~ .(round(summary(urb_n_sr_400)$coefficients[2,4], 3)))) +
  annotate("text", x = 0.75, y = 7.5, label = bquote(R^2 ~ " = "  ~ .(round((with(summary(urb_n_sr_400), 1 - deviance/null.deviance)), 2)))) 

sr_int <- int_plot(urb_n_sr_400, list("anthroper_400" = 0.5), condition = c("Niche.Breadth"), 
                   xlab = "", ylab = "Change in butterfly species richness with 50% \nincrease in anthropogenic land cover")


# Shannon
sh_an <- basic_plot(urb_sh_400, condition = "anthroper_400", 
                    dat = abp, x = anthroper_400, y = Shannon,
                    xlab = "Anthropogenic Land Cover (%)", ylab = "Butterfly Shannon Diversity") + 
  annotate("text", x = 0.75, y = 8.5, label = bquote("IRR = " ~ .(round(summary(urb_sh_400)$coefficients[2,1], 2)) ~ "+/-" ~ .(round(summary(urb_sh_400)$coefficients[2,2], 2)))) + 
  annotate("text", x = 0.75, y = 8, label = bquote("p-value = " ~ .(round(summary(urb_sh_400)$coefficients[2,4], 3)))) +
  annotate("text", x = 0.75, y = 7.5, label = bquote(R^2 ~ " = "  ~ .(round(summary(urb_sh_400)$adj.r.squared , 2)))) 


sr_urb <- (sr_an + sr_int) / sh_an + 
  plot_annotation(tag_levels = 'a', tag_suffix = ")") 


# Save --------------------------------------------------------------------
ggsave('figures/UrbanizationAbundance.png', ab_urb, height = 10, width = 10, units = 'in', dpi = 450)
ggsave('figures/UrbanizationSR.png', sr_urb, height = 10, width = 10, units = 'in', dpi = 450)

