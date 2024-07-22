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
nat_ab <- readRDS('large/NatAbund.rds')
nat_n_sr <- readRDS('large/NatNicheSR.rds')
nat_sh <- readRDS('large/NatShann.rds')

# Native Bloom ------------------------------------------------------------

# Abundance 
ab_nat_n <- basic_plot(nat_ab, condition = c("nnative", "Niche.Breadth"), dat = anp, 
                       x = nnative, y = Abundance, colour = Niche.Breadth,
                       xlab = "Number of Native Flowering Species", ylab = "Butterfly Abundance") + 
  annotate("text", x = 13, y = 94, label = bquote("IRR (WA) = " ~ .(round(exp(summary(nat_ab)$coefficients[4,1]), 2)) ~ "+/-" ~ .(round(exp(summary(nat_ab)$coefficients[4,2]), 2)))) + 
  annotate("text", x = 13, y = 90, label = bquote("p-value = " ~ .(round(summary(nat_ab)$coefficients[4,4], 10)))) +
  annotate("text", x = 13, y = 86, label = bquote(R^2 ~ " = "  ~ .(round((with(summary(nat_ab), 1 - deviance/null.deviance)), 2)))) 

ab_nat <- int_plot(nat_ab, list("avgnatbloom" = 10), condition = c("Niche.Breadth"), 
                   xlab = "", ylab = "Change in butterfly abundance with 10% \nincrease in average native bloom cover")


# Species Richness
sr_nat <- basic_plot(nat_n_sr, condition = "nnative", 
                     dat = anp, x = nnative, y = Species.Richness,
                     xlab = "Number of Native Flowering Species", ylab = "Butterfly Species Richness") + 
  annotate("text", x = 12, y = 10.5, label = bquote("IRR = " ~ .(round(exp(summary(urb_ab_400)$coefficients[2,1]), 2)) ~ "+/-" ~ .(round(exp(summary(urb_ab_400)$coefficients[2,2]), 2)))) + 
  annotate("text", x = 12, y = 10, label = bquote("p-value = " ~ .(round(summary(urb_ab_400)$coefficients[2,4], 4)))) +
  annotate("text", x = 12, y = 9.5, label = bquote(R^2 ~ " = "  ~ .(round((with(summary(urb_ab_400), 1 - deviance/null.deviance)), 2)))) 


sr_nat_n <- basic_plot(nat_n_sr, condition = c("nnative", "Niche.Breadth"), dat = anp, 
                       x = nnative, y = Species.Richness, colour = Niche.Breadth,
                       xlab = "Number of Native Flowering Species", ylab = "Butterfly Species Richness") + 
  annotate("text", x = 12, y = 10.5, label = bquote("IRR (WA) = " ~ .(round(exp(summary(nat_ab)$coefficients[4,1]), 2)) ~ "+/-" ~ .(round(exp(summary(nat_ab)$coefficients[4,2]), 2)))) + 
  annotate("text", x = 12, y = 10, label = bquote("p-value = " ~ .(round(summary(nat_ab)$coefficients[4,4], 10)))) +
  annotate("text", x = 12, y = 9.5, label = bquote(R^2 ~ " = "  ~ .(round((with(summary(nat_ab), 1 - deviance/null.deviance)), 2)))) 


nat <- (ab_nat_n + ab_nat) / (sr_nat + sr_nat_n) + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = 'top')
