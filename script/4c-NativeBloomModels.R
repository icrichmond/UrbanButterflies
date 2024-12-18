source('script/0-Packages.R')

# Data --------------------------------------------------------------------

plant <- read.csv('output/PlantCleanbySite.csv') %>% 
  mutate(Pond = str_remove(Pond, "SWF-"),
         Pond = as.numeric(Pond))
niche <- read.csv('output/ButterflyNiche.csv')
butt <- read.csv('output/ButterflyCleanbySite.csv')

pn <- left_join(niche, plant, by = "Pond")
pb <- inner_join(plant, butt, by = join_by("Pond" == "SWP"))

# Model -------------------------------------------------------------------

mod_ab <- glm(Abundance ~ 1 + nnative + avgnatbloom + nnative * Niche.Breadth + avgnatbloom * Niche.Breadth, family = poisson(), data = pn)
mod_n_sr <- glm(Species.Richness ~ 1 + nnative + avgnatbloom + nnative * Niche.Breadth + avgnatbloom*Niche.Breadth, family = poisson(), data = pn)
mod_sh <- lm(Shannon ~ nnative + avgnatbloom, data = pb)

# Diagnostics -------------------------------------------------------------

source('script/function-ResidPlots.R')

pdf('figures/diagnostics/NativeBloomModels.pdf')
resid_plots(mod_ab, "Abundance")
resid_plots(mod_n_sr, "Niche Species Richness")
resid_plots(mod_sh, "Shannon")
dev.off()


# Save --------------------------------------------------------------------

saveRDS(mod_ab, 'large/NatAbund.rds')
saveRDS(mod_n_sr, 'large/NatNicheSR.rds')
saveRDS(mod_sh, 'large/NatShann.rds')



modelsummary(list("Abundance" = mod_ab, "Species Richness" = mod_n_sr, "Shannon Diversity" = mod_sh),
             fmt = NULL,
             estimate = "{round(estimate, 2)}",
             exponentiate = T, 
             statistic = c("({round(conf.low, 2)}, {round(conf.high, 2)})", "{signif(p.value, 1)}"),
             conf_level = .95,
             shape = term ~ model + statistic,
             gof_map = NA,
             coef_rename = c("nnative" = "Number of Native Flowering Species",
                             "avgnatbloom" = "Average Native Bloom Cover",
                             "Niche.BreadthWetland specialist" = "Wetland specialist"),
             output = "output/NativeBloomModels.docx")
