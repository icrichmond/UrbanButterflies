source('script/0-Packages.R')

# Data --------------------------------------------------------------------

anthro <- st_read("output/AnthroFull.gpkg")
niche <- read.csv('output/ButterflyNiche.csv')
butt <- read.csv('output/ButterflyCleanbySite.csv')

an <- left_join(niche, anthro, by = "Pond")
ab <- inner_join(anthro, butt, by = join_by("Pond" == "SWP"))

# Model -------------------------------------------------------------------

mod_ab_400 <- glm(Abundance ~ 1 + anthroper_400 * Niche.Breadth, family = poisson(), data = an)
mod_n_sr_400 <- glm(Species.Richness ~ 1 + anthroper_400 * Niche.Breadth, family = poisson(), data = an)
mod_sh_400 <- lm(Shannon ~ anthroper_400, data = ab)

# Diagnostics -------------------------------------------------------------

source('script/function-ResidPlots.R')

pdf('figures/diagnostics/UrbanizationModels.pdf')

resid_plots(mod_ab_400, "Abundance @ 400 m")
resid_plots(mod_n_sr_400, "Niche Species Richness @ 400 m")
resid_plots(mod_sh_400, "Shannon @ 400 m")

dev.off()


# Save --------------------------------------------------------------------

saveRDS(mod_ab_400, 'large/UrbAbund_400.rds')
saveRDS(mod_n_sr_400, 'large/UrbNicheSR_400.rds')
saveRDS(mod_sh_400, 'large/UrbShann_400.rds')


modelsummary(list("Abundance" = mod_ab_400, "Species Richness" = mod_n_sr_400, "Shannon Diversity" = mod_sh_400),
             fmt = NULL,
             estimate = "{round(estimate, 2)}",
             exponentiate = c(TRUE, TRUE, FALSE), 
             statistic = c("({round(conf.low, 2)}, {round(conf.high, 2)})", "{signif(p.value, 1)}"),
             conf_level = .95,
             shape = term ~ model + statistic,
             gof_map = NA,
             coef_rename = c("anthroper_400" = "Anthropogenic Land Cover (%)",
                             "Niche.BreadthWetland specialist" = "Wetland specialist"),
             output = "output/AbundanceModels.docx")
