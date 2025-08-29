source('script/0-Packages.R')

# Data --------------------------------------------------------------------

anthro <- st_read("output/AnthroFull.gpkg")
butt <- read.csv('output/ButterflyCleanbySite.csv')

ab <- inner_join(anthro, butt, by = join_by("Pond" == "SWP"))

# Model -------------------------------------------------------------------

mod_ab_400 <- glm.nb(abund ~ 1 + anthroper_400 * Niche.Breadth + SamplingArea, data = ab)
mod_n_sr_400 <- lm(SpeciesRichness ~ 1 + anthroper_400 * Niche.Breadth + SamplingArea, data = ab)
mod_sh_400 <- lm(Shannon ~ 1 + anthroper_400 * Niche.Breadth + SamplingArea, data = ab)

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
                             "SamplingArea" = "Site Area (units)",
                             "Niche.BreadthWetland specialist" = "Wetland specialist"),
             output = "output/AbundanceModels.docx")
