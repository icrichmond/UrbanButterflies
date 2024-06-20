source('script/0-Packages.R')

# Data --------------------------------------------------------------------

anthro <- st_read("output/AnthroFull.gpkg")
niche <- read.csv('input/ButterflyNicheBreadth.csv')
butt <- read.csv('output/ButterflyCleanbySite.csv')

an <- left_join(niche, anthro, by = "Pond")
ab <- inner_join(anthro, butt, by = join_by("Pond" == "SWP"))

# Model -------------------------------------------------------------------

mod_ab <- lmer(Abundance ~ 1 + (anthroper|Niche.Breadth), data = an)
mod_sh <- lm(Shannon ~ anthroper, data = ab)
mod_sr <- lm(SpeciesRichness ~ anthroper, data = ab)

# Diagnostics -------------------------------------------------------------

source('script/function-ResidPlots.R')

pdf('figures/diagnostics/UrbanizationModels.pdf')
resid_plots(mod_ab, "Abundance")
resid_plots(mod_sh, "Shannon")
resid_plots(mod_sr, "Species Richness")
dev.off()


# Save --------------------------------------------------------------------

saveRDS(mod_ab, 'large/UrbAbund.rds')
saveRDS(mod_sh, 'large/UrbShann.rds')
saveRDS(mod_sr, 'large/UrbSR.rds')
