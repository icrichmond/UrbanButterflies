source('script/0-Packages.R')

# Data --------------------------------------------------------------------

plant <- read.csv('output/PlantCleanbySite.csv') %>% 
  mutate(Pond = str_remove(Pond, "SWF-"),
         Pond = as.numeric(Pond))
niche <- read.csv('input/ButterflyNicheBreadth.csv')
butt <- read.csv('output/ButterflyCleanbySite.csv')

pn <- left_join(niche, plant, by = "Pond")
pb <- inner_join(plant, butt, by = join_by("Pond" == "SWP"))

# Model -------------------------------------------------------------------

mod_ab <- lmer(Abundance ~ 1 + (nnative + avgnatbloom | Niche.Breadth), data = pn)
# model fails to converge
mod_sh <- lm(Shannon ~ nnative * avgnatbloom, data = pb)
mod_sr <- lm(SpeciesRichness ~ nnative * avgnatbloom, data = pb)

# Diagnostics -------------------------------------------------------------

source('script/function-ResidPlots.R')

pdf('figures/diagnostics/NativeBloomModels.pdf')
resid_plots(mod_ab, "Abundance")
resid_plots(mod_sh, "Shannon")
resid_plots(mod_sr, "Species Richness")
dev.off()


# Save --------------------------------------------------------------------

saveRDS(mod_ab, 'large/NatAbund.rds')
saveRDS(mod_sh, 'large/NatShann.rds')
saveRDS(mod_sr, 'large/NatSR.rds')
