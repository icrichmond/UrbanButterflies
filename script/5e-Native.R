source('script/0-Packages.R')
source('script/function-BasicPlot.R')
source('script/function-IntPlot.R')


# Data --------------------------------------------------------------------
plant <- read.csv('output/PlantCleanbySite.csv') %>%
  mutate(Pond = str_remove(Pond, "SWF-"), Pond = as.numeric(Pond))
anthro <- st_read("output/AnthroFull.gpkg")
niche <- read.csv('output/ButterflyNiche.csv')
butt <- read.csv('output/ButterflyCleanbySite.csv')

an <- left_join(niche, anthro, by = "Pond")
anp <- left_join(an, plant, by = "Pond")
ab <- inner_join(anthro, butt, by = join_by("Pond" == "SWP"))
abp <- inner_join(ab, plant, by = "Pond")


# Models ------------------------------------------------------------------

mod_ab_nat <- readRDS('large/NatAbund.rds')
mod_n_sr_nat <- readRDS('large/NatNicheSR.rds')
mod_sh_nat <- readRDS('large/NatShann.rds')

# Native Bloom ------------------------------------------------------------

# Abundance
ab_nnat <- basic_plot(
  mod_ab_nat,
  condition = c("nnative"),
  dat = abp,
  x = nnative,
  y = abund,
  xlab = "Number of Native Flowering Species",
  ylab = "Butterfly Abundance"
)


# Shannon
ab_natcov <- basic_plot(
  mod_ab_nat,
  condition = c("avgnatbloom"),
  dat = abp,
  x = avgnatbloom,
  y = abund,
  xlab = "Average Native Bloom Cover (%)",
  ylab = "Butterfly Abundance"
)

nat <- ab_nnat + ab_natcov

# Save --------------------------------------------------------------------

ggsave(
  'figures/NativeBloom.png',
  nat,
  height = 10,
  width = 10,
  units = 'in',
  dpi = 450
)
