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
mod_ab_tot <- readRDS('large/TotAbund.rds')
mod_n_sr_tot <- readRDS('large/TotNicheSR.rds')
mod_sh_tot <- readRDS('large/TotShann.rds')


# Total Bloom -------------------------------------------------------------

# Abundance

# Abundance
ab_cov <- basic_plot(
  mod_ab_tot,
  condition = c("avgbloom"),
  dat = abp,
  x = avgbloom,
  y = abund,
  xlab = "Average Total Bloom Cover (%)",
  ylab = "Butterfly Abundance"
)


# Species Richness

sr_ntot <- basic_plot(
  mod_n_sr_tot,
  condition = c("nspecies"),
  dat = abp,
  x = nspecies,
  y = SpeciesRichness,
  xlab = "Number of Flowering Species",
  ylab = "Butterfly Species Richness"
)

sr_cov <- basic_plot(
  mod_n_sr_tot,
  condition = c("avgbloom", "Niche.Breadth"),
  colour = Niche.Breadth,
  dat = abp,
  x = avgbloom,
  y = SpeciesRichness,
  xlab = "Average Total Bloom Cover (%)",
  ylab = "Butterfly Species Richness"
)


# Shannon
sh_ntot <- basic_plot(
  mod_sh_tot,
  condition = c("nspecies"),
  dat = abp,
  x = nspecies,
  y = Shannon,
  xlab = "Number of Flowering Species",
  ylab = "Butterfly Shannon"
)

sh_cov <- basic_plot(
  mod_sh_tot,
  condition = c("avgbloom"),
  dat = abp,
  x = avgbloom,
  y = Shannon,
  xlab = "Average Total Bloom Cover (%)",
  ylab = "Butterfly Shannon"
)


tot <- sr_ntot +
  sr_cov +
  sh_ntot +
  sh_cov +
  ab_cov +
  plot_layout(ncol = 2, nrow = 3, guides = 'collect') &
  theme(legend.position = "top")

# save
ggsave(
  'figures/TotalBloom.png',
  tot,
  height = 10,
  width = 10,
  units = 'in',
  dpi = 450
)
