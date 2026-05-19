source('script/0-Packages.R')
source('script/function-BasicPlot.R')
source('script/function-IntPlot.R')

# Data --------------------------------------------------------------------
plant <- read.csv('output/PlantCleanbySite.csv') %>%
  mutate(Pond = str_remove(Pond, "SWF-"), Pond = as.numeric(Pond))
anthro <- st_read("output/AnthroFull.gpkg")
niche <- read.csv('output/ButterflyNiche.csv')
butt <- read.csv('output/ButterflyCleanbySite.csv')

ab <- inner_join(anthro, butt, by = join_by("Pond" == "SWP"))
abp <- inner_join(ab, plant, by = "Pond")


# Models ------------------------------------------------------------------
urb_ab_400 <- readRDS('large/UrbAbund_400.rds')
urb_n_sr_400 <- readRDS('large/UrbNicheSR_400.rds')
urb_sh_400 <- readRDS('large/UrbShann_400.rds')


# Urbanization ------------------------------------------------------------

# Species Richness

sr_an <- basic_plot(
  urb_n_sr_400,
  condition = c("anthroper_400", "Niche.Breadth"),
  colour = Niche.Breadth,
  dat = abp,
  x = anthroper_400,
  y = SpeciesRichness,
  xlab = "Anthropogenic Land Cover (%)",
  ylab = "Butterfly Species Richness"
) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, .75), labels = c(0, 25, 50, 75)) +
  theme(legend.position = 'top')

# Shannon
sh_an <- basic_plot(
  urb_sh_400,
  condition = c("anthroper_400", "Niche.Breadth"),
  colour = Niche.Breadth,
  dat = abp,
  x = anthroper_400,
  y = Shannon,
  xlab = "Anthropogenic Land Cover (%)",
  ylab = "Butterfly Shannon Diversity"
) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, .75), labels = c(0, 25, 50, 75)) +
  theme(legend.position = 'top')

urb <- guide_area() /
  (sr_an |
    sh_an) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') +
  plot_layout(
    guides = 'collect',
    widths = c(1, 1),
    heights = c(1, 8)
  )

# Save --------------------------------------------------------------------
ggsave(
  'figures/Urbanization.png',
  urb,
  height = 8,
  width = 10,
  units = 'in',
  dpi = 450
)
