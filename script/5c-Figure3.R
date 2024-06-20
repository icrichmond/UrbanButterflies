source('script/0-Packages.R')


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
urb_ab <- readRDS('large/UrbAbund.rds')
urb_sh <- readRDS('large/UrbShann.rds')
urb_sr <- readRDS('large/UrbSR.rds')
tot_ab <- readRDS('large/TotAbund.rds')
tot_sh <- readRDS('large/TotShann.rds')
tot_sr <- readRDS('large/TotSR.rds')
nat_ab <- readRDS('large/NatAbund.rds')
nat_sh <- readRDS('large/NatShann.rds')
nat_sr <- readRDS('large/NatSR.rds')



# Plots -------------------------------------------------------------------

basic_plot <- function(mod, condition, x, y, dat, xlab, ylab, ex, ey, px, py){
  
  plot_predictions(mod, condition = condition) + 
    geom_point(aes(x = {{x}}, y = {{y}}), data = dat) + 
    annotate("text", x = ex, y = ey, label = bquote("Estimate = " ~ .(round(summary(mod)$coefficients[2,1], 2)) ~ "+/-" ~ .(round(summary(mod)$coefficients[2,2], 2)))) + 
    annotate("text", x = px, y = py, label = bquote("p-value = " ~ .(round(summary(mod)$coefficients[2,4], 2)) ~ ", R2 = " ~ .(round(summary(mod)$adj.r.squared, 2)))) + 
    labs(x = xlab, y = ylab) + 
    theme_classic() + 
    theme(axis.text = element_text(size = 10))
  
}


interaction_plot <- function(mod, var, condition, xlab, ylab, ex, ey, px, py){
  
  plot_slopes(mod, variables = var, condition = condition) +
    labs(x = xlab, y = ylab) + 
    annotate("text", x = ex, y = ey, label = bquote("Estimate = " ~ .(round(summary(mod)$coefficients[4,1], 2)) ~ "+/-" ~ .(round(summary(mod)$coefficients[4,2], 2)))) + 
    annotate("text", x = px, y = py, label = bquote("p-value = " ~ .(round(summary(mod)$coefficients[4,4], 2)))) + 
    theme_classic() + 
    theme(axis.text = element_text(size = 10))
  
}

varying_slopes <- function(){
  
  
}

# Urbanization ------------------------------------------------------------

# Abundance 

# Shannon 


# Species Richness 



# Total Bloom -------------------------------------------------------------

# Abundance 


# Native Bloom ------------------------------------------------------------

# Abundance 

