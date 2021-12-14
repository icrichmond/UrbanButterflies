#### PACKAGES #### 
p <- c("ggplot2", "visreg")
lapply(p, library, character.only = T)

#### DATA #### 
# top model - abundance ~ PerCut * anthroper at 50 m buffer
mod <- readRDS("large/ModAbundanceDist50.rds")

#### PLOT #### 
# interaction term
visreg(mod, "PerCut", by="anthroper", overlay=TRUE, band = F, gg = T)
# interaction indicates that higher anthropogenic percentage the greater the effect of percent mowed on abundance

# main effects 
visreg(mod) + 
  facet_wrap(~.)