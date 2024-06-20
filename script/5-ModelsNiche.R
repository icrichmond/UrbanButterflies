# Packages ----------------------------------------------------------------

p <- c("dplyr", "broom.mixed", "lme4", "lmerTest", "marginaleffects")
lapply(p, library, character.only = T)


# Data --------------------------------------------------------------------

bnb <- read.csv('input/ButterflyNicheBreadth.csv')


# Model -------------------------------------------------------------------

bnb_mod <- lmer(Abundance ~ Niche.Breadth + (1|Pond), data = bnb)

# diagnostics 
source("script/function-ResidPlots.R")
bnb_d <- resid_plots(bnb_mod, "Niche Breadth")
pdf("figures/Diagnostics/nichebreadth_glmm_diagnostics.pdf")
bnb_d
dev.off()

p <- plot_predictions(bnb_mod, condition = "Niche.Breadth") + 
  geom_point(aes(x = Niche.Breadth, y = Abundance), position = position_dodge2(0.3), alpha = 0.5, data = bnb) + 
  labs(x = "Niche Breadth", y = "Butterfly Abundance") + 
  theme_classic()

ggsave('figures/NicheBreadth.png', p, width = 8, height = 6, units = "in")

