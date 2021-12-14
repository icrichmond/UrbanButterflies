#### PACKAGES #### 
p <- c("dplyr", "tidyr", "ggplot2", "ggpubr")
lapply(p, library, character.only = T)

#### DATA #### 
full <- readRDS("large/FullSiteDataset.rds")
# expand to each buffer distance
list2env(full, envir = .GlobalEnv)

#### PLOT #### 
name.labs <- c("Percent Anthropogenic", "Percent Mowed")
names(name.labs) <- c("anthroper", "PerCut")

full50_p <- full50 %>%
  select(c(Pond, abund, PerCut, anthroper)) %>% 
  pivot_longer(cols = c(PerCut, anthroper))

p <- full50_p %>%
  ggplot(aes(y = abund, x = value)) + 
  geom_point() +
  theme_classic() + 
  xlab("") + 
  ylab("Abundance") + 
  facet_wrap(~ name,  scales = "free", labeller = labeller(name = name.labs)) +
  geom_smooth(data = subset(full50_p, name == "anthroper"), aes(x = value, y = abund), colour = "black", method = lm, se = FALSE) + 
  stat_cor(aes(x= value, y = abund, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.y = 150, label.x = 0.45, size = 5)+
  theme(strip.text.x = element_text(size = 12)) 
