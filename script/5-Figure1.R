#### PACKAGES #### 
p <- c("dplyr", "tidyr", "ggplot2")
lapply(p, library, character.only = T)

#### DATA #### 
full <- readRDS("large/FullSiteDataset.rds")
# expand to each buffer distance
list2env(full, envir = .GlobalEnv)

#### PLOT #### 
name.labs <- c("Percent Anthropogenic", "Percent Mowed")
names(name.labs) <- c("anthroper", "PerCut")

p <- full50 %>%
  select(c(Pond, abund, PerCut, anthroper)) %>% 
  pivot_longer(cols = c(PerCut, anthroper)) %>% 
  ggplot(aes(y = abund, x = value)) + 
  geom_point() +
  theme_classic() + 
  xlab("") + 
  ylab("Abundance") + 
  facet_wrap(~ name,  scales = "free", labeller = labeller(name = name.labs)) +
  theme(strip.text.x = element_text(size = 12))
