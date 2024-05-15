# Spatial Figures 
# Author: Isabella Richmond 
# This script is for producing species abundance bar plot


#### Load Packages ####
p <- c("dplyr" ,"ggplot2")
lapply(p, library, character.only=T)


#### Data Input ####
species <- read.csv('output/ButterflySpecies.csv')
butt <- read.csv("output/ButterflyAbundance.csv")


#### Species Counts ####
counts <- select(butt, HESSP:DANPLE)
names(counts) <- species$ScientificNames[match(names(counts), species$SpeciesCode)]
counts <- as.data.frame(colSums(counts, na.rm=TRUE))
counts <- rename(counts, 'count' = `colSums(counts, na.rm = TRUE)`)
counts$species <- row.names(counts)


#### Plot ####
h <- ggplot(counts, aes(count, reorder(species, count, sum))) + 
  geom_bar(stat = 'identity') + 
  geom_text(aes(label = count), hjust = 0) + 
  theme_classic() + 
  theme(axis.text.y = element_text(face = 'italic', colour = 'black')) + 
  scale_x_continuous(expand = c(0.0025, 0), limits = c(0, 450)) + 
  ylab("") + 
  xlab("Abundance")

ggsave("figures/AbundanceHistogram.png", h, height = 8, width = 8, dpi = 450)
