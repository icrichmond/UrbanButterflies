source('script/0-Packages.R')

# Data --------------------------------------------------------------------

buttraw <- read_csv("input/ButterflyRawData.csv")
buttniche <- read.csv("input/ButterflyNicheBreadth.csv")


# Species Names -----------------------------------------------------------

# need to transpose the dataset 
buttsp <- as.data.frame(t(buttraw[,-1]))
# convert row names to a column and add genus and species columns
buttsp <- buttsp %>% 
  rownames_to_column("ScientificNames") %>% 
  mutate(Genus = word(ScientificNames, 1)) %>% 
  mutate(species = word(ScientificNames, 2))
# convert NAs and blanks to "sp." in species column
buttsp$species[buttsp$species == ""] <- NA 
buttsp$Genus <- replace_na(buttsp$Genus, "UNK")
buttsp$species <- replace_na(buttsp$species, "sp")
# fix typo 
buttsp[buttsp$ScientificNames=="Polites Themistocles", "species"] <- "themistocles"
# add species code
buttsp <- mutate(buttsp, SpeciesCode = toupper(paste0(str_sub(Genus, 1, 3), "", str_sub(species, 1, 3))))



# Abundance ---------------------------------------------------------------

# separate columns with species names so they can be replaced with species codes
buttab1 <- select(buttraw, `Scientific Name`) %>% 
  rename(SWP = `Scientific Name`)
buttab2 <- select(buttraw, -`Scientific Name`)
names(buttab2) <- buttsp$SpeciesCode[match(names(buttab2), buttsp$ScientificNames)]
# bind datasets back together 
buttab <- cbind(buttab1, buttab2)
# transform each column to numeric 
buttab <- buttab %>%
  mutate(across(HESSP:DANPLE, as.numeric))
# remove columns where there are no observations 
buttab <- buttab %>% 
  select(where(function(x) any(!is.na(x))))
# calculate abundance and species richness per site on each visit
buttab <- buttab %>% 
  mutate(abund = rowSums(across(HESSP:DANPLE), na.rm = T))
# save
write.csv(buttab, "output/ButterflyAbundance.csv")


# Diversity ---------------------------------------------------------------

# use iNEXT to calculate Shannon diversity, Simpson diversity, and species richness 
# calculate the sampling coverage in our study to standardize our values wrt sampling effort 
nextd <- sapply(buttab2, as.numeric)
nextd[is.na(nextd)] <- 0
nextd <- cbind(buttab1[,1], nextd)
nextd <- nextd %>% 
  group_by(SWP) %>% 
  summarise(across(HESSP:DANPLE, sum))
nextd <- nextd %>% 
  select_if(colSums(.) != 0)
nextdl <- pivot_longer(nextd, HESSP:DANPLE)
nextdw <- pivot_wider(nextdl, names_from = SWP) %>% 
  column_to_rownames("name") %>% 
  select_if(colSums(.) != 0)
nextd <- as.list(nextdw)
# calculate iNEXT object
outrich <- iNEXT(nextd, q=0 ,datatype="abundance") # use min and max abundances observed for size
# visualize sample coverage
sample.coverage <- ggiNEXT(outrich, type= 2) + theme(legend.position = 'none')
coverage.by.richness <- ggiNEXT(outrich, type= 3)+ theme(legend.position = 'none') + ylim(c(-1,30))
# extract sample coverage info
# n = sample size, S.obs = species richness, SC = sample coverage
outrich$DataInfo
# calculate diversity values at the lowest sample coverage value
# q(order) 0 = species richness, 1 = Shannon diversity, 2 = Simpson diversity
cov <- min(outrich$DataInfo$SC)
rarediv <- estimateD(nextdw, datatype = "abundance", base = "coverage", 
                     level= cov, conf=0.95)
rarediv_w <- pivot_wider(rarediv, id_cols = Assemblage, names_from = Order.q,
                         names_sep = ".", values_from = c(m, SC, qD))
rarediv_w <- rarediv_w %>% 
  select(-c(m.1, m.2, SC.1, SC.2)) %>% 
  rename(SWP = Assemblage, 
         method = m.0, 
         sampcov = SC.0, 
         SpeciesRichness = qD.0, 
         Shannon = qD.1, 
         Simpson = qD.2)
# abundance
buttab_site <- buttab %>%
  group_by(SWP) %>% 
  summarise(abund = sum(across(HESSP:DANPLE), na.rm = T))
buttab_site$SWP <- as.character(buttab_site$SWP)
# join all metrics
butt_site <- inner_join(buttab_site, rarediv_w)



# Collapse Niches ---------------------------------------------------------

buttniche$Niche.Breadth <- as.factor(buttniche$Niche.Breadth)
buttniche$Niche.Breadth <- fct_collapse(buttniche$Niche.Breadth,
                                        `Wetland non-specialist` = c("Non-wetland", "Wetland associated"),
                                        `Wetland specialist` = c("Wetland specialist"))
buttniche <- buttniche %>% 
  group_by(Pond, Niche.Breadth) %>% 
  summarize(Abundance = sum(Abundance), 
            Species.Richness = sum(Species.Richness))

# Save --------------------------------------------------------------------

write_csv(buttsp, "output/ButterflySpecies.csv")
write_csv(butt_site, "output/ButterflyCleanbySite.csv")
write_csv(buttniche, 'output/ButterflyNiche.csv')
