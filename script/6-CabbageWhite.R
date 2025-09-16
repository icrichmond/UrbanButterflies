source('script/0-Packages.R')

# Data --------------------------------------------------------------------

buttraw <- read_csv("input/ButterflyRawData.csv") %>%
  # remove cabbage white
  dplyr::select(-c(`Pieris rapae`))
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
buttab1 <- dplyr::select(buttraw, `Scientific Name`) %>% 
  rename(SWP = `Scientific Name`)
buttab2 <- dplyr::select(buttraw, -`Scientific Name`)
names(buttab2) <- buttsp$SpeciesCode[match(names(buttab2), buttsp$ScientificNames)]
# bind datasets back together 
buttab <- cbind(buttab1, buttab2)
# transform each column to numeric 
buttab <- buttab %>%
  mutate(across(HESSP:DANPLE, as.numeric))
# remove columns where there are no observations 
buttab <- buttab %>% 
  dplyr::select(where(function(x) any(!is.na(x))))
# calculate abundance and species richness per site on each visit
buttab <- buttab %>% 
  mutate(abund = rowSums(across(HESSP:DANPLE), na.rm = T))

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

# separate by Niche Breadth 

nextgendl <- nextd %>% 
  dplyr::select(-c(ANCNUM, EUPDIO, LYCHYL, SATEUR)) %>% 
  pivot_longer(., HESSP:DANPLE) %>% 
  pivot_wider(., names_from = SWP) %>% 
  column_to_rownames("name") %>% 
  select_if(colSums(.) != 0)

nextspecdl <- nextd %>% 
  dplyr::select(c(SWP, ANCNUM, EUPDIO, LYCHYL, SATEUR)) %>% 
  pivot_longer(., ANCNUM:SATEUR) %>% 
  pivot_wider(., names_from = SWP) %>% 
  column_to_rownames("name") %>% 
  select_if(colSums(.) != 0)


nextgend <- as.list(nextgendl)
nextspecd <- as.list(nextspecdl)


# calculate iNEXT object
outrich_gen <- iNEXT(nextgend, q=0 ,datatype="abundance") # use min and max abundances observed for size
outrich_spec <- iNEXT(nextspecd, q=0 ,datatype="abundance")
# extract sample coverage info
# n = sample size, S.obs = species richness, SC = sample coverage
# calculate diversity values at the lowest sample coverage value
# q(order) 0 = species richness, 1 = Shannon diversity, 2 = Simpson diversity
cov_gen <- min(outrich_gen$DataInfo$SC)
cov_spec <- min(outrich_spec$DataInfo$SC)

rarediv_gen <- estimateD(nextgendl, datatype = "abundance", base = "coverage", 
                         level= cov_gen, conf=0.95) %>% 
  mutate(Niche.Breadth = "Generalist")


rarediv_spec <- estimateD(nextspecdl, datatype = "abundance", base = "coverage", 
                          level= cov_spec, conf=0.95) %>% 
  mutate(Niche.Breadth = "Specialist")


rarediv <- rbind(rarediv_gen, rarediv_spec) %>% 
  pivot_wider(., id_cols = c(Assemblage, Niche.Breadth), names_from = Order.q,
              names_sep = ".", values_from = c(m, SC, qD)) %>% 
  dplyr::select(-c(m.1, m.2, SC.1, SC.2)) %>% 
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
butt_site <- inner_join(buttab_site, rarediv)
write_csv(butt_site, "output/ButterflyCleanbySite_noCW.csv")

# Figures -----------------------------------------------------------------

gen_1 <- ggiNEXT(outrich_gen, type= 1) + theme(legend.position = 'none')
gen_2 <- ggiNEXT(outrich_gen, type= 2) + theme(legend.position = 'none')
gen_3 <- ggiNEXT(outrich_gen, type= 3) + theme(legend.position = 'none')

gen_fig <- gen_1 | gen_2 | gen_3

ggsave('figures/Generalist_SamplingCov_noCW.png', gen_fig, width = 15, height = 15, units = 'in')
