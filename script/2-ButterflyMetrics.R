#### PACKAGES #### 
p <- c("readr", "dplyr", "tibble", "stringr", "tidyr", "iNEXT", "ggplot2", "vegan")
lapply(p, library, character.only = T)

#### DATA ####
buttraw <- read_csv("input/ButterflyRawData.csv")

#### SPECIES NAMES ####
# select column names that correspond to species 
# first row of dataset is scientific names 
buttsp <- filter(buttraw, `Common name` == "Scientific Name")
buttsp <- select(buttsp, `Common name`:Unknown)
# need to transpose the dataset 
buttsp <- as.data.frame(t(buttsp[,-1]))
# convert row names to a column and add genus and species columns
buttsp <- buttsp %>% 
  rownames_to_column("CommonNames") %>% 
  rename(ScientificNames = V1) %>% 
  mutate(Genus = word(ScientificNames, 1)) %>% 
  mutate(species = word(ScientificNames, 2))
# convert NAs and blanks to "sp." in species column
buttsp$species[buttsp$species == ""] <- NA 
buttsp$Genus <- replace_na(buttsp$Genus, "UNK")
buttsp$species <- replace_na(buttsp$species, "sp")
# fix typo 
buttsp[buttsp$CommonNames=="Mustard White", "species"] <- "oleracea"
# add species code
buttsp <- mutate(buttsp, SpeciesCode = toupper(paste0(str_sub(Genus, 1, 3), "", str_sub(species, 1, 3))))


#### ABUNDANCE ####
# select valid rows 
buttab <- filter(buttraw, if_any(SWP, ~ !is.na(.)))
# separate columns with species names so they can be replaced with species codes
buttab1 <- select(buttab, SWP:NatRichnessMA)
buttab2 <- select(buttab, Skippers:Unknown)
names(buttab2) <- buttsp$SpeciesCode[match(names(buttab2), buttsp$CommonNames)]
# bind datasets back together 
buttab <- cbind(buttab1, buttab2)
# transform each column to numeric 
buttab <- buttab %>%
  mutate(across(HESSP:UNKSP, as.numeric))
# remove columns where there are no observations 
buttab <- buttab %>% 
  select(where(function(x) any(!is.na(x))))
# calculate abundance and species richness per site on each visit
buttab <- buttab %>% 
  mutate(abund = rowSums(across(HESSP:DANPLE), na.rm = T))
# save
write.csv(buttab, "output/ButterflyAbundance.csv")

#### DIVERSITY & SPECIES RICHNESS #### 
#  Shannon diversity 
shan <- select(buttab, HESSP:DANPLE)
shan <- sapply(shan,as.numeric)
shan <- replace_na(shan, 0)
div <- as.data.frame(diversity(shan, "shannon"))
# Pielou's evenness
even <- as.data.frame(diversity(shan, "shannon")/(log(specnumber(shan))))
# Simpson diversity 
simp <- as.data.frame(diversity(shan, "simpson"))
# Number of species 
spec <- as.data.frame(specnumber(shan))
# join together
div <- cbind(div, even, simp, spec)
div <- div %>%
  rename(Shannon = `diversity(shan, "shannon")`, 
         Even = `diversity(shan, "shannon")/(log(specnumber(shan)))`, 
         Simpson = `diversity(shan, "simpson")`,
         SpeciesRichness = `specnumber(shan)`)
pond <- select(buttab, SWP:Date)
div <- cbind(pond, div)
buttdate <- inner_join(buttab, div)
# select variables of interst 
buttdate <- buttdate %>% 
  select(c("SWP", "Date", "VisitNumber", "CloudCover", "WindSpeed", "Temp",
           "StartTime", "EndTime", "Observer", "TreeCover",
           "MidCanopy", "PerNatMA", "NatRichnessMA", "abund", "Shannon", 
           "Even", "Simpson", "SpeciesRichness"))

#### SITE ONLY ####
# use iNEXT to calculate Shannon diversity, Simpson diversity, and species richness 
# calculate the sampling coverage in our study to standardize our values wrt sampling effort 
nextd <- sapply(buttab2, as.numeric)
nextd[is.na(nextd)] <- 0
nextd <- cbind(buttab1[,1], nextd)
nextd <- nextd %>% 
  group_by(SWP) %>% 
  summarise(across(HESSP:UNKSP, sum))
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
rarediv_w <- pivot_wider(rarediv, id_cols = site, names_from = order,
                         names_sep = ".", values_from = c(method, SC, qD))
rarediv_w <- rarediv_w %>% 
  select(-c(method.1, method.2, SC.1, SC.2)) %>% 
  rename(SWP = site, 
         method = method.0, 
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

#### NOTE: by date data is NOT corrected for sampling effort ####

#### SAVE ####
write_csv(buttsp, "output/ButterflySpecies.csv")
write_csv(buttdate, "output/ButterflyCleanbyDate.csv")
write_csv(butt_site, "output/ButterflyCleanbySite.csv")
