#### PACKAGES #### 
p <- c("readr", "dplyr", "tibble", "stringr", "tidyr", "vegan")
lapply(p, library, character.only = T)

#### DATA ####
buttraw <- read_csv("input/ButterflyRawData.csv")

#### SPECIES NAMES ####
# select column names that correspond to species 
# first row of dataset is scientific names 
buttsp <- buttraw[1,15:147]
# need to transpose the dataset 
buttsp <- as.data.frame(t(buttsp[,-1]))
# convert row names to a column and add genus and species columns
buttsp <- buttsp %>% 
  rownames_to_column("CommonNames") %>% 
  rename(ScientificNames = V1) %>% 
  mutate(Genus = word(ScientificNames, 1)) %>% 
  mutate(species = word(ScientificNames, 2))
# select butterflies only 
buttsp <- buttsp[1:104,]
# convert NAs and blanks to "sp." in species column
buttsp$species[buttsp$species == ""] <- NA 
buttsp$Genus <- replace_na(buttsp$Genus, "UNK")
buttsp$species <- replace_na(buttsp$species, "sp")
# fix typo 
buttsp[buttsp$CommonNames=="Mustard White", "species"] <- "oleracea"
# add species code
buttsp <- mutate(buttsp, SpeciesCode = toupper(paste0(str_sub(Genus, 1, 3), "", str_sub(species, 1, 3))))


#### ABUNDANCE & SPECIES RICHNESS ####
# separate columns with species names so they can be replaced with species codes
buttab1 <- buttraw[3:86, 1:14]
buttab2 <- buttraw[3:86, 16:119]
names(buttab2) <- buttsp$SpeciesCode[match(names(buttab2), buttsp$CommonNames)]
# bind datasets back together 
buttab <- cbind(buttab1, buttab2)
# calculate abundance and species richness per site on each visit
buttab[15:118] <- sapply(buttab[15:118],as.numeric)
buttab <- buttab %>% 
  mutate(abund = rowSums(across(HESSP:UNKSP), na.rm = T))

#### DIVERSITY #### 
#  Shannon diversity 
shan <- buttraw[3:86, 16:119]
names(shan) <- buttsp$SpeciesCode[match(names(shan), buttsp$CommonNames)]
shan <- sapply(shan,as.numeric)
shan <- replace_na(shan, 0)
div <- as.data.frame(diversity(shan, "shannon"))
div <- cbind(buttraw[3:86,1:2], div)
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
buttdate <- inner_join(buttab, div)
# select variables of interst 
buttdate <- buttdate %>% 
  select(c("SWP", "Date", "VisitNumber", "CloudCover", "WindSpeed", "Temp",
           "StartTime", "EndTime", "Observer", "DisturbanceCat", "TreeCover",
           "MidCanopy", "PerNatMA", "NatRichnessMA", "abund", "Shannon", 
           "Even", "Simpson", "SpeciesRichness"))

#### SITE ONLY ####
buttab1 <- buttraw[3:86, 1:14]
buttab2 <- buttraw[3:86, 16:119]
names(buttab2) <- buttsp$SpeciesCode[match(names(buttab2), buttsp$CommonNames)]
buttab <- cbind(buttab1, buttab2)
buttab[15:118] <- sapply(buttab[15:118],as.numeric)
# abundance
buttab_site <- buttab %>%
  group_by(SWP) %>% 
  summarise(abund = sum(across(HESSP:UNKSP), na.rm = T))
# avg. diversity
buttdiv_site <- buttdate %>%
  group_by(SWP) %>%
  summarise_at(vars(Shannon, Even, Simpson), "mean", na.rm = T)
butt_site <- inner_join(buttab_site, buttdiv_site)
# species richness
butt_rich <- buttab[, c(1,15:118)]
butt_rich <- butt_rich %>%
  group_by(SWP) %>%
  summarise(across(HESSP:UNKSP, ~sum(.x, na.rm = TRUE)))
butt_rich <- butt_rich[,2:105]
spec_site <- as.data.frame(specnumber(butt_rich))
butt_site <- rename(butt_site, SpeciesRichness = `specnumber(butt_rich)`)
butt_site <- cbind(butt_site, spec_site)


#### SAVE ####
write_csv(buttsp, "output/ButterflySpecies.csv")
write_csv(buttdate, "output/ButterflyCleanbyDate.csv")
write_csv(butt_site, "output/ButterflyCleanbySite.csv")
