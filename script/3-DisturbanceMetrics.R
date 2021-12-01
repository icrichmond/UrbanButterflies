#### PACKAGES #### 
p <- c("data.table", "dplyr", "stringr", "sf", "sp")
lapply(p, library, character.only = T)

#### DATA ####
dist <- fread("input/SamplingArea.csv")
landcov <- st_read("large/LandCover2011_Rivers.shp")
swf <- fread("input/CoorMetre.csv", quote="")

#### DISTURBANCE CLASSIFICATION #### 
# set NA = 0
dist$PerCut[is.na(dist$PerCut)] <- 0 
# ponds with >= 30% mowed, classify them as disturbed 
# ponds with < 30% mowed, classify them as undisturbed 
dist <- dist %>% 
  mutate(Disturbance = if_else(PerCut >= 30, "Disturbed", "Undisturbed"))

#### LANDCOVER ####
## Stormwater Facilities
# pond coordinates were obtained from the City of Ottawa and have 
# a Transverse Mercator projection in meters and a GCS of 
# GCS North American 1983 (EPSG 4269)
coordinates(swf) <- c("X_Meters", "Y_Meters")
proj4string(swf) <- CRS("+proj=tmerc +lat_0=0 +lon_0=-76.5 +k=0.9999 +x_0=304800 +y_0=0 +datum=NAD83 +units=m")
swf <- st_as_sf(swf)
swf <- rename(swf, Pond = "Pond Name")
swf$Pond <- str_remove(swf$Pond, "SWF-")
swf$Pond <- as.double(swf$Pond)
studyponds <- inner_join(swf, dist)
## Buffers
# transform to a metric coordinate system that matches the landcover layer (units = m)
studyponds_m <- st_transform(studyponds, 32189)
landcov <- st_transform(landcov, 32189)
# produce 2 km and 5 km buffers
buffer2 <- st_buffer(studyponds_m, 2000)
buffer5 <- st_buffer(studyponds_m, 5000)
## Calculate intersections 
int2 <- st_intersection(buffer2, landcov)
int5 <- st_intersection(buffer5, landcov)
int2 <- st_make_valid(int2)
int5 <- st_make_valid(int5)
## Metrics of disturbance
dist2 <- int2 %>%
  group_by(Pond, LABEL) %>%
  summarise(geometry = st_union(geometry))
dist5 <- int5 %>%
  group_by(Pond, LABEL) %>% 
  summarise(geometry = st_union(geometry))
# area

#### SAVE ####
saveRDS(studyponds, "large/StudyPondsSpatial.rds")
saveRDS(buffer2, "large/TwoKMBuffer.rds")
saveRDS(buffer5, "large/FiveKMBuffer.rds")
saveRDS(int2, "large/TwoKMInt.rds")
saveRDS(int5, "large/FiveKMInt.rds")
