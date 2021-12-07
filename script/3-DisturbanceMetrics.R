#### PACKAGES #### 
p <- c("data.table", "dplyr", "stringr", "purrr", "sf", "sp")
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
# produce 20 m, 50 m, 100 m, 200 m, 500 m, 1 km, 2 km, and 5 km buffers
b <- c(20, 50, 100, 200, 500, 1000, 2000, 5000)
# calculate buffers at each distance for all study ponds
buffers <- purrr::map(.x = b,
             .f = function(x){st_buffer(studyponds_m, x)}) %>%
  purrr::set_names(., nm = paste0("buffer", b))
## Intersect buffers with landcover
ints <- purrr::map(.x = buffers, .f = function(x){st_intersection(x, landcov)})%>%
  purrr::set_names(., nm = paste0("int", b))
ints <- purrr::map(.x = ints, .f = function(x){st_make_valid(x)})
## Metrics of disturbance
dists <- purrr::map(.x = ints, .f = function(x){x %>% group_by(Pond, LABEL) %>% summarise(geometry = st_union(geometry))}) %>% 
  purrr::set_names(., nm = paste0("dist", b))
dists <- purrr::map(.x = dists, .f = function(x){x %>% group_by(Pond, LABEL) %>% mutate(area = st_area(geometry))})
anthro <- purrr::map(.x = dists, .f = function(x){x %>% group_by(Pond) %>% summarise(totarea = sum(area), 
                                                                                     settarea = sum(area[LABEL == 'Settlement']),
                                                                                     roadarea = sum(area[LABEL == 'Transportation']),
                                                                                     anthroarea = sum(area[LABEL == 'Settlement' | LABEL == 'Transportation']),
                                                                                     settper = settarea/totarea, 
                                                                                     roadper = roadarea/totarea,
                                                                                     anthroper = anthroarea/totarea)}) 
anthrofull <- purrr::map(.x = anthro, .f = function(x){st_join(x, studyponds_m, by = "Pond")}) %>% 
  purrr::set_names(., nm = paste0("anthro",b))

#### SAVE ####
saveRDS(studyponds, "large/StudyPondsSpatial.rds")
lapply(names(buffers), function(df) saveRDS(buffers[[df]], file=paste0("large/", df, ".rds")))
lapply(names(ints), function(df) saveRDS(ints[[df]], file=paste0("large/", df, ".rds")))
lapply(names(anthrofull), function(df) saveRDS(anthrofull[[df]], file=paste0("large/", df, ".rds")))

