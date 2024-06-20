source('script/0-Packages.R')

# Data --------------------------------------------------------------------
dist <- fread("input/SamplingArea.csv")
landcov <- st_read("large/LandCover2011_Rivers.shp")
swf <- fread("input/CoorMetre.csv", quote="")


# Landcover ---------------------------------------------------------------

## Stormwater Facilities
# pond coordinates were obtained from the City of Ottawa and have 
# a Transverse Mercator projection in meters and a GCS of 
# GCS North American 1983 (EPSG 4269)
swf <- st_as_sf(swf, coords = c("X_Meters", "Y_Meters"), crs = "+proj=tmerc +lat_0=0 +lon_0=-76.5 +k=0.9999 +x_0=304800 +y_0=0 +datum=NAD83 +units=m")
swf <- rename(swf, Pond = "Pond Name")
swf$Pond <- str_remove(swf$Pond, "SWF-")
swf$Pond <- as.double(swf$Pond)
studyponds <- inner_join(swf, dist)

## Buffers
# transform to a metric coordinate system that matches the landcover layer (units = m)
studyponds_m <- st_transform(studyponds, 32189)
landcov <- st_transform(landcov, 32189)
# produce 400 m buffer as per Rivest & Kharouba 2021
buffer <- st_buffer(studyponds_m, 400)

## Intersect buffers with landcover
ints <- st_intersection(buffer, landcov) %>% 
  st_make_valid()

## Metrics of disturbance
un <- ints %>% 
  group_by(Pond, LABEL) %>% 
  summarise(geometry = st_union(geometry))

dists <- un %>% 
  group_by(Pond, LABEL) %>% 
  mutate(area = st_area(geometry))

anthro <- dists %>% 
  group_by(Pond) %>% 
  summarise(totarea = sum(area), 
            settarea = sum(area[LABEL == 'Settlement']),
            roadarea = sum(area[LABEL == 'Transportation']),
            anthroarea = sum(area[LABEL == 'Settlement' | LABEL == 'Transportation']),
            settper = settarea/totarea, 
            roadper = roadarea/totarea,
            anthroper = anthroarea/totarea)

studyponds_df <- st_set_geometry(studyponds_m, NULL)
anthrofull <- inner_join(anthro, studyponds_df, by = "Pond")


# Save --------------------------------------------------------------------
write_sf(studyponds, "output/StudyPondsSpatial.gpkg")
write_sf(anthrofull, "output/AnthroFull.gpkg")
