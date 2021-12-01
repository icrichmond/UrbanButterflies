#### PACKAGES #### 
p <- c("readr", "dplyr", "tidyr", "data.table", "sf", "sp", "stringr")
lapply(p, library, character.only = T)

#### DATA ####
dist <- read_csv("input/SamplingArea.csv")
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
# transform to a metric coordinate system 
studyponds_km = st_transform(studyponds, "+proj=utm +zone=18N +datum=WGS84 +units=km")
allpoints_km = st_transform(allpoints, "+proj=utm +zone=18N +datum=WGS84 +units=km")
# produce 2 km buffer around study ponds because it is a biologically relevant distance 
# for dispersal- consistent with Perron et al 2021
studyponds_buffer <- st_buffer(studyponds_km, 2)

#### Calculate Intersections #### 
# rename pond name column in points and buffers to reduce confusion 
allpoints_km <- rename(allpoints_km, IntersectingPonds = OBJECTID)
studyponds_buffer <- rename(studyponds_buffer, Buffer = OBJECTID)
# calculate intersection for each buffer 
int <- st_intersection(studyponds_buffer, allpoints_km)
# how many intersections within 2 km occur for each study pond
# filter less than 6 because intersections include the study pond in the buffer 
inttally <- int %>%
  group_by(Buffer)%>%
  tally %>%
  filter(n < 6)

# 2 ponds do not have 5 intersections within 2 km, SWF-1133, SWF-1501, SWF-1622
# calculate the 5 nearest neighbours for these two ponds
# convert studyponds_km back to sp 
allpoints_km_sp <- as_Spatial(allpoints_km)
# convert sp to ppp for spatsstat package 
allpoints_km_ppp <- as.ppp(allpoints_km_sp)
# find which ponds are the nearest neighbours 
nn <- nnwhich(allpoints_km_ppp, k=1:5)
nn <- as.data.table(nn)
nn <- nn %>%
  mutate(Pond = allpoints$OBJECTID)
nn2 <-
  reshape2::dcast(mutate(
    reshape2::melt(nn, id.var="Pond"), 
    value = plyr::mapvalues(
      value, c(seq(1:18602)), c(allpoints_km_ppp$marks))
  ), Pond~variable)

# select SWF-1133, SWF-1501
nn_sub <- filter(nn2, Pond == "SWF-1133" | Pond == "SWF-1501" | Pond == "SWF-1622")
# melt to match format of int 
nn_sub_m <- melt(nn_sub, id.var="Pond")
nn_sub_m <- nn_sub_m %>%
  rename(Buffer = Pond) %>%
  rename(IntersectingPonds = value) %>%
  select(-variable)
# remove geometry from int 
int_fin <- st_set_geometry(int, NULL)
# remove rows of SWF-1133, SWF-1501, and SWF-1622 from int to avoid duplicates
int_fin <- filter(int_fin, ! Buffer %in% c("SWF-1133", "SWF-1501", "SWF-1622") )
# add SWF-1133 and SWF-1501 to int dataset 
int_fin <- rbind(nn_sub_m, int_fin)

#### Extract Coordinates ####
# we want the coordinates for point 1 and point 2 in our dataset 
# Rename Buffer to OBJECTID for first join 
int_fin <- rename(int_fin, OBJECTID = Buffer)
int_fin_pt1 <- left_join(int_fin, allpoints_coords, by = "OBJECTID")
int_fin_pt1 <- int_fin_pt1 %>%
  rename(Point1 = OBJECTID) %>%
  rename(X1 = X) %>%
  rename(Y1 = Y) %>%
  rename(OBJECTID = IntersectingPonds)
int_fin_pt2 <- left_join(int_fin_pt1, allpoints_coords, by = "OBJECTID")
int_fin_pt2 <- int_fin_pt2 %>%
  rename(Point2 = OBJECTID) %>%
  rename(X2 = X) %>%
  rename(Y2 = Y)

int_fin_full <- select(int_fin_pt2, c(-geometry.x,-geometry.y))
# save this as node dataset for SB
fwrite(int_fin_full, "input/cleaned/PairwiseResistance2km.csv")

# now stack data and pull out unique pond IDs and coordinates for 
# full list of points needed in Circuitscape 
int_stack <- stack(int_fin)
# remove duplicates 
int_stack <- int_stack[!duplicated(int_stack$values),]
# get coordinates 
int_stack <- int_stack %>%
  rename(OBJECTID = values) %>%
  select(-ind)

int_stack <- inner_join(int_stack, allpoints_coords, by = "OBJECTID")
fwrite(int_stack, "input/cleaned/PondCoords2km.csv")

# save 
write_csv(dist, "output/SamplingAreaClean.csv")
