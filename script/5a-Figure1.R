# Spatial Figures 
# Author: Isabella Richmond 
# This script is for producing spatial figures for this manuscript

#### Load Packages ####
p <- c("sf", "ggplot2", "ggmap", "ggspatial", "dplyr", "purrr")
lapply(p, library, character.only=T)

#### Load Data ####
ponds <- readRDS("large/StudyPondsSpatial.rds")
ponds <- st_transform(ponds, 4326)
ponds <- ponds %>%
  mutate(lat = unlist(map(ponds$geometry,1)),
         long = unlist(map(ponds$geometry,2)))

#### Study Site Figure ####
# Note: for this code to work you need a Google API with Geocoding and Maps Static enabled
register_google(key = "AIzaSyDlRF5BYeskCH7qWtq13WUV5ifG9Q1kT1c")
qmap(location = "Terry Carisse Park, Ottawa, Ontario", zoom = 10, maptype = "satellite", source = "google") +
  geom_point(data = ponds, aes(x = lat, y = long, colour = Disturbance), size = 3) + 
  coord_sf(crs = st_crs(4326), expand = FALSE) + 
  scale_colour_viridis_d()+ 
  labs(colour = "Disturbance") +
  theme(legend.position = c(0.9, 0.15)) +
  annotation_scale(location = "tl", width_hint = 0.3) +
  annotation_north_arrow(location = "tl", which_north = "true", pad_x = unit(0.2, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) 

#### Save ####
ggsave("figures/StudyArea.jpg", width = 4542, height = 2882, units = "px", dpi = 450)
