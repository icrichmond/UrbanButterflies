# Spatial Figures 
# Author: Isabella Richmond 
# This script is for producing spatial figures for this manuscript

source('script/0-Packages.R')


# Data --------------------------------------------------------------------

ponds <- read_sf("output/StudyPondsSpatial.gpkg")
ponds <- st_transform(ponds, 4326)
ponds <- ponds %>%
  mutate(lat = unlist(map(ponds$geom,1)),
         long = unlist(map(ponds$geom,2)))



# Study Site --------------------------------------------------------------

# Note: for this code to work you need a Google API with Geocoding and Maps Static enabled
register_google(key = "AIzaSyDlRF5BYeskCH7qWtq13WUV5ifG9Q1kT1c")
ss <- qmap(location = "Terry Carisse Park, Ottawa, Ontario", zoom = 10, maptype = "satellite", source = "google") +
  geom_point(data = ponds, aes(x = lat, y = long), colour = "red", linewidth = 3, shape = 1, stroke = 1) + 
  coord_sf(crs = st_crs(4326), expand = FALSE) +
  theme(panel.grid = element_line(color = '#323232', size = 0.2),
        axis.text = element_text(size = 11, color = 'black'),
        axis.title = element_blank()) +
  annotation_scale(location = "bl", width_hint = 0.3, pad_y = unit(0.3, "in"))


# get bbox of map
bb <- c(
  xmin = min(ponds$lat),
  ymin = min(ponds$long),
  xmax = max(ponds$lat),
  ymax = max(ponds$long)
)



# Inset -------------------------------------------------------------------

# download OSM data 
## Canada boundary
bounds <- opq(getbb('Ontario'), timeout = 150) %>%
  add_osm_feature(key = 'admin_level', value = '4') %>%
  osmdata_sf() %>%
  unname_osmdata_sf()
mp <- bounds$osm_multipolygons
mp <- mp[mp$name %in% c("Ontario", "Québec"), ]
st_crs(mp) <- 4326

on <- ggplot(mp) + 
  geom_sf(fill = '#c1d1aa', size = 0.3) +
  geom_rect(aes(
    xmin = bb['xmin'],
    xmax = bb['xmax'],
    ymin = bb['ymin'],
    ymax = bb['ymax']),
    fill = NA,
    size = 1.5,
    color = 'red') +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())


# Use annotation custom to drop the NL inset on Fogo
# Numbers here taken from the bbox but adjusted to be placed in the bottom left
ss +
    annotation_custom(
      ggplotGrob(on),
      xmin = -75.3,
      xmax = -75.74,
      ymin = 44.96,
      ymax = 45.12
    )



# Save --------------------------------------------------------------------

ggsave("figures/StudyArea.jpg", width = 4542, height = 2882, units = "px", dpi = 450)
