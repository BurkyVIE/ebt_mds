library(tidyverse)
library(rnaturalearth)
library(sf)

# EBT-Grid of Europe
raster <- list(long = seq(-12, 54, length = 158),
               lat = seq(29, 71, length = 151))
grid <- crossing(lat = raster$lat %>% head(-1),     # letzter Punkt ergibt sich rechnerisch, daher hier weg
                 long = raster$long %>% head(-1)) %>%
  rowid_to_column(var = "ID") %>% 
  mutate(dot = map2(.x = long, .y = lat,   # corners to all squares (must be closed)
                    .f = ~ matrix(c(..1 + c(0, 1, 1, 0, 0) * 66/157, ..2 + c(0, 0, 1, 1, 0) * 42/150), ncol = 2) %>% list() %>% st_polygon())) %>% 
  st_as_sf(sf_column_name = "dot", crs = 4326) %>%
  st_set_agr("constant")
rm(raster)

# Country borders
map_world <- ne_countries(scale = 10, returnclass = "sf")[1] %>%
  st_transform(crs = 4326) %>%
  st_set_agr("constant")

# Map of Austria - or other country/-ies if desired
map_at <- ne_countries(country = c("Austria"), scale = 10, returnclass = "sf")[1] %>%
  st_transform(crs = 4326) %>%
  st_set_agr("constant")

# Locations
locs <- pseudo %>%
  select(Loc, Country, Coords) %>% 
  separate(col = Coords, into = c("Long", "Lat"), sep = "~", convert = TRUE) %>% 
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>%
  st_set_agr("constant")

# Bounding box for data reduction
xtra <- 2  # Additional Dots on each side
#box <- (st_bbox(map_at) + 3 *c(-66/157, -42/150, 66/157, 42/150))[c(1, 1, 3, 3, 1, 2, 4, 4, 2, 2)] %>% # add x times size of one dot to every side
box <- (c(-12 + (53 - xtra) * 66/157, 29 + (64 - xtra) * 42/150, -12 + (68 + xtra) * 66/157, 29 + (70 + xtra) * 42/150) + 2.5 *c(-66/157, -42/150, 66/157, 42/150))[c(1, 1, 3, 3, 1, 2, 4, 4, 2, 2)] %>% # add x times size of one dot to every side
  matrix(ncol = 2) %>%
  list() %>%
  st_polygon() %>%
  st_sfc(crs = 4326)
rm(xtra)

# Data reduction
map_world <- st_intersection(map_world, box)
grid <- st_intersection(grid, box)
locs <- st_intersection(locs, box)

# Dots mit Location
been <- st_intersection(grid, locs) %>%
  group_by(ID) %>%
  count()
visited <- st_intersection(box, grid %>% filter(ID %in% been$ID))
rm(been)

# Seperate at and non-at locs
locs <- locs %>% mutate(Origin = case_when(Country == "Austria" ~ "AT",
                                           TRUE ~ "non AT"))

# Projection transformation
map_world <- st_transform(map_world, crs = 3416)
map_at <- st_transform(map_at, crs = 3416)
grid <- st_transform(grid, crs = 3416)
locs <- st_transform(locs, crs = 3416)
visited <- st_transform(visited, crs = 3416)

#Farben
colors <- c("wheat", "grey65", "red3", "grey35", "blue3", "mediumblue")

# Plot
p <- ggplot() +
  geom_sf(data = visited, color = NA, fill = colors[1], alpha = .9) +
  geom_sf(data = map_world, color = colors[2], size = 2/2, fill = NA) +
  geom_sf(data = map_at, color = colors[3], size = 3/2, fill = NA) +
  geom_sf(data = grid, color = colors[4], fill = NA) +
  geom_sf(data = locs, mapping = aes(color = Origin), size = 5/4, alpha = 1/2, show.legend = FALSE) +
  scale_color_manual(values = c("AT" = colors[5], "non AT" = colors[6])) +
  #  coord_sf(xlim = st_bbox(map_at)[c(1, 3)], ylim = st_bbox(map_at)[c(2, 4)]) +
  labs(title = "Locations / Dots in and around Austria by Burky",
       subtitle = "Projection = WGS 89 / Austria Lambert (EPSG:3416)",
       #caption = "Projection = WGS 84 (EPSG:4326)") +
       caption = "http://www.eurobilltracker.com") +
  theme_ebt()

plot(p)
rm(grid, visited, map_at, map_world, locs, box, colors, p)
