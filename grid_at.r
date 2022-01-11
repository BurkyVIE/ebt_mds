# libbraries ----
library(tidyverse)
library(rnaturalearth)
library(sf)

# definitions ----
## colors ----
cols <- viridis::viridis(4, direction = -1, begin = 2/10)

## enlarge raster ----
i <- 1

# initialization ----
## EBT-Grid of/around austria (based on europe-grid) ----
raster <- list(long = seq(-12 + (51-i) * 66/157, -12 + (69+i) * 66/157, length = 19+2*i), 
               lat = seq(29 + (62-i) * 42/150, 29 + (71+i) * 42/150, length = 10+2*i))
grid <- crossing(lat = raster$lat,
                 long = raster$long) %>%
  rowid_to_column(var = "ID") %>% 
  mutate(dot = map2(.x = long, .y = lat,   # corners to all squares (must be closed)
                    .f = ~ matrix(c(..1 + c(0, 1, 1, 0, 0) * 66/157, ..2 + c(0, 0, 1, 1, 0) * 42/150), ncol = 2) %>% list() %>% st_polygon())) %>% 
  st_as_sf(sf_column_name = "dot", crs = 4326) %>% 
  st_set_agr(., "constant")
rm(i, raster)

## Map of Europe ----
map_eu <- ne_countries(continent = "Europe", scale = 10, returnclass = "sf") %>% 
  st_set_agr(., "constant")

## Locations ----
locs <- pseudo %>%
  separate(Coords, into = c("Long", "Lat"), sep = "~", convert = TRUE) %>% 
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>%
  st_set_agr(., "constant")

## Dots mit Location ----
visited <- st_join(grid, locs, join = st_covers) %>%
  filter(!is.na(Loc)) %>% 
  select(-(First:Loc)) %>% # no doubles; also unique next line
  unique()
  
## Box ----
box <- st_bbox(grid) %>%
  st_as_sfc(crs = 4326)

## Crop- Data reduction ----
mapngrid <- st_intersection(grid, map_eu[box,]) #map plus grid because double lines otherwise after transformation
locs <- locs[grid,] # faster than st_crop(locs, grid)

# Plot ----
p <- ggplot() +
  geom_sf(data = visited, color = NA, fill = cols[1], alpha = 1/5) +
  geom_sf(data = mapngrid, color = cols[3], fill = NA) +
  geom_sf(data = map_eu %>% filter(geounit == "Austria"), color = cols[2], size = 3/2, fill = NA) +
  geom_sf(data = locs, color = cols[4], size = 7/4, alpha = 1/2) +
  scale_x_continuous(expand = c(.01, .01)) +
  scale_y_continuous(expand = c(.01, .01)) +
  labs(title = "Dots and Locations In and Around AT",
       caption = "Proj = WGS 89 (EPSG:3416)") +
  coord_sf(crs = st_crs(3416)) +
  theme_ebt()

windows(15,9)
plot(p)

# clean-up ----
rm(cols, map_eu, box, grid, mapngrid, locs, visited, p)
