# Libraries ----
library(tidyverse)
library(leaflet)
library(sf)

# Locations ----
locs <- pseudo %>%
  separate(Coords, into = c("Long", "Lat"), sep = "~", convert = TRUE) %>% 
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>%
  st_set_agr(., "constant")

# https://forum.eurobilltracker.com/viewtopic.php?t=5229&postdays=0&postorder=asc&start=9
# For reference, here are the current parameters for the grid:
#   Europe:
# - minimum latitude: 29
# - maximum latitude: 71
# - minimum longitude: -12
# - maximum longitude: 54
# - grid size: 157 x 150 (up from 150x150)

# Grid ----
raster <- list(long = seq(-12, 54, length = 158), 
               lat = seq(29, 71, length = 151))
grid <- crossing(lat = raster$lat,
                 long = raster$long) %>%
  rowid_to_column(var = "ID") %>% 
  mutate(dot = map2(.x = long, .y = lat,   # corners to all squares (must be closed)
                    .f = ~ matrix(c(..1 + c(0, 1, 1, 0, 0) * 66/157, ..2 + c(0, 0, 1, 1, 0) * 42/150), ncol = 2) %>% list() %>% st_polygon())) %>% 
  st_as_sf(sf_column_name = "dot", crs = 4326) %>% 
  st_set_agr(., "constant")

# Dots mit Location ----
visited <- st_join(grid, locs, join = st_covers) %>%
  filter(!is.na(Loc)) %>% 
  select(-c(First, Country:Loc)) %>% # no doubles; also unique next line
  # select(-Country) %>%
  unique()

# Plot ----
leaflet() |> 
  addTiles(group = "OSM (default)") |> 
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") |> 
  addPolygons(data = grid,
              weight = 1,
              color = "red",
              fill = FALSE,
              group = "Dots - Europe") |> 
  addPolygons(data = visited,
              weight = 0,
              color = "navy",
              group = "Visited Dots") |> 
  addCircleMarkers(data = locs,
                   label = paste(locs$ZIP, locs$City), 
                   weight = 2,
                   radius = 21,
                   color = "navy",
                   group = "Locations",
                   clusterOptions = markerClusterOptions(),
                   labelOptions = labelOptions()) |> 
  addLayersControl(baseGroups = c("OSM (default)", "Toner Lite"),
                   overlayGroups = c("Dots - Europe", "Visited Dots", "Locations"),
                   options = layersControlOptions(collapsed=FALSE)) |> 
  hideGroup("Locations")

# saveWidget(map, file = "map1.html", selfcontained = FALSE)

# Clean up ----
rm(locs, raster, grid, visited)
