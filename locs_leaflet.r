# Libraries ----
library(tidyverse)
library(leaflet)
library(sf)

# Locations ----
locs <- pseudo |> 
  transmute(Loc, Label = paste(ZIP, City), Coords) |> 
  separate(Coords, into = c("Long", "Lat"), sep = "~", convert = TRUE) |> 
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) |> 
  st_set_agr("constant")

# https://forum.eurobilltracker.com/viewtopic.php?t=5229&postdays=0&postorder=asc&start=9
# For reference, here are the current parameters for the grid:
#   Europe:
# - minimum latitude: 29
# - maximum latitude: 71
# - minimum longitude: -12
# - maximum longitude: 54
# - grid size: 157 x 150 (up from 150x150)

# Grid ----
## Original Raster ----
raster <- list(long = seq(-12, 54, length = 158),
               lat = seq(29, 71, length = 151))

## Expanded Raster (Azores and Canaries) ----
# deltas are the differences between grid lines
deltas <- list(long = 66/157, # = (54 - -12) / 157
               lat = 7/25) # = (71 - 29) / 150

raster <- list(long = seq(-12 - 46 * deltas$long, 54, by = deltas$long),
               lat = seq(29 - 5 * deltas$lat, 71, by = deltas$lat))

glines <- as_tibble(
  rbind(cbind(x1 = -12, y1 = raster$lat, x2 = 54, y2 = raster$lat),
        cbind(x1 = raster$long, y1 = 29, x2 = raster$long, y2 = 71))
  ) |> 
  mutate(line = pmap(.l = list(x1, y1, x2, y2),
                     .f = ~ st_linestring(rbind(c(..1, ..2), c(..3, ..4))))) |> 
  select(-(x1:y2)) |> 
  st_as_sf(sf_column_name = "line", crs = 4326) |>  
  st_set_agr("constant")

grid <- crossing(lat = raster$lat |> head(-1),
                 long = raster$long |> head(-1)) |> 
  rowid_to_column(var = "ID") |> 
  mutate(dot = map2(.x = long, .y = lat,   # corners to all squares (must be closed)
                    .f = ~ matrix(c(..1 + c(0, 1, 1, 0, 0) * deltas$long, ..2 + c(0, 0, 1, 1, 0) * deltas$lat), ncol = 2) |>  list() |>  st_polygon())) |>  
  st_as_sf(sf_column_name = "dot", crs = 4326) |>  
  st_set_agr("constant")

# Dots mit Location ----
visited <- st_join(grid, locs, join = st_covers) |> 
  filter(!is.na(Label)) |> 
  select(-c(Loc:Label)) |> # no doubles; also unique next line
  unique()

# Plot ----
mydotmap <- leaflet() |> 
  addTiles(group = "OSM (default)") |> 
  # addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") |> 
  setView(lng = 16.36449, lat = 48.210033, zoom = 7) |> 
  addPolylines(data = glines,
              weight = 2,
              color = "tomato", #red
              fill = FALSE,
              group = "Dots - Europe") |> 
  addPolygons(data = visited,
              weight = 0,
              color = "darkcyan", #navy
              group = "Visited Dots") |>
  addCircleMarkers(data = locs,
                   label = locs$Label, 
                   weight = 2,
                   radius = 21,
                   color = "navy",
                   group = "Locations",
                   clusterOptions = markerClusterOptions(),
                   labelOptions = labelOptions(textsize = "12px")) |> 
  addLayersControl(#baseGroups = c("OSM (default)", "Toner Lite"),
                   overlayGroups = c("Dots - Europe", "Visited Dots", "Locations"),
                   options = layersControlOptions(collapsed = FALSE)) |> 
  hideGroup("Locations")

htmlwidgets::saveWidget(mydotmap, file = "spec/mydotmap.html", selfcontained = FALSE, title = "MyDotMap - Burky")

# Clean up ----
rm(locs, raster, glines, grid, visited, mydotmap)
