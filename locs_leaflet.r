library(leaflet)

## Locations ----
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

## enlarge raster ----
i <- 50

# initialization ----
raster <- list(long = seq(-12, 54, length = 158), 
               lat = seq(29, 71, length = 151))
grid <- crossing(lat = raster$lat,
                 long = raster$long) %>%
  rowid_to_column(var = "ID") %>% 
  mutate(dot = map2(.x = long, .y = lat,   # corners to all squares (must be closed)
                    .f = ~ matrix(c(..1 + c(0, 1, 1, 0, 0) * 66/157, ..2 + c(0, 0, 1, 1, 0) * 42/150), ncol = 2) %>% list() %>% st_polygon())) %>% 
  st_as_sf(sf_column_name = "dot", crs = 4326) %>% 
  st_set_agr(., "constant")
rm(i, raster)

## Dots mit Location ----
visited <- st_join(grid, locs, join = st_covers) %>%
  filter(!is.na(Loc)) %>% 
  select(-c(First, Country:Loc)) %>% # no doubles; also unique next line
  # select(-Country) %>%
  unique()

leaflet() |> 
  addTiles() |> 
  addCircleMarkers(data = locs,
             label = paste(locs$ZIP, locs$City), 
             weight = 2,
             radius = 5,
             color = "navy",
             group = "Locs") |> 
  addPolygons(data = grid,
              weight = .5,
              color = "red",
              fill = FALSE,
              group = "Dots") |> 
  addPolygons(data = visited,
              weight = .5,
              color = "gold",
              group = "Visit")

rm(locs, grid, visited)