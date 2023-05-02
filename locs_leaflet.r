library(leaflet)

## Locations ----
locs <- pseudo %>%
  separate(Coords, into = c("Long", "Lat"), sep = "~", convert = TRUE) %>% 
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>%
  st_set_agr(., "constant")

## enlarge raster ----
i <- 50

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
             weight = 3,
             radius = 7,
             color = "red",
             group = "Locs") |> 
  addPolygons(data = grid,
              weight = 1,
              fill = FALSE,
              group = "Dots") |> 
  addPolygons(data = visited,
              weight = 1,
              group = "Visit")

rm(locs, grid, visited)