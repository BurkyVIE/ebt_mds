library(leaflet)

locs <- pseudo |>
  # filter(Country == "Austria") |>
  separate(Coords, sep = "~", into = c("Long", "Lat"), convert = TRUE) |> 
  (\(x) st_as_sf(x, coords = c("Long", "Lat")))()

leaflet() |> 
  addTiles() |> 
  addCircleMarkers(data = locs,
             label = paste(locs$ZIP, locs$City), 
             radius = 7,
             group = "Locs")
