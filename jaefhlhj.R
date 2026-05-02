library(leaflet)
library(sf)

# Transform to WGS84
map_fl_districts_long_wgs84 <- st_transform(map_fl_districts_long, crs = 4326)

leaflet(data = map_fl_districts_long_wgs84) |>
  addTiles() |>
  addPolygons(
    fillColor = ~colorNumeric("Blues", score)(score),
    fillOpacity = 0.3,
    color = "white",
    weight = 1,
    popup = ~paste0(
      "<b>District:</b> ", cd_2020, "<br>",
      "<b>Score:</b> ", round(score, 4)
    ),
    label = ~paste0("District ", cd_2020, " - ", round(score, 1))
  ) |>
  addLegend(
    position = "bottomright",
    pal = colorNumeric("Blues", map_fl_districts_long_wgs84$score),
    values = ~score,
    title = "Score",
    opacity = 0.8
  )
