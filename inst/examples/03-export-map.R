library(arcgistiles)

census <- map_server(census_url())

img <- export_map(census, c(-104, 35.6, -94.32, 41), size = c(600, 400))
img

img@bbox

img@scale

states <- export_map(
  census,
  c(-104, 35.6, -94.32, 41),
  size = c(600, 400),
  layers = 3,
  visibility = "show",
  layer_defs = list(`3` = "POP2000 > 1000000"),
  transparent = TRUE,
  format = "png32"
)
states

as_rast(img)
