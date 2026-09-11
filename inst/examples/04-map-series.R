library(arcgistiles)

census <- map_server(census_url())

pages <- map_grid(c(-104, 35.6, -94.32, 41), nrow = 2, ncol = 3, crs = 4326)
pages

atlas <- map_series(census, pages, size = c(800, 600), progress = FALSE)
atlas[, c("page", "name", "ok", "xmin", "ymax", "scale")]

file.size(atlas$path)

padded <- map_grid(
  c(-104, 35.6, -94.32, 41),
  nrow = 2,
  ncol = 2,
  overlap = 0.05,
  crs = 4326
)

map_series(
  census,
  padded,
  size = c(400, 300),
  page_names = c("northwest", "northeast", "southwest", "southeast"),
  progress = FALSE
)[, c("name", "ok", "scale")]
