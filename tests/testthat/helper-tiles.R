web_mercator_info <- function(levels = 0:5, size = 256L) {
  resolution <- 156543.03392800014 / 2^levels

  TileInfo(
    rows = size,
    cols = size,
    dpi = 96L,
    format = "PNG",
    origin = c(-20037508.342787, 20037508.342787),
    crs = sf::st_crs(3857),
    lods = data.frame(
      level = as.integer(levels),
      resolution = resolution,
      scale = resolution * 96 / 0.0254
    )
  )
}

skip_if_no_network <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
}

world_extent <- function() {
  c(-20037508.342787, -20037508.342787, 20037508.342787, 20037508.342787)
}

boston_vector_tiles <- function() {
  service <- vector_tile_server(open_street_map_url())

  bbox <- sf::st_bbox(
    c(xmin = -71.10, ymin = 42.35, xmax = -71.08, ymax = 42.36),
    crs = sf::st_crs(4326)
  )

  get_vector_tiles(service, bbox, level = 14L, progress = FALSE)
}
