library(arcgistiles)

ms <- map_server(world_imagery_url())

bbox <- sf::st_bbox(
  c(xmin = -71.2, ymin = 42.3, xmax = -71.0, ymax = 42.4),
  crs = sf::st_crs(4326)
)

level_for_size(ms, bbox, c(1024, 1024))

grid <- tile_grid(ms, bbox, 13L)
nrow(grid)
head(grid)

tilemap(ms, 13L, 3029L, 2475L, 4L, 4L)

tiles <- get_tiles(ms, bbox, level = 13L, progress = FALSE)
tiles

tileset_bbox(tiles)

boston <- as_rast(tiles)
boston

terra::writeRaster(
  boston,
  file.path(tempdir(), "boston-imagery.tif"),
  overwrite = TRUE
)
