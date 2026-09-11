test_that("tiles download and carry their extents", {
  skip_if_no_network()

  service <- map_server(world_imagery_url())
  tiles <- get_tiles(service, service@full_extent, level = 2L, progress = FALSE)

  expect_s3_class(tiles, "arcgistiles::TileSet")
  expect_equal(nrow(tiles@tiles), 16L)
  expect_true(all(tiles@tiles[["ok"]]))
  expect_true(all(file.exists(tiles@tiles[["path"]])))
  expect_true(all(file.size(tiles@tiles[["path"]]) > 0))
})

test_that("a tile set bounding box covers every tile", {
  skip_if_no_network()

  service <- map_server(world_imagery_url())
  tiles <- get_tiles(service, service@full_extent, level = 1L, progress = FALSE)
  bbox <- tileset_bbox(tiles)

  expect_s3_class(bbox, "bbox")
  expect_equal(bbox[["xmin"]], min(tiles@tiles[["xmin"]]))
  expect_equal(bbox[["ymax"]], max(tiles@tiles[["ymax"]]))
})

test_that("tiles become a georeferenced raster", {
  skip_if_no_network()
  skip_if_not_installed("terra")

  service <- map_server(world_imagery_url())
  tiles <- get_tiles(service, service@full_extent, level = 1L, progress = FALSE)
  raster <- as_rast(tiles)

  expect_s4_class(raster, "SpatRaster")
  expect_equal(terra::crs(raster, describe = TRUE)$code, "3857")

  extent <- as.vector(terra::ext(raster))
  expect_equal(unname(extent[["xmin"]]), min(tiles@tiles[["xmin"]]))
  expect_equal(unname(extent[["ymax"]]), max(tiles@tiles[["ymax"]]))
})

test_that("vector tiles download as pbf", {
  skip_if_no_network()

  service <- vector_tile_server(open_street_map_url())
  tiles <- get_vector_tiles(service, service@full_extent, level = 1L, progress = FALSE)

  expect_equal(tolower(tiles@format), "pbf")
  expect_true(any(tiles@tiles[["ok"]]))
  expect_true(all(grepl("\\.pbf$", tiles@tiles[["path"]])))
})

test_that("vector tiles cannot be rasterized", {
  skip_if_no_network()
  skip_if_not_installed("terra")

  service <- vector_tile_server(open_street_map_url())
  tiles <- get_vector_tiles(service, service@full_extent, level = 0L, progress = FALSE)

  expect_error(as_rast(tiles), "Vector tiles")
})

test_that("get_vector_tiles rejects a map service", {
  skip_if_no_network()

  expect_error(
    get_vector_tiles(map_server(world_imagery_url()), c(0, 0, 1, 1), level = 1L),
    "VectorTileServer"
  )
})
