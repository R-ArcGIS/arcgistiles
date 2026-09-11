test_that("a cached map service reports its tiling scheme", {
  skip_if_no_network()

  service <- map_server(world_imagery_url())

  expect_s3_class(service, "arcgistiles::MapServer")
  expect_true(service@cached)
  expect_s3_class(service@tile_info, "arcgistiles::TileInfo")
  expect_true(nrow(service@tile_info@lods) > 0L)
  expect_false(is.na(service@crs))
})

test_that("a dynamic map service has no tiling scheme", {
  skip_if_no_network()

  service <- map_server(census_url())

  expect_false(service@cached)
  expect_null(service@tile_info)
  expect_error(tile_info(service), "not a cached service")
})

test_that("a vector tile service reports pbf tiles", {
  skip_if_no_network()

  service <- vector_tile_server(open_street_map_url())

  expect_s3_class(service, "arcgistiles::VectorTileServer")
  expect_equal(tolower(service@tile_info@format), "pbf")
  expect_true(length(service@tiles) > 0L)
})

test_that("a service bounding box carries its crs", {
  skip_if_no_network()

  bbox <- map_server(world_imagery_url())@full_extent

  expect_s3_class(bbox, "bbox")
  expect_false(is.na(sf::st_crs(bbox)))
})

test_that("the wrong endpoint kind is rejected before any request", {
  expect_error(map_server("https://example.com/rest/services/Thing/FeatureServer"), "MapServer")
  expect_error(vector_tile_server(world_imagery_url()), "VectorTileServer")
})

test_that("tilemap reports which tiles the cache holds", {
  skip_if_no_network()

  service <- map_server(world_imagery_url())
  available <- tilemap(service, 6L, 24L, 18L, 4L, 4L)

  expect_equal(nrow(available), 16L)
  expect_true(all(c("level", "row", "col", "available") %in% names(available)))
  expect_type(available[["available"]], "logical")
})
