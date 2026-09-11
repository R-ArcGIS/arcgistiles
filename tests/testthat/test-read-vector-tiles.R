test_that("vector tiles decode into named sf layers", {
  skip_if_no_network()
  skip_if_not_installed("protolite")

  tiles <- boston_vector_tiles()
  layers <- vector_tile_layers(tiles)

  expect_type(layers, "character")
  expect_true(length(layers) > 0L)
  expect_false(is.unsorted(layers))

  parts <- read_vector_tiles(tiles, layers = layers[[1L]])

  expect_named(parts, layers[[1L]])
  expect_s3_class(parts[[1L]], "sf")
})

test_that("decoded features fall inside the tiles they came from", {
  skip_if_no_network()
  skip_if_not_installed("protolite")

  tiles <- boston_vector_tiles()
  parts <- read_vector_tiles(tiles, crs = 3857)

  expect_true(length(parts) > 0L)

  bbox <- sf::st_bbox(parts[[1L]])
  extent <- tileset_bbox(tiles)

  expect_gte(wk::rct_xmin(bbox), wk::rct_xmin(extent) - 1)
  expect_lte(wk::rct_xmax(bbox), wk::rct_xmax(extent) + 1)
  expect_equal(sf::st_crs(parts[[1L]]), sf::st_crs(3857))
})

test_that("an unknown layer names the ones that exist", {
  skip_if_no_network()
  skip_if_not_installed("protolite")

  expect_error(
    read_vector_tiles(boston_vector_tiles(), layers = "not-a-layer"),
    "not in these tiles"
  )
})

test_that("raster tiles cannot be decoded as vector tiles", {
  skip_if_no_network()
  skip_if_not_installed("protolite")

  service <- map_server(world_imagery_url())
  tiles <- get_tiles(service, service@full_extent, level = 1L, progress = FALSE)

  expect_error(read_vector_tiles(tiles), "TileSet.*vector tiles")
})

