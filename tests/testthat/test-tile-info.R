test_that("level 0 is a single tile covering the world", {
  info <- web_mercator_info()
  grid <- tile_grid(info, c(-20037508.342787, -20037508.342787, 20037508.342787, 20037508.342787), 0L)

  expect_equal(nrow(grid), 1L)
  expect_equal(grid[["row"]], 0L)
  expect_equal(grid[["col"]], 0L)
})

test_that("tile counts quadruple with each level", {
  info <- web_mercator_info()
  world <- service_extent()

  expect_equal(nrow(tile_grid(info, world, 1L)), 4L)
  expect_equal(nrow(tile_grid(info, world, 2L)), 16L)
  expect_equal(nrow(tile_grid(info, world, 3L)), 64L)
})

test_that("a tile's own extent round trips to exactly that tile", {
  info <- web_mercator_info()
  extent <- tile_extent(info, 3L, 2L, 5L)

  bbox <- sf::st_bbox(
    c(
      xmin = extent[["xmin"]],
      ymin = extent[["ymin"]],
      xmax = extent[["xmax"]],
      ymax = extent[["ymax"]]
    ),
    crs = sf::st_crs(3857)
  )

  grid <- tile_grid(info, bbox, 3L)

  expect_equal(nrow(grid), 1L)
  expect_equal(grid[["row"]], 2L)
  expect_equal(grid[["col"]], 5L)
})

test_that("tile extents tile the plane without gaps", {
  info <- web_mercator_info()

  left <- tile_extent(info, 4L, 3L, 3L)
  right <- tile_extent(info, 4L, 3L, 4L)
  below <- tile_extent(info, 4L, 4L, 3L)

  expect_equal(left[["xmax"]], right[["xmin"]])
  expect_equal(left[["ymin"]], below[["ymax"]])
})

test_that("tile_extent recycles its coordinates", {
  info <- web_mercator_info()
  extents <- tile_extent(info, 2L, 0:3, 1L)

  expect_equal(nrow(extents), 4L)
  expect_equal(extents[["col"]], rep(1L, 4L))
  expect_true(all(diff(extents[["ymax"]]) < 0))
})

test_that("a bbox in another crs is reprojected before gridding", {
  info <- web_mercator_info()
  bbox <- sf::st_bbox(c(xmin = -1, ymin = -1, xmax = 1, ymax = 1), crs = sf::st_crs(4326))

  grid <- tile_grid(info, bbox, 2L)

  expect_true(nrow(grid) >= 1L)
  expect_true(all(grid[["level"]] == 2L))
})

test_that("lods reports tile spans in map units", {
  info <- web_mercator_info()
  detail <- lods(info)

  expect_true(all(c("level", "resolution", "scale", "tile_width", "tile_height") %in% names(detail)))
  expect_equal(detail[["tile_width"]], detail[["resolution"]] * info@cols)
})

test_that("level selection follows resolution", {
  info <- web_mercator_info(0:10)

  expect_equal(level_for_resolution(info, 156543.03392800014), 0L)
  expect_equal(level_for_resolution(info, 156543.03392800014 / 8), 3L)
  expect_true(level_for_size(info, service_extent(), c(256L, 256L)) < 3L)
})

test_that("an unknown level is an error", {
  expect_error(tile_grid(web_mercator_info(0:3), service_extent(), 9L), "tiling scheme")
})

test_that("a resolution of zero is an error", {
  expect_error(level_for_resolution(web_mercator_info(), 0), "greater than 0")
})

test_that("TileInfo rejects a malformed tiling scheme", {
  expect_error(
    TileInfo(
      rows = 256L,
      cols = 256L,
      dpi = 96L,
      format = "PNG",
      origin = 0,
      crs = sf::st_crs(3857),
      lods = data.frame(level = 0L, resolution = 1, scale = 1)
    ),
    "length two"
  )

  expect_error(
    TileInfo(
      rows = 256L,
      cols = 256L,
      dpi = 96L,
      format = "PNG",
      origin = c(0, 0),
      crs = sf::st_crs(3857),
      lods = data.frame(level = 0L)
    ),
    "resolution"
  )
})
