test_that("an exported image reports the extent the server drew", {
  skip_if_no_network()

  service <- map_server(census_url())
  image <- export_map(service, c(-104, 35.6, -94.32, 41), size = c(600L, 400L))

  expect_s3_class(image, "arcgistiles::MapImage")
  expect_equal(image@size, c(600L, 400L))
  expect_true(file.exists(image@path))
  expect_true(file.size(image@path) > 0)

  expect_equal(unname(image@bbox[["xmin"]]), -104)
  expect_equal(unname(image@bbox[["xmax"]]), -94.32)
  expect_true(image@bbox[["ymin"]] < 35.6)
  expect_true(image@bbox[["ymax"]] > 41)
})

test_that("an exported image becomes a georeferenced raster", {
  skip_if_no_network()
  skip_if_not_installed("terra")

  service <- map_server(census_url())
  image <- export_map(service, c(-104, 35.6, -94.32, 41), size = c(300L, 200L))
  raster <- as_rast(image)

  expect_s4_class(raster, "SpatRaster")
  expect_equal(terra::ncol(raster), 300L)
  expect_equal(terra::nrow(raster), 200L)
  expect_equal(unname(as.vector(terra::ext(raster))[["xmin"]]), unname(image@bbox[["xmin"]]))
})

test_that("layers and definition expressions reach the service", {
  skip_if_no_network()

  service <- map_server(census_url())
  image <- export_map(
    service,
    c(-104, 35.6, -94.32, 41),
    size = c(200L, 200L),
    layers = 3,
    visibility = "show",
    layer_defs = list(`3` = "POP2000 > 1000000")
  )

  expect_true(file.exists(image@path))
})

test_that("an unsupported format is rejected before any request", {
  skip_if_no_network()

  service <- map_server(census_url())

  expect_error(export_map(service, c(0, 0, 1, 1), format = "tiff"))
  expect_error(export_map(service, c(0, 0, 1, 1), size = c(100L)), "length two")
})
