test_that("size must be two positive numbers", {
  expect_equal(arcgistiles:::check_size(c(10, 20)), c(10L, 20L))
  expect_equal(arcgistiles:::check_size(c(10.4, 20.6)), c(10L, 21L))

  expect_error(arcgistiles:::check_size(10), "length two")
  expect_error(arcgistiles:::check_size(c(0, 10)), "positive")
  expect_error(arcgistiles:::check_size(c(NA, 10)), "length two")
})

test_that("a service extent resolves to an epsg code, not an esri wkid", {
  meta <- list(
    spatialReference = list(wkid = 102100L, latestWkid = 3857L),
    fullExtent = list(
      xmin = -1,
      ymin = -1,
      xmax = 1,
      ymax = 1,
      spatialReference = list(cs = "pcs", wkid = 102100L)
    )
  )

  expect_equal(sf::st_crs(arcgistiles:::service_extent(meta))$epsg, 3857L)
})

test_that("a spatial reference reaches a query as a bare wkid", {
  bbox <- sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 1, ymax = 1), crs = sf::st_crs(3857))
  query <- arcgistiles:::bbox_query(bbox)

  expect_equal(query[["bboxSR"]], 3857L)
  expect_equal(query[["bbox"]], "0, 0, 1, 1")

  expect_null(arcgistiles:::bbox_query(c(0, 0, 1, 1))[["bboxSR"]])
})

test_that("capabilities split on commas", {
  expect_equal(arcgistiles:::split_capabilities("Map,Query,Data"), c("Map", "Query", "Data"))
  expect_equal(arcgistiles:::split_capabilities(NULL), character())
})
