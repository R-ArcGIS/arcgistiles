test_that("a length four numeric becomes a bbox", {
  bbox <- arcgistiles:::as_bbox(c(0, 1, 2, 3))

  expect_s3_class(bbox, "bbox")
  expect_equal(unname(as.double(bbox)), c(0, 1, 2, 3))
})

test_that("a bbox with no crs takes the one it is given", {
  bbox <- arcgistiles:::as_bbox(c(0, 1, 2, 3), sf::st_crs(3857))

  expect_equal(sf::st_crs(bbox), sf::st_crs(3857))
  expect_equal(unname(as.double(bbox)), c(0, 1, 2, 3))
})

test_that("a bbox in another crs is transformed", {
  bbox <- sf::st_bbox(c(xmin = -1, ymin = -1, xmax = 1, ymax = 1), crs = sf::st_crs(4326))
  out <- arcgistiles:::as_bbox(bbox, sf::st_crs(3857))

  expect_equal(sf::st_crs(out), sf::st_crs(3857))
  expect_true(abs(out[["xmin"]]) > 100000)
})

test_that("an sf object supplies its own bounding box", {
  point <- sf::st_sfc(sf::st_point(c(0, 0)), crs = sf::st_crs(4326))

  expect_s3_class(arcgistiles:::as_bbox(point), "bbox")
})

test_that("a malformed bbox is an error", {
  expect_error(arcgistiles:::as_bbox("nope"), "bbox")
  expect_error(arcgistiles:::as_bbox(c(1, 2, 3)), "bbox")
})

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
