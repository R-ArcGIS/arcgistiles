test_that("a grid has one page per cell and tiles the extent", {
  pages <- map_grid(c(0, 0, 10, 10), nrow = 2L, ncol = 5L, crs = 3857)

  expect_s3_class(pages, "sf")
  expect_equal(nrow(pages), 10L)
  expect_equal(pages[["page"]], 1:10)
  expect_equal(unname(as.double(sf::st_bbox(pages))), c(0, 0, 10, 10))
})

test_that("overlap grows each page beyond its cell", {
  plain <- map_grid(c(0, 0, 10, 10), nrow = 2L, ncol = 2L, crs = 3857)
  padded <- map_grid(c(0, 0, 10, 10), nrow = 2L, ncol = 2L, overlap = 0.1, crs = 3857)

  expect_true(all(sf::st_area(padded) > sf::st_area(plain)))
})

test_that("pages read in reading order", {
  pages <- map_grid(c(0, 0, 10, 10), nrow = 2L, ncol = 2L, crs = 3857)

  expect_equal(pages[["row"]], c(1L, 1L, 2L, 2L))
  expect_equal(pages[["col"]], c(1L, 2L, 1L, 2L))

  centroids <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(pages)))
  expect_true(centroids[1L, "Y"] > centroids[3L, "Y"])
  expect_true(centroids[1L, "X"] < centroids[2L, "X"])
})


test_that("pages come from a data frame of extents as well as an sf", {
  skip_if_no_network()

  service <- map_server(census_url())
  extents <- data.frame(
    xmin = c(-104, -100),
    ymin = 35.6,
    xmax = c(-100, -96),
    ymax = 41
  )

  atlas <- map_series(service, extents, size = c(200L, 200L), progress = FALSE)

  expect_equal(nrow(atlas), 2L)
  expect_true(all(atlas[["ok"]]))
})

test_that("a data frame without extent columns is an error", {
  skip_if_no_network()

  service <- map_server(census_url())

  expect_error(
    map_series(service, data.frame(a = 1), progress = FALSE),
    "missing"
  )
})

test_that("margin widens every page", {
  skip_if_no_network()

  service <- map_server(census_url())
  pages <- map_grid(c(-104, 35.6, -94.32, 41), nrow = 1L, ncol = 2L, crs = 4326)

  plain <- map_series(service, pages, size = c(200L, 200L), progress = FALSE)
  padded <- map_series(
    service,
    pages,
    size = c(200L, 200L),
    margin = 0.2,
    progress = FALSE
  )

  expect_true(all(padded[["scale"]] > plain[["scale"]]))
})
