test_that("a grid has one page per cell and covers the extent", {
  pages <- map_grid(c(0, 0, 10, 10), nrow = 2L, ncol = 5L, crs = 3857)

  expect_s3_class(pages[["extent"]], "wk_rct")
  expect_equal(nrow(pages), 10L)
  expect_equal(pages[["page"]], 1:10)
  expect_equal(
    unname(unlist(unclass(wk::wk_bbox(pages[["extent"]])))),
    c(0, 0, 10, 10)
  )
  expect_true(wk::wk_crs_equal(wk::wk_crs(pages[["extent"]]), 3857))
})

test_that("overlap grows each page beyond its cell", {
  plain <- map_grid(c(0, 0, 10, 10), nrow = 2L, ncol = 2L, crs = 3857)[["extent"]]
  padded <- map_grid(
    c(0, 0, 10, 10),
    nrow = 2L,
    ncol = 2L,
    overlap = 0.1,
    crs = 3857
  )[["extent"]]

  expect_true(all(wk::rct_width(padded) > wk::rct_width(plain)))
  expect_true(all(wk::rct_height(padded) > wk::rct_height(plain)))
})

test_that("pages read in reading order", {
  pages <- map_grid(c(0, 0, 10, 10), nrow = 2L, ncol = 2L, crs = 3857)
  extent <- pages[["extent"]]

  expect_equal(pages[["row"]], c(1L, 1L, 2L, 2L))
  expect_equal(pages[["col"]], c(1L, 2L, 1L, 2L))

  expect_true(wk::rct_ymin(extent)[1L] > wk::rct_ymin(extent)[3L])
  expect_true(wk::rct_xmin(extent)[1L] < wk::rct_xmin(extent)[2L])
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
