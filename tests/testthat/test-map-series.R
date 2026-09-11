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

test_that("page extents come from every supported input", {
  pages <- map_grid(c(0, 0, 10, 10), nrow = 1L, ncol = 2L, crs = 3857)

  from_sf <- arcgistiles:::page_bboxes(pages)
  from_df <- arcgistiles:::page_bboxes(
    data.frame(xmin = c(0, 5), ymin = 0, xmax = c(5, 10), ymax = 10)
  )
  from_list <- arcgistiles:::page_bboxes(list(c(0, 0, 5, 10), c(5, 0, 10, 10)))

  expect_length(from_sf, 2L)
  expect_length(from_df, 2L)
  expect_length(from_list, 2L)
  expect_s3_class(from_sf[[1L]], "bbox")
})

test_that("a data frame without extent columns is an error", {
  expect_error(arcgistiles:::page_bboxes(data.frame(a = 1)), "missing")
})

test_that("margin expands a page symmetrically", {
  bbox <- sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 10, ymax = 10), crs = sf::st_crs(3857))
  wider <- arcgistiles:::expand_bbox(bbox, 0.1)

  expect_equal(unname(as.double(wider)), c(-1, -1, 11, 11))
})
