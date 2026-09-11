test_that("the style catalogue lists both families", {
  skip_if_no_network()

  styles <- basemap_styles()

  expect_s3_class(styles, "data.frame")
  expect_true(nrow(styles) > 0L)
  expect_true(all(c("path", "name", "styleFamily") %in% names(styles)))
  expect_true(all(c("arcgis", "open") %in% styles[["styleFamily"]]))
})

test_that("the catalogue filters by family", {
  skip_if_no_network()

  arcgis <- basemap_styles("arcgis")

  expect_true(nrow(arcgis) > 0L)
  expect_true(all(arcgis[["styleFamily"]] == "arcgis"))
  expect_error(basemap_styles("esri"))
})

test_that("the catalogue carries the codes basemap_style accepts", {
  skip_if_no_network()

  styles <- basemap_styles()
  languages <- attr(styles, "languages")
  worldviews <- attr(styles, "worldviews")

  expect_true(all(c("code", "name") %in% names(languages)))
  expect_true(all(c("code", "name") %in% names(worldviews)))
  expect_true("global" %in% languages[["code"]])
})

