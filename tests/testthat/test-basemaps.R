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

test_that("languages and worldviews come back as lookup tables", {
  skip_if_no_network()

  languages <- basemap_languages()
  worldviews <- basemap_worldviews()

  expect_true(all(c("code", "name") %in% names(languages)))
  expect_true(all(c("code", "name") %in% names(worldviews)))
  expect_true("global" %in% languages[["code"]])
})

test_that("a style path is built from its parts", {
  expect_equal(arcgistiles:::style_path("navigation", "arcgis"), "arcgis/navigation")
  expect_equal(arcgistiles:::style_path("arcgis/navigation", "open"), "arcgis/navigation")
  expect_error(arcgistiles:::style_path("", "arcgis"))
})

test_that("style query parameters are validated", {
  expect_equal(
    arcgistiles:::style_query("es", NULL, "all"),
    list(language = "es", places = "all", f = "json")
  )

  expect_equal(arcgistiles:::style_query(NULL, NULL, NULL), list(f = "json"))
  expect_error(arcgistiles:::style_query(NULL, NULL, "some"))
})

test_that("the service url is configurable", {
  withr::local_options(arcgistiles.basemap_url = "https://example.com/v2")

  expect_equal(basemap_url(), "https://example.com/v2")
})

test_that("a style names its vector tile source", {
  expect_equal(
    arcgistiles:::style_source_url(
      list(sources = list(esri = list(url = "https://example.com/VectorTileServer/")))
    ),
    "https://example.com/VectorTileServer"
  )

  expect_error(
    arcgistiles:::style_source_url(list(sources = list(esri = list(type = "vector")))),
    "no vector tile service"
  )
})
