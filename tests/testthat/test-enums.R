test_that("enums accept their variants and reject everything else", {
  expect_equal(as.character(image_format("png32")), "png32")
  expect_equal(as.character(layer_visibility("hide")), "hide")
  expect_equal(as.character(export_by("Scale")), "Scale")
  expect_equal(as.character(storage_format("Compact")), "Compact")
  expect_equal(as.character(style_family("open")), "open")
  expect_equal(as.character(places("attributed")), "attributed")

  expect_error(image_format("tiff"))
  expect_error(layer_visibility("maybe"))
  expect_error(style_family("esri"))
})

test_that("enums carry sensible defaults", {
  expect_equal(as.character(image_format()), "png")
  expect_equal(as.character(layer_visibility()), "show")
  expect_equal(as.character(export_by()), "LevelID")
  expect_equal(as.character(storage_format()), "CompactV2")
})

test_that("layer visibility builds the layers parameter", {
  expect_equal(arcgistiles:::layer_query(c(2, 4, 7), "show"), "show:2, 4, 7")
  expect_equal(arcgistiles:::layer_query(0, "hide"), "hide:0")
  expect_null(arcgistiles:::layer_query(NULL, "show"))

  expect_error(arcgistiles:::layer_query("two", "show"), "layer ids")
})

test_that("layer definitions are sent as json", {
  expect_equal(
    arcgistiles:::layer_defs_query(list(`0` = "POP > 100")),
    "{\"0\":\"POP > 100\"}"
  )

  expect_null(arcgistiles:::layer_defs_query(NULL))
  expect_error(arcgistiles:::layer_defs_query(list("POP > 100")), "named")
})

test_that("levels collapse to the service's syntax", {
  expect_equal(arcgistiles:::collapse_levels(0:3), "0, 1, 2, 3")
  expect_equal(arcgistiles:::collapse_levels("1-4,7-9"), "1-4,7-9")
  expect_error(arcgistiles:::collapse_levels(list(1)), "numeric vector")
})
