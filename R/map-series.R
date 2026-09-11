#' @include export-map.R
NULL

#' Build a grid of map pages
#'
#' Splits `bbox` into a regular grid of page extents, the starting point for a
#' map series.
#'
#' @inheritParams tile_grid
#' @param nrow,ncol Integer. Number of page rows and columns.
#' @param overlap Double. Fraction of a page to expand each extent by, so
#'   adjacent pages share a margin. `0.05` adds 5 percent on every side.
#' @param crs Coordinate reference system of the pages. Defaults to the CRS of
#'   `bbox`.
#' @returns An `sf` data frame with columns `page`, `row`, `col`, and a
#'   rectangle geometry per page.
#' @family map series
#' @export
#' @examples
#' map_grid(c(-104, 35.6, -94.32, 41), nrow = 2, ncol = 3)
map_grid <- function(
  bbox,
  nrow = 2L,
  ncol = 2L,
  overlap = 0,
  crs = NULL,
  error_call = rlang::caller_env()
) {
  check_number_whole(nrow, min = 1, call = error_call)
  check_number_whole(ncol, min = 1, call = error_call)
  check_number_decimal(overlap, min = 0, call = error_call)

  bbox <- as_bbox(bbox, crs, call = error_call)

  width <- (bbox[["xmax"]] - bbox[["xmin"]]) / ncol
  height <- (bbox[["ymax"]] - bbox[["ymin"]]) / nrow

  grid <- expand.grid(col = seq_len(ncol), row = seq_len(nrow))

  xmin <- bbox[["xmin"]] + (grid[["col"]] - 1L) * width
  ymax <- bbox[["ymax"]] - (grid[["row"]] - 1L) * height

  pages <- data.frame(
    page = seq_len(nrow(grid)),
    row = grid[["row"]],
    col = grid[["col"]],
    xmin = xmin - width * overlap,
    ymin = ymax - height - height * overlap,
    xmax = xmin + width + width * overlap,
    ymax = ymax + height * overlap
  )

  boxes <- lapply(seq_len(nrow(pages)), function(i) {
    sf::st_as_sfc(sf::st_bbox(c(
      xmin = pages[["xmin"]][i],
      ymin = pages[["ymin"]][i],
      xmax = pages[["xmax"]][i],
      ymax = pages[["ymax"]][i]
    )))[[1L]]
  })

  sf::st_sf(
    pages[, c("page", "row", "col")],
    geometry = sf::st_sfc(boxes, crs = sf::st_crs(bbox))
  )
}

#' Export a series of map images
#'
#' Exports one image per page extent, the equivalent of a map series or
#' data driven pages. Requests run in parallel.
#'
#' @inheritParams export_map
#' @param pages An `sf` object, an `sfc`, a list of `bbox` objects, or a data
#'   frame with `xmin`, `ymin`, `xmax`, and `ymax` columns. One image is
#'   exported per row or feature.
#' @param dir String. Directory to write images into. Created if needed.
#' @param page_names Character. File name stems, one per page. Defaults to
#'   `page-001`, `page-002`, and so on.
#' @inheritParams get_tiles
#' @param margin Double. Fraction to expand each page extent by before
#'   exporting.
#' @returns A data frame with one row per page and columns `page`, `name`,
#'   `path`, `ok`, `xmin`, `ymin`, `xmax`, `ymax`, and `scale`. The extents are
#'   the ones the server rendered.
#' @family map series
#' @export
#' @examples
#' \dontrun{
#' ms <- map_server(census_url())
#' pages <- map_grid(c(-104, 35.6, -94.32, 41), nrow = 2, ncol = 2)
#' map_series(ms, pages, size = c(800, 600))
#' }
map_series <- function(
  x,
  pages,
  size = c(800L, 600L),
  format = "png",
  dir = tempfile("map-series"),
  page_names = NULL,
  margin = 0,
  crs = NULL,
  layers = NULL,
  visibility = "show",
  layer_defs = NULL,
  dpi = 96L,
  transparent = FALSE,
  progress = TRUE,
  ...,
  error_call = rlang::caller_env()
) {
  check_bool(transparent, call = error_call)
  check_number_whole(dpi, min = 1, call = error_call)
  check_number_decimal(margin, min = 0, call = error_call)
  check_character(page_names, allow_null = TRUE, call = error_call)

  format <- as.character(image_format(as.character(format)))
  size <- check_size(size, call = error_call)

  extents <- if (inherits(pages, "bbox")) {
    list(pages)
  } else if (inherits(pages, c("sf", "sfc"))) {
    geometry <- sf::st_geometry(pages)
    page_crs <- sf::st_crs(geometry)

    lapply(seq_along(geometry), function(i) {
      sf::st_bbox(geometry[i], crs = page_crs)
    })
  } else if (is.data.frame(pages)) {
    missing <- setdiff(c("xmin", "ymin", "xmax", "ymax"), base::names(pages))

    if (length(missing) > 0L) {
      cli::cli_abort(
        "{.arg pages} is missing {.field {missing}}.",
        call = error_call
      )
    }

    lapply(seq_len(nrow(pages)), function(i) {
      sf::st_bbox(c(
        xmin = pages[["xmin"]][i],
        ymin = pages[["ymin"]][i],
        xmax = pages[["xmax"]][i],
        ymax = pages[["ymax"]][i]
      ))
    })
  } else if (is.list(pages)) {
    lapply(pages, as_bbox, call = error_call)
  } else {
    cli::cli_abort(
      "{.arg pages} must be an {.cls sf}, a data frame of extents, or a list of {.cls bbox}.",
      call = error_call
    )
  }

  if (margin > 0) {
    extents <- lapply(extents, function(bbox) {
      width <- (bbox[["xmax"]] - bbox[["xmin"]]) * margin
      height <- (bbox[["ymax"]] - bbox[["ymin"]]) * margin

      bbox[["xmin"]] <- bbox[["xmin"]] - width
      bbox[["xmax"]] <- bbox[["xmax"]] + width
      bbox[["ymin"]] <- bbox[["ymin"]] - height
      bbox[["ymax"]] <- bbox[["ymax"]] + height

      bbox
    })
  }

  n <- length(extents)

  page_names <- page_names %||% sprintf("page-%03d", seq_len(n))

  if (length(page_names) != n) {
    cli::cli_abort(
      "{.arg page_names} must be length {n}, not {length(page_names)}.",
      call = error_call
    )
  }

  shared <- compact(c(
    list(
      size = toString(size),
      format = format,
      transparent = tolower(transparent),
      dpi = as.integer(dpi),
      imageSR = arcgisutils::validate_crs(
        crs,
        call = error_call
      )[["spatialReference"]][["wkid"]],
      layers = layer_query(layers, visibility, error_call),
      layerDefs = layer_defs_query(layer_defs, error_call),
      f = "json"
    ),
    list(...)
  ))

  reqs <- lapply(extents, function(bbox) {
    arcgisutils::arc_base_req(
      x@url,
      x@token,
      path = "export",
      query = c(bbox_query(bbox, error_call), shared),
      error_call = error_call
    )
  })

  responses <- httr2::req_perform_parallel(
    reqs,
    on_error = "continue",
    progress = progress
  )

  # a page that failed leaves no href, and the rest of the series still stands
  results <- lapply(responses, function(resp) {
    if (inherits(resp, "error") || httr2::resp_status(resp) >= 300L) {
      return(list())
    }

    res <- RcppSimdJson::fparse(httr2::resp_body_string(resp))

    if (is.null(res[["error"]])) res else list()
  })

  paths <- file.path(dir, paste0(page_names, ".", image_file_ext(format)))

  hrefs <- vapply(results, function(r) r[["href"]] %||% NA_character_, character(1))
  ok <- download_all(hrefs[!is.na(hrefs)], paths[!is.na(hrefs)], x@token, progress, error_call)

  downloaded <- !is.na(hrefs)
  downloaded[downloaded] <- ok

  extents <- lapply(results, function(r) {
    if (is.null(r[["extent"]])) {
      return(rep(NA_real_, 4L))
    }

    as.double(arcgisutils::from_envelope(r[["extent"]]))
  })

  data_frame(data.frame(
    page = seq_along(page_names),
    name = page_names,
    path = ifelse(downloaded, paths, NA_character_),
    ok = downloaded,
    xmin = vapply(extents, `[`, double(1), 1L),
    ymin = vapply(extents, `[`, double(1), 2L),
    xmax = vapply(extents, `[`, double(1), 3L),
    ymax = vapply(extents, `[`, double(1), 4L),
    scale = vapply(
      results,
      function(r) as.double(r[["scale"]] %||% NA_real_),
      double(1)
    )
  ))
}
