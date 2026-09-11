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

  bbox <- as_tile_bbox(bbox, crs, call = error_call)

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

  sf::st_sf(
    pages[, c("page", "row", "col")],
    geometry = extents_as_sfc(pages, sf::st_crs(bbox))
  )
}

extents_as_sfc <- function(pages, crs) {
  boxes <- lapply(seq_len(nrow(pages)), function(i) {
    sf::st_as_sfc(sf::st_bbox(c(
      xmin = pages[["xmin"]][i],
      ymin = pages[["ymin"]][i],
      xmax = pages[["xmax"]][i],
      ymax = pages[["ymax"]][i]
    )))[[1L]]
  })

  sf::st_sfc(boxes, crs = crs)
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

  extents <- as_page_extents(pages, margin, error_call)
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
      size = collapse_num(size),
      format = format,
      transparent = tolower(transparent),
      dpi = as.integer(dpi),
      imageSR = crs_wkid(crs, required = TRUE, call = error_call),
      layers = layer_query(layers, visibility, error_call),
      layerDefs = layer_defs_query(layer_defs, error_call),
      f = "json"
    ),
    list(...)
  ))

  reqs <- lapply(extents, function(bbox) {
    arcgisutils::arc_base_req(
      paste0(x@url, "/export"),
      x@token,
      query = c(bbox_query(bbox, x@crs, error_call), shared),
      error_call = error_call
    )
  })

  responses <- httr2::req_perform_parallel(
    reqs,
    on_error = "continue",
    progress = progress
  )

  results <- lapply(responses, parse_export_response)
  paths <- file.path(dir, paste0(page_names, ".", image_file_ext(format)))

  hrefs <- vapply(results, function(r) r[["href"]] %||% NA_character_, character(1))
  ok <- download_all(hrefs[!is.na(hrefs)], paths[!is.na(hrefs)], x@token, progress, error_call)

  downloaded <- !is.na(hrefs)
  downloaded[downloaded] <- ok

  series_table(page_names, paths, downloaded, results)
}

parse_export_response <- function(resp) {
  if (inherits(resp, "error") || httr2::resp_status(resp) >= 300L) {
    return(list())
  }

  res <- RcppSimdJson::fparse(httr2::resp_body_string(resp))

  if (!is.null(res[["error"]])) {
    return(list())
  }

  res
}

series_table <- function(page_names, paths, ok, results) {
  extents <- lapply(results, function(r) as_extent_vec(r[["extent"]]))

  data.frame(
    page = seq_along(page_names),
    name = page_names,
    path = ifelse(ok, paths, NA_character_),
    ok = ok,
    xmin = vapply(extents, `[`, double(1), 1L),
    ymin = vapply(extents, `[`, double(1), 2L),
    xmax = vapply(extents, `[`, double(1), 3L),
    ymax = vapply(extents, `[`, double(1), 4L),
    scale = vapply(
      results,
      function(r) as.double(r[["scale"]] %||% NA_real_),
      double(1)
    )
  )
}

as_page_extents <- function(pages, margin = 0, call = rlang::caller_env()) {
  boxes <- page_bboxes(pages, call)

  if (margin == 0) {
    return(boxes)
  }

  lapply(boxes, expand_bbox, margin = margin)
}

page_bboxes <- function(pages, call = rlang::caller_env()) {
  if (inherits(pages, "bbox")) {
    return(list(pages))
  }

  if (inherits(pages, c("sf", "sfc"))) {
    geometry <- sf::st_geometry(pages)
    crs <- sf::st_crs(geometry)

    return(lapply(seq_along(geometry), function(i) {
      sf::st_bbox(geometry[i], crs = crs)
    }))
  }

  if (is.data.frame(pages)) {
    missing <- setdiff(c("xmin", "ymin", "xmax", "ymax"), base::names(pages))

    if (length(missing) > 0L) {
      cli::cli_abort(
        "{.arg pages} is missing {.field {missing}}.",
        call = call
      )
    }

    return(lapply(seq_len(nrow(pages)), function(i) {
      sf::st_bbox(c(
        xmin = pages[["xmin"]][i],
        ymin = pages[["ymin"]][i],
        xmax = pages[["xmax"]][i],
        ymax = pages[["ymax"]][i]
      ))
    }))
  }

  if (is.list(pages)) {
    return(lapply(pages, as_tile_bbox, call = call))
  }

  cli::cli_abort(
    "{.arg pages} must be an {.cls sf}, a data frame of extents, or a list of {.cls bbox}.",
    call = call
  )
}

expand_bbox <- function(bbox, margin) {
  width <- (bbox[["xmax"]] - bbox[["xmin"]]) * margin
  height <- (bbox[["ymax"]] - bbox[["ymin"]]) * margin

  bbox[["xmin"]] <- bbox[["xmin"]] - width
  bbox[["xmax"]] <- bbox[["xmax"]] + width
  bbox[["ymin"]] <- bbox[["ymin"]] - height
  bbox[["ymax"]] <- bbox[["ymax"]] + height

  bbox
}
