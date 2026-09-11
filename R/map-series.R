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
#' @returns A data frame with columns `page`, `row`, `col`, and `extent`, a
#'   [wk::rct()] vector of page extents.
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

  bbox <- as_bbox(bbox, crs, error_call = error_call)

  width <- (rct_xmax(bbox) - rct_xmin(bbox)) / ncol
  height <- (rct_ymax(bbox) - rct_ymin(bbox)) / nrow

  grid <- expand.grid(col = seq_len(ncol), row = seq_len(nrow))

  xmin <- rct_xmin(bbox) + (grid[["col"]] - 1L) * width
  ymax <- rct_ymax(bbox) - (grid[["row"]] - 1L) * height

  data_frame(data.frame(
    page = seq_len(nrow(grid)),
    row = grid[["row"]],
    col = grid[["col"]],
    extent = rct(
      xmin - width * overlap,
      ymax - height - height * overlap,
      xmin + width + width * overlap,
      ymax + height * overlap,
      crs = wk_crs(bbox)
    )
  ))
}

#' Export a series of map images
#'
#' Exports one image per page extent, the equivalent of a map series or
#' data driven pages. Requests run in parallel.
#'
#' @inheritParams export_map
#' @param pages A [wk::rct()], a data frame with an `extent` column or with
#'   `xmin`, `ymin`, `xmax`, and `ymax` columns, or any geometry
#'   [wk::wk_envelope()] understands. One image is exported per page.
#' @param dir String. Directory to write images into. Created if needed.
#' @param page_names Character. File name stems, one per page. Defaults to
#'   `page-001`, `page-002`, and so on.
#' @inheritParams get_tiles
#' @param margin Double. Fraction to expand each page extent by before
#'   exporting.
#' @returns A data frame with one row per page and columns `page`, `name`,
#'   `path`, `ok`, `extent`, and `scale`. `extent` is what the server drew.
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

  extents <- if (inherits(pages, "wk_rct")) {
    pages
  } else if (is.data.frame(pages) && "extent" %in% base::names(pages)) {
    pages[["extent"]]
  } else if (is.data.frame(pages) && !inherits(pages, "sf")) {
    missing <- setdiff(c("xmin", "ymin", "xmax", "ymax"), base::names(pages))

    if (length(missing) > 0L) {
      cli::cli_abort(
        "{.arg pages} is missing {.field {missing}}.",
        call = error_call
      )
    }

    rct(
      pages[["xmin"]],
      pages[["ymin"]],
      pages[["xmax"]],
      pages[["ymax"]]
    )
  } else {
    rlang::try_fetch(
      wk::wk_envelope(pages),
      error = function(cnd) {
        cli::cli_abort(
          c(
            "{.arg pages} must be a {.cls wk_rct}, a data frame of extents, or a geometry.",
            "i" = "{.cls {class(pages)[1]}} has no per feature extent."
          ),
          call = error_call
        )
      }
    )
  }

  if (margin > 0) {
    width <- (rct_xmax(extents) - rct_xmin(extents)) * margin
    height <- (rct_ymax(extents) - rct_ymin(extents)) * margin

    extents <- rct(
      rct_xmin(extents) - width,
      rct_ymin(extents) - height,
      rct_xmax(extents) + width,
      rct_ymax(extents) + height,
      crs = wk_crs(extents)
    )
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

  # a wk_rct is a record vector, so iterate positions rather than its fields
  reqs <- lapply(seq_along(extents), function(i) {
    bbox <- extents[i]

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

  drawn <- lapply(results, function(r) {
    if (is.null(r[["extent"]])) {
      return(rep(NA_real_, 4L))
    }

    unname(as.double(arcgisutils::from_envelope(r[["extent"]])))
  })

  data_frame(data.frame(
    page = seq_along(page_names),
    name = page_names,
    path = ifelse(downloaded, paths, NA_character_),
    ok = downloaded,
    extent = rct(
      vapply(drawn, `[`, double(1), 1L),
      vapply(drawn, `[`, double(1), 2L),
      vapply(drawn, `[`, double(1), 3L),
      vapply(drawn, `[`, double(1), 4L),
      crs = wk_crs(extents)
    ),
    scale = vapply(
      results,
      function(r) as.double(r[["scale"]] %||% NA_real_),
      double(1)
    )
  ))
}
