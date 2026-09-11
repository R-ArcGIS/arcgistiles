#' @include rasterize.R
NULL

#' Exported map image
#'
#' An image written by [export_map()]. `bbox` is the extent the server actually
#' rendered, which is expanded from the requested extent to match the aspect
#' ratio of `size`.
#'
#' @param path String. Path to the image on disk.
#' @param bbox Numeric. Length four `c(xmin, ymin, xmax, ymax)` of the image.
#' @param crs Coordinate reference system of the image.
#' @param size Integer. Length two `c(width, height)` in pixels.
#' @param format String. Image format.
#' @param scale Double. Map scale the image was rendered at.
#' @returns A `MapImage` object.
#' @family map images
#' @export
MapImage <- S7::new_class(
  "MapImage",
  package = "arcgistiles",
  properties = list(
    path = s7x::class_string,
    bbox = S7::class_double,
    crs = class_crs,
    size = S7::class_integer,
    format = s7x::class_string,
    scale = s7x::class_float
  )
)

S7::method(print, MapImage) <- function(x, ...) {
  cli::cli_text("{.cls MapImage} {x@size[1]}x{x@size[2]} {x@format}")
  cli::cli_text("{.strong CRS:} {crs_label(x@crs)}")
  cli::cli_text("{.strong Extent:} {.val {x@bbox}}")
  cli::cli_text("{.file {x@path}}")

  invisible(x)
}

S7::method(as_rast, MapImage) <- function(x, ...) {
  check_terra()

  name_bands(georeference(
    x@path,
    c(xmin = x@bbox[1L], xmax = x@bbox[3L], ymin = x@bbox[2L], ymax = x@bbox[4L]),
    x@crs
  ))
}

#' Bounding box of a map image
#'
#' @param x A [MapImage].
#' @returns A `bbox`.
#' @family map images
#' @export
#' @examples
#' \dontrun{
#' image_bbox(export_map(ms, bbox))
#' }
image_bbox <- function(x) {
  sf::st_bbox(
    stats::setNames(x@bbox, c("xmin", "ymin", "xmax", "ymax")),
    crs = x@crs
  )
}

#' Export a map image
#'
#' Renders an extent of a map service to an image file. The server expands the
#' requested extent to the aspect ratio of `size`, so the returned
#' [MapImage] carries the extent that was actually drawn.
#'
#' @param x A [MapServer].
#' @inheritParams tile_grid
#' @param size Integer. Length two `c(width, height)` in pixels.
#' @param format String or [image_format()]. Output image format.
#' @param file String. Where to write the image. Defaults to a temporary file.
#' @param crs Coordinate reference system of the output image. Defaults to the
#'   service's own.
#' @param layers Integer. Layer ids to apply `visibility` to.
#' @param visibility String or [layer_visibility()]. How `layers` is applied.
#' @param layer_defs Named list. Definition expressions keyed by layer id.
#' @param dpi Integer. Device resolution of the image.
#' @param transparent Bool. Render the background transparent.
#' @param rotation Double. Degrees to rotate the map counterclockwise.
#' @param time Double. Length one or two epoch milliseconds.
#' @param ... Additional query parameters passed to the service.
#' @returns A [MapImage].
#' @family map images
#' @export
#' @examples
#' \dontrun{
#' ms <- map_server(census_url())
#' export_map(ms, c(-104, 35.6, -94.32, 41), size = c(600, 400))
#' }
export_map <- function(
  x,
  bbox,
  size = c(800L, 600L),
  format = "png",
  file = NULL,
  crs = NULL,
  layers = NULL,
  visibility = "show",
  layer_defs = NULL,
  dpi = 96L,
  transparent = FALSE,
  rotation = NULL,
  time = NULL,
  ...,
  error_call = rlang::caller_env()
) {
  check_bool(transparent, call = error_call)
  check_number_whole(dpi, min = 1, call = error_call)
  check_string(file, allow_null = TRUE, allow_empty = FALSE, call = error_call)

  format <- as.character(image_format(as.character(format)))
  size <- check_size(size, call = error_call)

  query <- compact(c(
    bbox_query(bbox, x@crs, error_call),
    list(
      size = collapse_num(size),
      format = format,
      transparent = tolower(transparent),
      dpi = as.integer(dpi),
      imageSR = crs_wkid(crs, required = TRUE, call = error_call),
      layers = layer_query(layers, visibility, error_call),
      layerDefs = layer_defs_query(layer_defs, error_call),
      rotation = if (!is.null(rotation)) as.double(rotation),
      time = if (!is.null(time)) collapse_num(time),
      f = "json"
    ),
    list(...)
  ))

  res <- arc_json(
    paste0(x@url, "/export"),
    token = x@token,
    query = query,
    call = error_call
  )

  file <- file %||% tempfile(fileext = paste0(".", image_file_ext(format)))
  download_href(res[["href"]], file, x@token, error_call)

  MapImage(
    path = file,
    bbox = as_extent_vec(res[["extent"]]),
    crs = as_crs(res[["extent"]][["spatialReference"]], call = error_call),
    size = c(as.integer(res[["width"]]), as.integer(res[["height"]])),
    format = format,
    scale = as.double(res[["scale"]] %||% NA_real_)
  )
}

# a bbox whose crs has no EPSG code cannot be named in a query, so send it in
# the service's own crs instead
bbox_query <- function(bbox, service_crs = NULL, call = rlang::caller_env()) {
  bbox <- as_tile_bbox(bbox, call = call)
  wkid <- crs_wkid(sf::st_crs(bbox), call = call)

  if (is.null(wkid) && !is.null(service_crs) && !is.na(sf::st_crs(bbox))) {
    bbox <- as_tile_bbox(bbox, service_crs, call = call)
    wkid <- crs_wkid(service_crs, call = call)
  }

  list(
    bbox = collapse_num(as.double(bbox)),
    bboxSR = wkid
  )
}

crs_wkid <- function(crs, required = FALSE, call = rlang::caller_env()) {
  if (is.null(crs)) {
    return(NULL)
  }

  crs <- as_crs(crs, call = call)

  if (is.na(crs)) {
    return(NULL)
  }

  epsg <- sf::st_crs(crs)$epsg

  if (is.null(epsg) || is.na(epsg)) {
    if (required) {
      cli::cli_abort(
        "{.arg crs} must have an EPSG code to send to the service.",
        call = call
      )
    }

    return(NULL)
  }

  as.integer(epsg)
}

layer_query <- function(layers, visibility, call = rlang::caller_env()) {
  if (is.null(layers)) {
    return(NULL)
  }

  if (!is.numeric(layers) || anyNA(layers)) {
    cli::cli_abort("{.arg layers} must be a numeric vector of layer ids.", call = call)
  }

  paste0(
    as.character(layer_visibility(as.character(visibility))),
    ":",
    collapse_num(as.integer(layers))
  )
}

layer_defs_query <- function(layer_defs, call = rlang::caller_env()) {
  if (is.null(layer_defs)) {
    return(NULL)
  }

  if (!rlang::is_named(layer_defs)) {
    cli::cli_abort(
      "{.arg layer_defs} must be named by layer id.",
      call = call
    )
  }

  yyjsonr::write_json_str(as.list(layer_defs), auto_unbox = TRUE)
}

download_href <- function(href, file, token, call = rlang::caller_env()) {
  if (is.null(href)) {
    cli::cli_abort(
      c(
        "The service did not return an image.",
        "i" = "It may not support the {.path /export} operation."
      ),
      call = call
    )
  }

  arcgisutils::arc_base_req(href, token, error_call = call) |>
    httr2::req_perform(path = file, error_call = call)

  invisible(file)
}

image_file_ext <- function(format) {
  switch(
    format,
    "png8" = "png",
    "png24" = "png",
    "png32" = "png",
    format
  )
}
