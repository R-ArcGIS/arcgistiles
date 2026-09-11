#' @include tile-info.R
NULL

#' ArcGIS service
#'
#' Abstract parent of [MapServer] and [VectorTileServer].
#'
#' @param url String. Service URL.
#' @param metadata List. The service's JSON metadata.
#' @param name String. Service name.
#' @param crs Coordinate reference system of the service.
#' @param full_extent Numeric. Length four `c(xmin, ymin, xmax, ymax)`.
#' @param tile_info A [TileInfo], or `NULL` when the service is not cached.
#' @param capabilities Character. Supported capabilities.
#' @param token An `httr2_token`, or `NULL`.
#' @returns A `Service` object.
#' @family services
#' @export
Service <- S7::new_class(
  "Service",
  package = "arcgistiles",
  abstract = TRUE,
  properties = list(
    url = s7x::class_string,
    metadata = S7::class_list,
    name = s7x::class_string,
    crs = class_crs,
    full_extent = S7::class_double,
    tile_info = S7::new_union(TileInfo, NULL),
    capabilities = S7::class_character,
    token = class_token
  )
)

S7::method(tile_info, Service) <- function(x) {
  if (is.null(x@tile_info)) {
    cli::cli_abort(c(
      "{.arg x} is not a cached service.",
      "i" = "{.val {x@name}} has no tiling scheme."
    ))
  }

  x@tile_info
}

#' Map service
#'
#' A `MapServer` endpoint. Use [map_server()] to create one.
#'
#' @inheritParams Service
#' @param cached Bool. Whether the service is served from a tile cache.
#' @param layers Data frame. The service's layers.
#' @param export_tiles_allowed Bool. Whether clients may export cache tiles.
#' @param max_export_tiles Double. Tile count ceiling for [export_tiles()].
#' @returns A `MapServer` object.
#' @family services
#' @export
MapServer <- S7::new_class(
  "MapServer",
  parent = Service,
  package = "arcgistiles",
  properties = list(
    cached = s7x::class_boolean,
    layers = S7::class_data.frame,
    export_tiles_allowed = s7x::class_boolean,
    max_export_tiles = s7x::class_float
  )
)

#' Vector tile service
#'
#' A `VectorTileServer` endpoint. Use [vector_tile_server()] to create one.
#'
#' @inheritParams Service
#' @inheritParams MapServer
#' @param tiles Character. Tile URL templates relative to the service.
#' @param default_styles String. Path to the service's default style resource.
#' @returns A `VectorTileServer` object.
#' @family services
#' @export
VectorTileServer <- S7::new_class(
  "VectorTileServer",
  parent = Service,
  package = "arcgistiles",
  properties = list(
    tiles = S7::class_character,
    default_styles = s7x::class_string,
    export_tiles_allowed = s7x::class_boolean,
    max_export_tiles = s7x::class_float
  )
)

#' Open a map service
#'
#' @param url String. URL of a `MapServer` endpoint.
#' @inheritParams arcgisutils::arc_base_req
#' @returns A [MapServer] object.
#' @family services
#' @export
#' @examples
#' \dontrun{
#' map_server(world_imagery_url())
#' }
map_server <- function(
  url,
  token = arcgisutils::arc_token(),
  error_call = rlang::caller_env()
) {
  meta <- fetch_service(url, "MapServer", token, error_call)

  extent <- as_extent_vec(meta[["fullExtent"]])
  crs <- as_crs(meta[["spatialReference"]], call = error_call)

  MapServer(
    url = sub("/$", "", url),
    metadata = meta,
    name = meta[["mapName"]] %||% NA_character_,
    crs = crs,
    full_extent = extent,
    tile_info = as_tile_info(meta[["tileInfo"]], call = error_call),
    capabilities = split_capabilities(meta[["capabilities"]]),
    token = token,
    cached = isTRUE(meta[["singleFusedMapCache"]]),
    layers = as_layer_table(meta[["layers"]]),
    export_tiles_allowed = isTRUE(meta[["exportTilesAllowed"]]),
    max_export_tiles = as.double(meta[["maxExportTilesCount"]] %||% NA_real_)
  )
}

#' Open a vector tile service
#'
#' @param url String. URL of a `VectorTileServer` endpoint.
#' @inheritParams arcgisutils::arc_base_req
#' @returns A [VectorTileServer] object.
#' @family services
#' @export
#' @examples
#' \dontrun{
#' vector_tile_server(open_street_map_url())
#' }
vector_tile_server <- function(
  url,
  token = arcgisutils::arc_token(),
  error_call = rlang::caller_env()
) {
  meta <- fetch_service(url, "VectorTileServer", token, error_call)

  VectorTileServer(
    url = sub("/$", "", url),
    metadata = meta,
    name = meta[["name"]] %||% NA_character_,
    crs = as_crs(meta[["tileInfo"]][["spatialReference"]], call = error_call),
    full_extent = as_extent_vec(meta[["fullExtent"]]),
    tile_info = as_tile_info(meta[["tileInfo"]], call = error_call),
    capabilities = split_capabilities(meta[["capabilities"]]),
    token = token,
    tiles = as.character(meta[["tiles"]] %||% "tile/{z}/{y}/{x}.pbf"),
    default_styles = meta[["defaultStyles"]] %||% "resources/styles",
    export_tiles_allowed = isTRUE(meta[["exportTilesAllowed"]]),
    max_export_tiles = as.double(meta[["maxExportTilesCount"]] %||% NA_real_)
  )
}

fetch_service <- function(url, kind, token, call = rlang::caller_env()) {
  check_string(url, allow_empty = FALSE, call = call)

  if (!grepl(paste0("/", kind, "/?$"), url)) {
    cli::cli_abort(
      "{.arg url} must point to a {.field {kind}} endpoint.",
      call = call
    )
  }

  arc_json(url, token = token, query = list(f = "json"), call = call)
}

arc_json <- function(url, token, query = list(f = "json"), call = rlang::caller_env()) {
  resp <- arcgisutils::arc_base_req(
    url,
    token,
    query = query,
    error_call = call
  ) |>
    httr2::req_perform(error_call = call)

  res <- RcppSimdJson::fparse(httr2::resp_body_string(resp))
  arcgisutils::detect_errors(res)
  res
}

as_extent_vec <- function(x) {
  if (is.null(x)) {
    return(rep(NA_real_, 4L))
  }

  as.double(c(x[["xmin"]], x[["ymin"]], x[["xmax"]], x[["ymax"]]))
}

split_capabilities <- function(x) {
  if (is.null(x)) {
    return(character())
  }

  trimws(strsplit(as.character(x), ",", fixed = TRUE)[[1L]])
}

as_layer_table <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(data.frame(id = integer(), name = character()))
  }

  if (!is.data.frame(x)) {
    x <- rbind_rows(x)
  }

  x
}

#' Extent of a service
#'
#' @param x A [MapServer] or [VectorTileServer].
#' @returns A `bbox`.
#' @family services
#' @export
#' @examples
#' \dontrun{
#' service_bbox(map_server(world_imagery_url()))
#' }
service_bbox <- function(x) {
  sf::st_bbox(
    stats::setNames(x@full_extent, c("xmin", "ymin", "xmax", "ymax")),
    crs = x@crs
  )
}

print_service <- function(x) {
  cli::cli_text("{.cls {class(x)[1]}} {.val {x@name}}")
  cli::cli_text("{.url {x@url}}")
  cli::cli_text("{.strong CRS:} {crs_label(x@crs)}")

  if (length(x@capabilities) > 0L) {
    cli::cli_text("{.strong Capabilities:} {.val {x@capabilities}}")
  }

  if (is.null(x@tile_info)) {
    cli::cli_text("{.strong Cached:} FALSE")
  } else {
    info <- x@tile_info
    levels <- info@lods[["level"]]
    cli::cli_text(
      "{.strong Levels:} {min(levels)}-{max(levels)} at {info@cols}x{info@rows} {info@format}"
    )
  }

  invisible(x)
}

S7::method(print, Service) <- function(x, ...) {
  print_service(x)
}

S7::method(print, MapServer) <- function(x, ...) {
  print_service(x)

  if (nrow(x@layers) > 0L) {
    cli::cli_text("{.strong Layers:} {nrow(x@layers)}")
  }

  invisible(x)
}
