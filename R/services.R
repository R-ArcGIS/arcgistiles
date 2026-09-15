#' @include tile-info.R
NULL

#' ArcGIS service
#'
#' Abstract parent of [MapServer] and [VectorTileServer].
#'
#' @param url String. Service URL.
#' @param metadata List. The service's JSON metadata.
#' @param name String. Service name.
#' @param full_extent A `bbox` carrying the service's extent and CRS.
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
    full_extent = class_rct,
    tile_info = S7::new_union(NULL, TileInfo),
    capabilities = S7::class_character,
    token = class_token,
    crs = S7::new_property(
      getter = function(self) wk_crs(self@full_extent)
    )
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
#' Layer level access belongs to `arcgislayers`. Use `arcgislayers::arc_open()`
#' and `arcgislayers::get_layer()` to reach the features behind a map service.
#'
#' @inheritParams Service
#' @param cached Bool. Whether the service is served from a tile cache.
#' @param export_tiles_allowed Bool. Whether clients may export cache tiles.
#' @param max_export_tiles Double. Tile count ceiling for [export_tiles()].
#' @returns A `MapServer` object.
#' @family services
#' @export
#' @examplesIf curl::has_internet()
#' ms <- map_server(world_imagery_url())
#'
#' ms@full_extent
#' ms@cached
MapServer <- S7::new_class(
  "MapServer",
  parent = Service,
  package = "arcgistiles",
  properties = list(
    cached = s7x::class_boolean,
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
#' @examplesIf curl::has_internet()
#' vts <- vector_tile_server(open_street_map_url())
#'
#' vts@full_extent
#' vts@tiles
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
#' @param x A `MapServer` URL, a `MapServer` from `arcgislayers::arc_open()`,
#'   or a `PortalItem` whose `url` points at one.
#' @inheritParams arcgisutils::arc_base_req
#' @returns A [MapServer] object.
#' @family services
#' @export
#' @examplesIf curl::has_internet()
#' map_server(world_imagery_url())
map_server <- function(
  x,
  token = arcgisutils::arc_token(),
  error_call = rlang::caller_env()
) {
  url <- service_url(x, "MapServer", call = error_call)
  meta <- arcgisutils::fetch_layer_metadata(url, token, call = error_call)

  MapServer(
    url = url,
    metadata = meta,
    name = meta[["mapName"]] %||% NA_character_,
    full_extent = service_extent(meta, error_call),
    tile_info = as_tile_info(meta[["tileInfo"]], call = error_call),
    capabilities = split_capabilities(meta[["capabilities"]]),
    token = token,
    cached = isTRUE(meta[["singleFusedMapCache"]]),
    export_tiles_allowed = isTRUE(meta[["exportTilesAllowed"]]),
    max_export_tiles = as.double(meta[["maxExportTilesCount"]] %||% NA_real_)
  )
}

#' Open a vector tile service
#'
#' @inheritParams map_server
#' @param x A `VectorTileServer` URL, or a `PortalItem` whose `url` points at
#'   one.
#' @returns A [VectorTileServer] object.
#' @family services
#' @export
#' @examplesIf curl::has_internet()
#' vector_tile_server(open_street_map_url())
vector_tile_server <- function(
  x,
  token = arcgisutils::arc_token(),
  error_call = rlang::caller_env()
) {
  url <- service_url(x, "VectorTileServer", call = error_call)
  meta <- arcgisutils::fetch_layer_metadata(url, token, call = error_call)

  VectorTileServer(
    url = url,
    metadata = meta,
    name = meta[["name"]] %||% NA_character_,
    full_extent = service_extent(meta, error_call),
    tile_info = as_tile_info(meta[["tileInfo"]], call = error_call),
    capabilities = split_capabilities(meta[["capabilities"]]),
    token = token,
    tiles = as.character(meta[["tiles"]] %||% "tile/{z}/{y}/{x}.pbf"),
    default_styles = meta[["defaultStyles"]] %||% "resources/styles",
    export_tiles_allowed = isTRUE(meta[["exportTilesAllowed"]]),
    max_export_tiles = as.double(meta[["maxExportTilesCount"]] %||% NA_real_)
  )
}

# arcgislayers services and portal items are both classed lists carrying a
# `url`, so neither package has to depend on the other
service_url <- function(x, kind, call = rlang::caller_env()) {
  if (!is.character(x)) {
    url <- x[["url"]]

    if (!rlang::is_string(url)) {
      cli::cli_abort(
        c(
          "{.arg x} must be a URL, a {.cls {kind}}, or a {.cls PortalItem}.",
          "i" = "{.cls {class(x)[1]}} carries no {.field url}."
        ),
        call = call
      )
    }

    x <- url
  }

  check_string(x, allow_empty = FALSE, call = call)
  x <- sub("/$", "", x)

  if (!grepl(paste0("/", kind, "$"), x)) {
    cli::cli_abort(
      "{.arg x} must point to a {.field {kind}} endpoint.",
      call = call
    )
  }

  x
}

service_extent <- function(meta, call = rlang::caller_env()) {
  extent <- meta[["fullExtent"]] %||% meta[["initialExtent"]]

  if (is.null(extent)) {
    return(NULL)
  }

  # an envelope often carries a bare `wkid`, which resolves to the ESRI
  # authority rather than the EPSG code the service publishes as `latestWkid`
  candidates <- compact(list(
    meta[["spatialReference"]],
    meta[["tileInfo"]][["spatialReference"]],
    extent[["spatialReference"]]
  ))

  richest <- Position(
    function(sr) !is.null(sr[["latestWkid"]]),
    candidates,
    nomatch = 1L
  )

  extent[["spatialReference"]] <- candidates[[richest]]

  as_bbox(arcgisutils::from_envelope(extent, error_call = call))
}

split_capabilities <- function(x) {
  if (is.null(x)) {
    return(character())
  }

  trimws(strsplit(as.character(x), ",", fixed = TRUE)[[1L]])
}

S7::method(print, Service) <- function(x, ...) {
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
