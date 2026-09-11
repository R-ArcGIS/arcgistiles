#' @include services.R
NULL

# the basemap styles service is an ArcGIS Location Platform endpoint with no
# enterprise equivalent, so the host is fixed rather than derived from a portal
basemap_api <- "https://basemapstyles-api.arcgis.com/arcgis/rest/services/styles/v2"

#' Available basemap styles
#'
#' Lists the styles the basemap styles service publishes, along with the label
#' languages and worldviews they accept. No token is required.
#'
#' @param family String or [style_family()]. Restrict to one style family.
#' @inheritParams tile_grid
#' @returns A data frame with one row per style, including its `path`, `name`,
#'   `styleFamily`, and `group`. The `languages` and `worldviews` attributes
#'   hold the codes accepted by [basemap_style()].
#' @family basemaps
#' @export
#' @examples
#' \dontrun{
#' styles <- basemap_styles("arcgis")
#' attr(styles, "languages")
#' }
basemap_styles <- function(family = NULL, error_call = rlang::caller_env()) {
  self <- arcgisutils::fetch_layer_metadata(
    basemap_api,
    path = c("styles", "self"),
    call = error_call
  )

  styles <- self[["styles"]]

  if (!is.null(family)) {
    family <- as.character(style_family(as.character(family)))
    styles <- styles[styles[["styleFamily"]] == family, , drop = FALSE]
    row.names(styles) <- NULL
  }

  structure(
    data_frame(styles),
    languages = data_frame(self[["languages"]]),
    worldviews = data_frame(self[["worldviews"]])
  )
}

#' Fetch a basemap style
#'
#' Reads one style from the basemap styles service as a
#' [Mapbox GL style](https://maplibre.org/maplibre-style-spec/) document.
#' Requires a token with the `premium:user:basemaps` privilege.
#'
#' @param style String. A style path such as `"arcgis/navigation"`, or a bare
#'   style name, in which case `family` supplies the family.
#' @param family String or [style_family()]. Used when `style` has no family.
#' @param language String. Label language code, from the `languages` attribute
#'   of [basemap_styles()].
#' @param worldview String. Worldview code controlling how disputed boundaries
#'   are drawn, from the `worldviews` attribute of [basemap_styles()].
#' @param places String or [places()]. Which place labels to include.
#' @inheritParams arcgisutils::arc_base_req
#' @returns A list holding the style document.
#' @family basemaps
#' @export
#' @examples
#' \dontrun{
#' basemap_style("arcgis/navigation", language = "es")
#' }
basemap_style <- function(
  style,
  family = "arcgis",
  language = NULL,
  worldview = NULL,
  places = NULL,
  token = arcgisutils::arc_token(),
  error_call = rlang::caller_env()
) {
  basemap_document(
    "styles",
    style,
    family,
    language,
    worldview,
    places,
    token,
    error_call
  )
}

#' Fetch a basemap web map
#'
#' The web map equivalent of [basemap_style()], for styles that publish one.
#'
#' @inheritParams basemap_style
#' @returns A list holding the web map document.
#' @family basemaps
#' @export
#' @examples
#' \dontrun{
#' basemap_webmap("arcgis/navigation")
#' }
basemap_webmap <- function(
  style,
  family = "arcgis",
  language = NULL,
  worldview = NULL,
  places = NULL,
  token = arcgisutils::arc_token(),
  error_call = rlang::caller_env()
) {
  basemap_document(
    "webmaps",
    style,
    family,
    language,
    worldview,
    places,
    token,
    error_call
  )
}

basemap_document <- function(
  kind,
  style,
  family,
  language,
  worldview,
  places,
  token,
  call = rlang::caller_env()
) {
  check_string(style, allow_empty = FALSE, call = call)
  check_string(language, allow_null = TRUE, allow_empty = FALSE, call = call)
  check_string(worldview, allow_null = TRUE, allow_empty = FALSE, call = call)

  if (!grepl("/", style, fixed = TRUE)) {
    style <- paste0(as.character(style_family(as.character(family))), "/", style)
  }

  arcgisutils::fetch_layer_metadata(
    basemap_api,
    token,
    path = c(kind, strsplit(style, "/", fixed = TRUE)[[1L]]),
    query = compact(list(
      language = language,
      worldview = worldview,
      places = if (!is.null(places)) as.character(places(as.character(places)))
    )),
    call = call
  )
}

#' Open the vector tile service behind a basemap style
#'
#' Reads a style, finds the vector tile service in its `sources`, and opens it
#' so its tiles can be downloaded with [get_vector_tiles()].
#'
#' @inheritParams basemap_style
#' @returns A [VectorTileServer].
#' @family basemaps
#' @export
#' @examples
#' \dontrun{
#' basemap_tile_server("arcgis/navigation")
#' }
basemap_tile_server <- function(
  style,
  family = "arcgis",
  language = NULL,
  worldview = NULL,
  places = NULL,
  token = arcgisutils::arc_token(),
  error_call = rlang::caller_env()
) {
  doc <- basemap_style(
    style,
    family = family,
    language = language,
    worldview = worldview,
    places = places,
    token = token,
    error_call = error_call
  )

  urls <- vapply(
    doc[["sources"]] %||% list(),
    function(s) s[["url"]] %||% NA_character_,
    character(1)
  )

  urls <- urls[!is.na(urls) & grepl("VectorTileServer", urls, fixed = TRUE)]

  if (length(urls) == 0L) {
    cli::cli_abort(
      "The style has no vector tile service source.",
      call = error_call
    )
  }

  vector_tile_server(urls[[1L]], token = token, error_call = error_call)
}

#' Start a basemap session
#'
#' Exchanges a token for a session token, which is billed per session rather
#' than per tile. Requires an ArcGIS Location Platform account.
#'
#' @param family String or [style_family()]. Style family the session covers.
#' @param duration Integer. Session length in seconds.
#' @inheritParams arcgisutils::arc_base_req
#' @returns A list with the session `token`, `startTime`, and `endTime`.
#' @family basemaps
#' @export
#' @examples
#' \dontrun{
#' basemap_session("arcgis")
#' }
basemap_session <- function(
  family = "arcgis",
  duration = 43200L,
  token = arcgisutils::arc_token(),
  error_call = rlang::caller_env()
) {
  check_number_whole(duration, min = 1, call = error_call)

  arcgisutils::fetch_layer_metadata(
    basemap_api,
    token,
    path = c("sessions", "start"),
    query = list(
      styleFamily = as.character(style_family(as.character(family))),
      durationSeconds = as.integer(duration)
    ),
    call = error_call
  )
}
