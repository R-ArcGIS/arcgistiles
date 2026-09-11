#' @include services.R
NULL

#' Basemap styles service URL
#'
#' @returns The base URL of the basemap styles service, from the
#'   `arcgistiles.basemap_url` option.
#' @family basemaps
#' @export
#' @examples
#' basemap_url()
basemap_url <- function() {
  getOption(
    "arcgistiles.basemap_url",
    "https://basemapstyles-api.arcgis.com/arcgis/rest/services/styles/v2"
  )
}

#' Available basemap styles
#'
#' Lists the styles the basemap styles service publishes. No token is required.
#'
#' @param family String or [style_family()]. Restrict to one style family.
#' @inheritParams tile_grid
#' @returns A data frame with one row per style, including its `path`, `name`,
#'   `styleFamily`, and `group`.
#' @family basemaps
#' @export
#' @examples
#' \dontrun{
#' basemap_styles("arcgis")
#' }
basemap_styles <- function(family = NULL, error_call = rlang::caller_env()) {
  styles <- basemap_self(error_call = error_call)[["styles"]]

  if (is.null(family)) {
    return(styles)
  }

  family <- as.character(style_family(as.character(family)))
  styles[styles[["styleFamily"]] == family, , drop = FALSE]
}

#' Basemap styles service metadata
#'
#' The service's own description of the styles, languages, worldviews, and
#' places it supports. No token is required.
#'
#' @inheritParams tile_grid
#' @returns A list.
#' @family basemaps
#' @export
#' @examples
#' \dontrun{
#' names(basemap_self())
#' }
basemap_self <- function(error_call = rlang::caller_env()) {
  arc_json(
    paste0(basemap_url(), "/styles/self"),
    token = NULL,
    call = error_call
  )
}

#' Basemap label languages
#'
#' @inheritParams tile_grid
#' @returns A data frame with `code` and `name` columns.
#' @family basemaps
#' @export
#' @examples
#' \dontrun{
#' basemap_languages()
#' }
basemap_languages <- function(error_call = rlang::caller_env()) {
  basemap_self(error_call = error_call)[["languages"]]
}

#' Basemap worldviews
#'
#' The boundary and label treatments a style can be requested with.
#'
#' @inheritParams tile_grid
#' @returns A data frame with `code` and `name` columns.
#' @family basemaps
#' @export
#' @examples
#' \dontrun{
#' basemap_worldviews()
#' }
basemap_worldviews <- function(error_call = rlang::caller_env()) {
  basemap_self(error_call = error_call)[["worldviews"]]
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
#' @param language String. Label language code, from [basemap_languages()].
#' @param worldview String. Worldview code, from [basemap_worldviews()].
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
  arc_json(
    paste0(basemap_url(), "/styles/", style_path(style, family, error_call)),
    token = token,
    query = style_query(language, worldview, places, error_call),
    call = error_call
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
  arc_json(
    paste0(basemap_url(), "/webmaps/", style_path(style, family, error_call)),
    token = token,
    query = style_query(language, worldview, places, error_call),
    call = error_call
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

  vector_tile_server(
    style_source_url(doc, error_call),
    token = token,
    error_call = error_call
  )
}

style_source_url <- function(doc, call = rlang::caller_env()) {
  urls <- vapply(
    doc[["sources"]] %||% list(),
    function(s) s[["url"]] %||% NA_character_,
    character(1)
  )

  urls <- urls[!is.na(urls) & grepl("VectorTileServer", urls, fixed = TRUE)]

  if (length(urls) == 0L) {
    cli::cli_abort(
      "The style has no vector tile service source.",
      call = call
    )
  }

  sub("/$", "", urls[[1L]])
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

  arc_json(
    paste0(basemap_url(), "/sessions/start"),
    token = token,
    query = list(
      styleFamily = as.character(style_family(as.character(family))),
      durationSeconds = as.integer(duration),
      f = "json"
    ),
    call = error_call
  )
}

style_path <- function(style, family, call = rlang::caller_env()) {
  check_string(style, allow_empty = FALSE, call = call)

  if (grepl("/", style, fixed = TRUE)) {
    return(style)
  }

  paste0(as.character(style_family(as.character(family))), "/", style)
}

style_query <- function(language, worldview, places, call = rlang::caller_env()) {
  check_string(language, allow_null = TRUE, allow_empty = FALSE, call = call)
  check_string(worldview, allow_null = TRUE, allow_empty = FALSE, call = call)

  compact(list(
    language = language,
    worldview = worldview,
    places = if (!is.null(places)) as.character(places(as.character(places))),
    f = "json"
  ))
}
