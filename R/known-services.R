#' Public ArcGIS service URLs
#'
#' URLs for a handful of well known public services, to use in examples and
#' when trying the package out. None of them require a token.
#'
#' @returns A service URL.
#' @family services
#' @name known-services
#' @examples
#' world_imagery_url()
NULL

#' @rdname known-services
#' @export
world_imagery_url <- function() {
  "https://services.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer"
}

#' @rdname known-services
#' @export
world_street_map_url <- function() {
  "https://services.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer"
}

#' @rdname known-services
#' @export
world_topo_map_url <- function() {
  "https://services.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer"
}

#' @rdname known-services
#' @export
world_hillshade_url <- function() {
  "https://services.arcgisonline.com/ArcGIS/rest/services/Elevation/World_Hillshade/MapServer"
}

#' @rdname known-services
#' @export
open_street_map_url <- function() {
  "https://basemaps.arcgis.com/arcgis/rest/services/OpenStreetMap_v2/VectorTileServer"
}

#' @rdname known-services
#' @export
census_url <- function() {
  "https://sampleserver6.arcgisonline.com/arcgis/rest/services/Census/MapServer"
}
