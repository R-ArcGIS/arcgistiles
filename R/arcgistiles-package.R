#' @keywords internal
#' @aliases arcgistiles-package
#' @section Getting started:
#' Open a service with [map_server()] or [vector_tile_server()]. Download the
#' tiles covering an area with [get_tiles()], or render an extent on the server
#' with [export_map()].
#'
#' Either way, [as_rast()] turns the result into a georeferenced `SpatRaster`
#' you can plot your own vector data on top of.
"_PACKAGE"

## usethis namespace: start
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
#' @importFrom S7 method new_class new_property prop
#' @importFrom s7x new_enum class_string
## usethis namespace: end
NULL
