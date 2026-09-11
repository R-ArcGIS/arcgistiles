#' Image format
#'
#' Output format of an exported map image.
#'
#' @param value String. One of the format variants.
#' @returns An `ImageFormat` enum.
#' @family enums
#' @export
#' @examples
#' image_format("png32")
image_format <- s7x::new_enum(
  "ImageFormat",
  c(
    "png",
    "png8",
    "png24",
    "png32",
    "jpg",
    "pdf",
    "bmp",
    "gif",
    "svg",
    "svgz",
    "emf",
    "ps"
  ),
  package = "arcgistiles",
  allow_na = FALSE,
  default = "png"
)

#' Layer visibility
#'
#' How the `layers` argument of [export_map()] is applied.
#'
#' @param value String. One of `"show"`, `"hide"`, `"include"`, or `"exclude"`.
#' @returns A `LayerVisibility` enum.
#' @family enums
#' @export
#' @examples
#' layer_visibility("hide")
layer_visibility <- s7x::new_enum(
  "LayerVisibility",
  c("show", "hide", "include", "exclude"),
  package = "arcgistiles",
  allow_na = FALSE,
  default = "show"
)

#' Tile export criteria
#'
#' How the `levels` argument of [export_tiles()] is interpreted.
#'
#' @param value String. One of `"LevelID"`, `"Resolution"`, or `"Scale"`.
#' @returns An `ExportBy` enum.
#' @family enums
#' @export
#' @examples
#' export_by("Scale")
export_by <- s7x::new_enum(
  "ExportBy",
  c("LevelID", "Resolution", "Scale"),
  package = "arcgistiles",
  allow_na = FALSE,
  default = "LevelID"
)

#' Tile package storage format
#'
#' `"CompactV2"` produces a `.tpkx` file, `"Compact"` a `.tpk` file.
#'
#' @param value String. One of `"CompactV2"` or `"Compact"`.
#' @returns A `StorageFormat` enum.
#' @family enums
#' @export
#' @examples
#' storage_format("Compact")
storage_format <- s7x::new_enum(
  "StorageFormat",
  c("CompactV2", "Compact"),
  package = "arcgistiles",
  allow_na = FALSE,
  default = "CompactV2"
)

#' Basemap style family
#'
#' @param value String. Either `"arcgis"` or `"open"`.
#' @returns A `StyleFamily` enum.
#' @family enums
#' @export
#' @examples
#' style_family("open")
style_family <- s7x::new_enum(
  "StyleFamily",
  c("arcgis", "open"),
  package = "arcgistiles",
  allow_na = FALSE,
  default = "arcgis"
)

#' Basemap places
#'
#' Controls which place labels a basemap style returns.
#'
#' @param value String. One of `"none"`, `"all"`, or `"attributed"`.
#' @returns A `Places` enum.
#' @family enums
#' @export
#' @examples
#' places("all")
places <- s7x::new_enum(
  "Places",
  c("none", "all", "attributed"),
  package = "arcgistiles",
  allow_na = FALSE,
  default = "none"
)

esri_storage_format <- function(x) {
  paste0("esriMapCacheStorageMode", as.character(x))
}
