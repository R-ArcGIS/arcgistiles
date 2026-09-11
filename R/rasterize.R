#' @include tiles.R
NULL

#' Convert to a georeferenced raster
#'
#' Reads downloaded imagery into a `SpatRaster` with the extent and CRS of the
#' service it came from. Requires the terra package.
#'
#' @param x A [TileSet] or [MapImage].
#' @param ... Passed to methods.
#' @returns A `terra::SpatRaster`.
#' @family tiles
#' @export
#' @examples
#' \dontrun{
#' ms <- map_server(world_imagery_url())
#' as_rast(get_tiles(ms, service_bbox(ms), level = 3L))
#' }
as_rast <- S7::new_generic("as_rast", "x")

S7::method(as_rast, TileSet) <- function(x, ...) {
  check_terra()

  if (identical(tolower(x@format), "pbf")) {
    cli::cli_abort(c(
      "Vector tiles cannot be converted to a raster.",
      "i" = "{.fn as_rast} works on {.cls MapServer} tiles."
    ))
  }

  tiles <- x@tiles[x@tiles[["ok"]], , drop = FALSE]

  if (nrow(tiles) == 0L) {
    cli::cli_abort("No tiles downloaded successfully.")
  }

  rasters <- lapply(seq_len(nrow(tiles)), function(i) {
    georeference(tiles[["path"]][i], unlist(tiles[i, c("xmin", "xmax", "ymin", "ymax")]), x@crs)
  })

  out <- if (length(rasters) == 1L) {
    rasters[[1L]]
  } else {
    terra::merge(terra::sprc(rasters))
  }

  name_bands(out)
}

georeference <- function(path, ext, crs) {
  r <- suppressWarnings(terra::rast(path))

  terra::ext(r) <- terra::ext(ext[["xmin"]], ext[["xmax"]], ext[["ymin"]], ext[["ymax"]])
  terra::crs(r) <- if (is.na(crs)) "" else crs[["wkt"]]

  r
}

name_bands <- function(x) {
  n <- terra::nlyr(x)

  names(x) <- switch(
    as.character(n),
    "1" = "gray",
    "3" = c("red", "green", "blue"),
    "4" = c("red", "green", "blue", "alpha"),
    paste0("band_", seq_len(n))
  )

  x
}

check_terra <- function(call = rlang::caller_env()) {
  rlang::check_installed("terra", "to build a raster from tiles.", call = call)
}
