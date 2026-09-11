#' @include utils.R
NULL

#' Tiling scheme
#'
#' The tiling scheme of a cached map or vector tile service.
#'
#' @param rows,cols Integer. Tile height and width in pixels.
#' @param dpi Integer. Dots per inch the cache was built at.
#' @param format String. Tile image format, or `"pbf"` for vector tiles.
#' @param origin Double. Length two `c(x, y)` upper left corner of the grid.
#' @param crs Coordinate reference system of the tiling scheme.
#' @param lods Data frame. Columns `level`, `resolution`, and `scale`.
#' @returns A `TileInfo` object.
#' @family tiling scheme
#' @export
#' @examples
#' TileInfo(
#'   rows = 256L,
#'   cols = 256L,
#'   dpi = 96L,
#'   format = "PNG",
#'   origin = c(-20037508.342787, 20037508.342787),
#'   crs = sf::st_crs(3857),
#'   lods = data.frame(
#'     level = 0:1,
#'     resolution = c(156543.033928, 78271.5169639),
#'     scale = c(591657527.6, 295828763.8)
#'   )
#' )
TileInfo <- S7::new_class(
  "TileInfo",
  package = "arcgistiles",
  properties = list(
    rows = s7x::class_int,
    cols = s7x::class_int,
    dpi = s7x::class_int,
    format = s7x::class_string,
    origin = S7::class_double,
    crs = class_crs,
    lods = S7::class_data.frame
  ),
  validator = function(self) {
    if (length(self@origin) != 2L) {
      return("@origin must be length two")
    }

    missing <- setdiff(c("level", "resolution", "scale"), names(self@lods))

    if (length(missing) > 0L) {
      return(cli::format_inline("@lods is missing {.field {missing}}"))
    }

    NULL
  }
)

S7::method(print, TileInfo) <- function(x, ...) {
  levels <- x@lods[["level"]]

  cli::cli_text("{.cls TileInfo} {x@cols}x{x@rows} {x@format} at {x@dpi} dpi")
  cli::cli_text("{.strong CRS:} {crs_label(x@crs)}")
  cli::cli_text("{.strong Origin:} {.val {x@origin}}")
  cli::cli_text(
    "{.strong Levels:} {min(levels)}-{max(levels)} ({length(levels)} lods)"
  )

  invisible(x)
}

as_tile_info <- function(x, call = rlang::caller_env()) {
  if (is.null(x)) {
    return(NULL)
  }

  lods <- x[["lods"]]

  if (!is.data.frame(lods)) {
    lods <- rbind_rows(lods)
  }

  lods <- lods[order(lods[["level"]]), c("level", "resolution", "scale")]
  row.names(lods) <- NULL

  TileInfo(
    rows = as.integer(x[["rows"]]),
    cols = as.integer(x[["cols"]]),
    dpi = as.integer(x[["dpi"]]),
    format = as.character(x[["format"]]),
    origin = c(x[["origin"]][["x"]], x[["origin"]][["y"]]),
    crs = as_crs(x[["spatialReference"]], call = call),
    lods = lods
  )
}

#' Tiling scheme of a service
#'
#' @param x A [TileInfo], [MapServer], or [VectorTileServer].
#' @returns A [TileInfo] object.
#' @family tiling scheme
#' @export
#' @examples
#' \dontrun{
#' tile_info(map_server(world_imagery_url()))
#' }
tile_info <- S7::new_generic("tile_info", "x")

S7::method(tile_info, TileInfo) <- function(x) x

#' Levels of detail
#'
#' The levels of a tiling scheme, one row per level.
#'
#' @inheritParams tile_info
#' @returns A data frame with columns `level`, `resolution`, `scale`,
#'   `tile_width`, and `tile_height`, the latter two in map units.
#' @family tiling scheme
#' @export
#' @examples
#' \dontrun{
#' lods(map_server(world_imagery_url()))
#' }
lods <- function(x) {
  info <- tile_info(x)
  res <- info@lods

  res[["tile_width"]] <- res[["resolution"]] * info@cols
  res[["tile_height"]] <- res[["resolution"]] * info@rows
  res
}

#' Level nearest a resolution
#'
#' Finds the level of detail whose resolution is closest to `resolution` on a
#' log scale.
#'
#' @inheritParams tile_info
#' @param resolution Double. Target map units per pixel.
#' @returns An integer level.
#' @family tiling scheme
#' @export
#' @examples
#' \dontrun{
#' level_for_resolution(map_server(world_imagery_url()), 100)
#' }
level_for_resolution <- function(x, resolution, error_call = rlang::caller_env()) {
  check_number_decimal(resolution, allow_infinite = FALSE, call = error_call)

  if (resolution <= 0) {
    cli::cli_abort("{.arg resolution} must be greater than 0.", call = error_call)
  }

  info <- tile_info(x)
  available <- info@lods[["resolution"]]

  info@lods[["level"]][which.min(abs(log(available) - log(resolution)))]
}

#' Level for a bounding box and image size
#'
#' Picks the level of detail whose resolution renders `bbox` closest to `size`
#' pixels.
#'
#' @inheritParams tile_grid
#' @param size Integer. Length two `c(width, height)` in pixels.
#' @returns An integer level.
#' @family tiling scheme
#' @export
#' @examples
#' \dontrun{
#' level_for_size(map_server(world_imagery_url()), bbox, c(1024, 1024))
#' }
level_for_size <- function(
  x,
  bbox,
  size = c(1024L, 1024L),
  error_call = rlang::caller_env()
) {
  info <- tile_info(x)
  bbox <- as_tile_bbox(bbox, info@crs, call = error_call)
  size <- check_size(size, call = error_call)

  level_for_resolution(
    info,
    max(
      (bbox[["xmax"]] - bbox[["xmin"]]) / size[1L],
      (bbox[["ymax"]] - bbox[["ymin"]]) / size[2L]
    ),
    error_call = error_call
  )
}

lod_index <- function(info, level, call = rlang::caller_env()) {
  i <- match(level, info@lods[["level"]])

  if (anyNA(i)) {
    cli::cli_abort(
      c(
        "{.arg level} {.val {unique(level[is.na(i)])}} is not in the tiling scheme.",
        "i" = "Levels {.val {range(info@lods$level)}} are available."
      ),
      call = call
    )
  }

  i
}

#' Extent of a tile
#'
#' @inheritParams tile_info
#' @param level,row,col Integer. Tile coordinates, recycled to a common length.
#' @returns A data frame with columns `level`, `row`, `col`, `xmin`, `ymin`,
#'   `xmax`, and `ymax`.
#' @family tiling scheme
#' @export
#' @examples
#' \dontrun{
#' tile_extent(map_server(world_imagery_url()), 6L, 24L, 18L)
#' }
tile_extent <- function(x, level, row, col, error_call = rlang::caller_env()) {
  info <- tile_info(x)

  args <- recycle_common(
    level = as.integer(level),
    row = as.integer(row),
    col = as.integer(col),
    call = error_call
  )

  resolution <- info@lods[["resolution"]][lod_index(info, args[["level"]], error_call)]

  width <- resolution * info@cols
  height <- resolution * info@rows

  xmin <- info@origin[1L] + args[["col"]] * width
  ymax <- info@origin[2L] - args[["row"]] * height

  data.frame(
    level = args[["level"]],
    row = args[["row"]],
    col = args[["col"]],
    xmin = xmin,
    ymin = ymax - height,
    xmax = xmin + width,
    ymax = ymax
  )
}

#' Tiles covering a bounding box
#'
#' Computes the tile coordinates covering `bbox` at `level`.
#'
#' @inheritParams tile_info
#' @param bbox A `bbox`, an object with a bounding box, or a length four
#'   numeric `c(xmin, ymin, xmax, ymax)`.
#' @param level Integer. Level of detail.
#' @inheritParams rlang::args_error_context
#' @returns A data frame with columns `level`, `row`, `col`, `xmin`, `ymin`,
#'   `xmax`, and `ymax`, ordered by row then column.
#' @family tiling scheme
#' @export
#' @examples
#' \dontrun{
#' tile_grid(map_server(world_imagery_url()), bbox, 6L)
#' }
tile_grid <- function(x, bbox, level, error_call = rlang::caller_env()) {
  info <- tile_info(x)
  check_number_whole(level, call = error_call)

  level <- as.integer(level)
  bbox <- as_tile_bbox(bbox, info@crs, call = error_call)

  resolution <- info@lods[["resolution"]][lod_index(info, level, error_call)]

  width <- resolution * info@cols
  height <- resolution * info@rows

  cols <- span_indices(bbox[["xmin"]] - info@origin[1L], bbox[["xmax"]] - info@origin[1L], width)
  rows <- span_indices(info@origin[2L] - bbox[["ymax"]], info@origin[2L] - bbox[["ymin"]], height)

  grid <- expand.grid(col = cols, row = rows)

  tile_extent(info, level, grid[["row"]], grid[["col"]], error_call = error_call)
}

span_indices <- function(lo, hi, size) {
  tol <- sqrt(.Machine$double.eps)

  first <- floor(lo / size + tol)
  last <- ceiling(hi / size - tol) - 1

  seq.int(max(first, 0), max(last, first, 0))
}
