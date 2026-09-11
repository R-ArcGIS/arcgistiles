#' @include services.R
NULL

#' Downloaded tiles
#'
#' A set of tiles on disk. Use [get_tiles()] or [get_vector_tiles()] to create
#' one.
#'
#' @param tiles Data frame. One row per tile with columns `level`, `row`,
#'   `col`, `xmin`, `ymin`, `xmax`, `ymax`, `path`, and `ok`.
#' @param crs Coordinate reference system of the tiles.
#' @param level Integer. Level of detail.
#' @param format String. Tile format.
#' @param url String. Service the tiles came from.
#' @returns A `TileSet` object.
#' @family tiles
#' @export
TileSet <- S7::new_class(
  "TileSet",
  package = "arcgistiles",
  properties = list(
    tiles = class_table,
    crs = class_crs,
    level = s7x::class_int,
    format = s7x::class_string,
    url = s7x::class_string
  )
)

S7::method(print, TileSet) <- function(x, ...) {
  ok <- sum(x@tiles[["ok"]])

  cli::cli_text("{.cls TileSet} {ok}/{nrow(x@tiles)} tiles at level {x@level}")
  cli::cli_text("{.strong Format:} {x@format}")
  cli::cli_text("{.strong CRS:} {crs_label(x@crs)}")

  invisible(x)
}

#' Bounding box of a tile set
#'
#' @param x A [TileSet].
#' @returns A `bbox` covering every tile that downloaded successfully.
#' @family tiles
#' @export
#' @examples
#' \dontrun{
#' tileset_bbox(get_tiles(map_server(world_imagery_url()), bbox, 6L))
#' }
tileset_bbox <- function(x) {
  tiles <- x@tiles[x@tiles[["ok"]], , drop = FALSE]

  if (nrow(tiles) == 0L) {
    cli::cli_abort("No tiles downloaded successfully.")
  }

  sf::st_bbox(
    c(
      xmin = min(tiles[["xmin"]]),
      ymin = min(tiles[["ymin"]]),
      xmax = max(tiles[["xmax"]]),
      ymax = max(tiles[["ymax"]])
    ),
    crs = x@crs
  )
}

#' Download map tiles
#'
#' Downloads every cached tile covering `bbox` at `level`. Tiles that are
#' missing from the cache are reported in the `ok` column rather than erroring.
#'
#' @param x A [MapServer] or [VectorTileServer].
#' @inheritParams tile_grid
#' @param level Integer. Level of detail. Defaults to the level that renders
#'   `bbox` closest to `size` pixels.
#' @param size Integer. Length two `c(width, height)` used to choose `level`.
#' @param dir String. Directory to write tiles into. Created if needed.
#' @param progress Bool. Show a download progress bar.
#' @returns A [TileSet].
#' @family tiles
#' @export
#' @examples
#' \dontrun{
#' ms <- map_server(world_imagery_url())
#' get_tiles(ms, service_bbox(ms), level = 3L)
#' }
get_tiles <- function(
  x,
  bbox,
  level = NULL,
  size = c(1024L, 1024L),
  dir = tempfile("tiles"),
  progress = TRUE,
  error_call = rlang::caller_env()
) {
  info <- tile_info(x)
  level <- level %||% level_for_size(x, bbox, size, error_call = error_call)

  grid <- tile_grid(x, bbox, level, error_call = error_call)
  ext <- tile_file_ext(info@format)

  paths <- file.path(
    dir,
    sprintf("%d_%d_%d.%s", grid[["level"]], grid[["row"]], grid[["col"]], ext)
  )

  urls <- sprintf(
    "%s/tile/%d/%d/%d%s",
    x@url,
    grid[["level"]],
    grid[["row"]],
    grid[["col"]],
    if (identical(ext, "pbf")) ".pbf" else ""
  )

  grid[["ok"]] <- download_all(urls, paths, x@token, progress, error_call)
  grid[["path"]] <- resolve_mixed(paths, grid[["ok"]], info@format)

  TileSet(
    tiles = grid,
    crs = info@crs,
    level = as.integer(level),
    format = info@format,
    url = x@url
  )
}

#' Download vector tiles
#'
#' Downloads the `.pbf` tiles covering `bbox` at `level`.
#'
#' @inheritParams get_tiles
#' @param x A [VectorTileServer].
#' @returns A [TileSet] whose `path` column points at `.pbf` files.
#' @family vector tiles
#' @export
#' @examples
#' \dontrun{
#' vts <- vector_tile_server(open_street_map_url())
#' get_vector_tiles(vts, service_bbox(vts), level = 3L)
#' }
get_vector_tiles <- function(
  x,
  bbox,
  level = NULL,
  size = c(1024L, 1024L),
  dir = tempfile("vector-tiles"),
  progress = TRUE,
  error_call = rlang::caller_env()
) {
  if (!S7::S7_inherits(x, VectorTileServer)) {
    cli::cli_abort(
      "{.arg x} must be a {.cls VectorTileServer}.",
      call = error_call
    )
  }

  get_tiles(
    x,
    bbox = bbox,
    level = level,
    size = size,
    dir = dir,
    progress = progress,
    error_call = error_call
  )
}

download_all <- function(urls, paths, token, progress, call = rlang::caller_env()) {
  dirs <- unique(dirname(paths))
  created <- vapply(
    dirs,
    function(d) dir.create(d, recursive = TRUE, showWarnings = FALSE),
    logical(1)
  )

  if (!all(created | dir.exists(dirs))) {
    cli::cli_abort("Could not create {.file {dirs[!created]}}.", call = call)
  }

  reqs <- lapply(urls, function(u) {
    arcgisutils::arc_base_req(u, token, error_call = call)
  })

  resps <- httr2::req_perform_parallel(
    reqs,
    paths = paths,
    on_error = "continue",
    progress = progress
  )

  vapply(resps, is_downloaded, logical(1))
}

is_downloaded <- function(resp) {
  !inherits(resp, "error") &&
    httr2::resp_status(resp) < 300L &&
    !identical(httr2::resp_header(resp, "blank-tile"), "true")
}

tile_file_ext <- function(format) {
  switch(
    toupper(format),
    "PBF" = "pbf",
    "JPEG" = "jpg",
    "JPG" = "jpg",
    "PNG" = "png",
    "PNG8" = "png",
    "PNG24" = "png",
    "PNG32" = "png",
    "MIXED" = "bin",
    tolower(format)
  )
}

# a mixed cache serves png or jpeg per tile, so the extension is only known
# once the bytes are on disk
resolve_mixed <- function(paths, ok, format) {
  if (!identical(toupper(format), "MIXED")) {
    return(paths)
  }

  for (i in which(ok)) {
    renamed <- sub("\\.bin$", paste0(".", sniff_image_ext(paths[i])), paths[i])

    if (file.rename(paths[i], renamed)) {
      paths[i] <- renamed
    }
  }

  paths
}

sniff_image_ext <- function(path) {
  magic <- readBin(path, "raw", 4L)

  if (identical(magic[1:3], as.raw(c(0xff, 0xd8, 0xff)))) {
    "jpg"
  } else {
    "png"
  }
}
