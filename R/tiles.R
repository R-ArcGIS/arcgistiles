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
    tiles = class_data_frame,
    crs = S7::new_union(NULL, S7::new_S3_class("crs")),
    level = s7x::class_int,
    format = s7x::class_string,
    url = s7x::class_string
  )
)

S7::method(print, TileSet) <- function(x, ...) {
  ok <- sum(x@tiles[["ok"]])

  cli::cli_text("{.cls TileSet} {ok}/{nrow(x@tiles)} tiles at level {x@level}")
  cli::cli_text("{.strong Format:} {x@format}")
  cli::cli_text("{.strong CRS:} {x@crs$input %||% 'unknown'}")

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

  ext <- switch(
    toupper(info@format),
    "PBF" = "pbf",
    "JPEG" = "jpg",
    "JPG" = "jpg",
    "PNG" = "png",
    "PNG8" = "png",
    "PNG24" = "png",
    "PNG32" = "png",
    "MIXED" = "bin",
    tolower(info@format)
  )

  paths <- file.path(
    dir,
    sprintf("%d_%d_%d.%s", grid[["level"]], grid[["row"]], grid[["col"]], ext)
  )

  # esri serves tiles at tile/{level}/{row}/{col}, with a .pbf suffix on
  # vector tiles only
  urls <- vapply(
    seq_len(nrow(grid)),
    function(i) {
      httr2::url_modify(
        x@url,
        path = file.path(
          httr2::url_parse(x@url)[["path"]],
          "tile",
          grid[["level"]][i],
          grid[["row"]][i],
          paste0(grid[["col"]][i], if (identical(ext, "pbf")) ".pbf")
        )
      )
    },
    character(1)
  )

  grid[["ok"]] <- download_all(urls, paths, x@token, progress, error_call)

  # a mixed cache serves png or jpeg per tile, so the extension is only known
  # once the bytes are on disk
  if (identical(toupper(info@format), "MIXED")) {
    for (i in which(grid[["ok"]])) {
      magic <- readBin(paths[i], "raw", 3L)
      jpeg <- identical(magic, as.raw(c(0xff, 0xd8, 0xff)))
      renamed <- sub("\\.bin$", if (jpeg) ".jpg" else ".png", paths[i])

      if (file.rename(paths[i], renamed)) {
        paths[i] <- renamed
      }
    }
  }

  grid[["path"]] <- paths

  TileSet(
    tiles = data_frame(grid),
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

  vapply(
    resps,
    function(resp) {
      !inherits(resp, "error") &&
        httr2::resp_status(resp) < 300L &&
        !identical(httr2::resp_header(resp, "blank-tile"), "true")
    },
    logical(1)
  )
}

