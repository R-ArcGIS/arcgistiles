#' @include tiles.R
NULL

#' Vector tile style
#'
#' Reads the service's style, a [Mapbox GL style](https://maplibre.org/maplibre-style-spec/)
#' document. The relative `sprite` and `glyphs` paths are resolved to absolute
#' URLs so they can be fetched directly.
#'
#' @param x A [VectorTileServer].
#' @inheritParams tile_grid
#' @returns A list holding the style document.
#' @family vector tiles
#' @export
#' @examples
#' \dontrun{
#' vector_tile_style(vector_tile_server(open_street_map_url()))
#' }
vector_tile_style <- function(x, error_call = rlang::caller_env()) {
  base <- paste0(x@url, "/", x@default_styles)
  style <- arc_json(base, token = x@token, call = error_call)

  root <- paste0(x@url, "/resources/")

  if (!is.null(style[["sprite"]])) {
    style[["sprite"]] <- resolve_relative(style[["sprite"]], root)
  }

  if (!is.null(style[["glyphs"]])) {
    style[["glyphs"]] <- resolve_relative(style[["glyphs"]], root)
  }

  style
}

# style documents reference sibling resources as `../sprites/sprite`
resolve_relative <- function(path, root) {
  sub("^(\\.\\./)+", root, path)
}

#' Vector tile resources
#'
#' Lists the font and sprite resource files the service publishes.
#'
#' @inheritParams vector_tile_style
#' @returns A character vector of resource paths relative to the service.
#' @family vector tiles
#' @export
#' @examples
#' \dontrun{
#' vector_tile_resources(vector_tile_server(open_street_map_url()))
#' }
vector_tile_resources <- function(x, error_call = rlang::caller_env()) {
  res <- arc_json(
    paste0(x@url, "/resources/info"),
    token = x@token,
    call = error_call
  )

  as.character(res[["resourceInfo"]])
}

#' Vector tile fonts
#'
#' The font stacks a vector tile service publishes glyphs for.
#'
#' @inheritParams vector_tile_style
#' @returns A character vector of font stack names.
#' @family vector tiles
#' @export
#' @examples
#' \dontrun{
#' vector_tile_fonts(vector_tile_server(open_street_map_url()))
#' }
vector_tile_fonts <- function(x, error_call = rlang::caller_env()) {
  resources <- vector_tile_resources(x, error_call = error_call)
  fonts <- grep("fonts/", resources, fixed = TRUE, value = TRUE)

  sort(unique(sub("^.*fonts/(.*)/[^/]+$", "\\1", fonts)))
}

#' Download a vector tile sprite
#'
#' Downloads the sprite sheet and its index.
#'
#' @inheritParams vector_tile_style
#' @param dir String. Directory to write into. Created if needed.
#' @param retina Bool. Download the 2x sprite sheet instead.
#' @returns A named character vector with the `json` and `png` paths.
#' @family vector tiles
#' @export
#' @examples
#' \dontrun{
#' vector_tile_sprite(vector_tile_server(open_street_map_url()))
#' }
vector_tile_sprite <- function(
  x,
  dir = tempfile("sprite"),
  retina = FALSE,
  error_call = rlang::caller_env()
) {
  check_bool(retina, call = error_call)

  style <- vector_tile_style(x, error_call = error_call)
  base <- style[["sprite"]] %||% paste0(x@url, "/resources/sprites/sprite")

  if (retina) {
    base <- paste0(base, "@2x")
  }

  urls <- paste0(base, c(".json", ".png"))
  paths <- file.path(dir, paste0("sprite", c(".json", ".png")))

  ok <- download_all(urls, paths, x@token, progress = FALSE, call = error_call)

  if (!all(ok)) {
    cli::cli_abort("Could not download the sprite.", call = error_call)
  }

  stats::setNames(paths, c("json", "png"))
}

#' Download vector tile glyphs
#'
#' Downloads a range of glyphs for one font stack.
#'
#' @inheritParams vector_tile_sprite
#' @param font String. Font stack name, from [vector_tile_fonts()].
#' @param range String. Unicode range, such as `"0-255"`.
#' @returns The path to the downloaded `.pbf` file.
#' @family vector tiles
#' @export
#' @examples
#' \dontrun{
#' vts <- vector_tile_server(open_street_map_url())
#' vector_tile_font(vts, vector_tile_fonts(vts)[1], "0-255")
#' }
vector_tile_font <- function(
  x,
  font,
  range = "0-255",
  dir = tempfile("fonts"),
  error_call = rlang::caller_env()
) {
  check_string(font, allow_empty = FALSE, call = error_call)
  check_string(range, allow_empty = FALSE, call = error_call)

  url <- paste0(
    x@url,
    "/resources/fonts/",
    utils::URLencode(font, reserved = TRUE),
    "/",
    range,
    ".pbf"
  )

  path <- file.path(dir, paste0(gsub("[^A-Za-z0-9]+", "-", font), "_", range, ".pbf"))
  ok <- download_all(url, path, x@token, progress = FALSE, call = error_call)

  if (!ok) {
    cli::cli_abort(
      "Could not download glyphs for {.val {font}}.",
      call = error_call
    )
  }

  path
}

#' Tile availability
#'
#' Asks a service which tiles in a block actually exist in its cache. Only
#' services whose capabilities include `Tilemap` support this.
#'
#' @inheritParams tile_grid
#' @param level,row,col Integer. Upper left tile of the block to check.
#' @param width,height Integer. Size of the block in tiles.
#' @returns A data frame with columns `level`, `row`, `col`, and `available`.
#' @family tiles
#' @export
#' @examples
#' \dontrun{
#' tilemap(map_server(world_imagery_url()), 6L, 24L, 18L, 4L, 4L)
#' }
tilemap <- function(
  x,
  level,
  row,
  col,
  width = 8L,
  height = 8L,
  error_call = rlang::caller_env()
) {
  if (!"Tilemap" %in% x@capabilities) {
    cli::cli_abort(
      "{.val {x@name}} does not support the {.path /tilemap} operation.",
      call = error_call
    )
  }

  check_number_whole(width, min = 1, call = error_call)
  check_number_whole(height, min = 1, call = error_call)

  res <- arc_json(
    sprintf(
      "%s/tilemap/%d/%d/%d/%d/%d",
      x@url,
      as.integer(level),
      as.integer(row),
      as.integer(col),
      as.integer(width),
      as.integer(height)
    ),
    token = x@token,
    call = error_call
  )

  location <- res[["location"]] %||% list(top = row, left = col, width = width, height = height)
  data <- res[["data"]] %||% rep(as.integer(isTRUE(res[["valid"]])), width * height)

  grid <- expand.grid(
    col = seq.int(location[["left"]], length.out = location[["width"]]),
    row = seq.int(location[["top"]], length.out = location[["height"]])
  )

  data.frame(
    level = as.integer(level),
    row = grid[["row"]],
    col = grid[["col"]],
    available = as.logical(data)
  )
}
