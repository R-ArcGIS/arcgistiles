#' @include tiles.R
NULL

#' Read vector tiles into sf
#'
#' Decodes downloaded `.pbf` tiles and binds each layer across tiles. Requires
#' the protolite package.
#'
#' Features are stored clipped to their tile, so a road crossing a tile
#' boundary arrives as one feature per tile.
#'
#' @param x A [TileSet] of vector tiles, from [get_vector_tiles()].
#' @param layers Character. Layers to read. Defaults to every layer present.
#' @param crs Coordinate reference system to return. Defaults to `EPSG:4326`,
#'   which is what the tiles decode to.
#' @inheritParams tile_grid
#' @returns A named list of `sf` data frames, one per layer.
#' @family vector tiles
#' @export
#' @examplesIf curl::has_internet() && rlang::is_installed("protolite")
#' vts <- vector_tile_server(open_street_map_url())
#' boston <- wk::rct(-71.07, 42.35, -71.05, 42.36, crs = 4326)
#' tiles <- get_vector_tiles(vts, boston, level = 14L, progress = FALSE)
#'
#' roads <- read_vector_tiles(tiles, layers = "road", crs = 3857)
#' roads$road
read_vector_tiles <- function(
  x,
  layers = NULL,
  crs = NULL,
  error_call = rlang::caller_env()
) {
  check_character(layers, allow_null = TRUE, call = error_call)
  check_vector_tiles(x, error_call)

  decoded <- decode_tiles(x, error_call)
  present <- unique(unlist(lapply(decoded, names)))

  if (!is.null(layers)) {
    unknown <- setdiff(layers, present)

    if (length(unknown) > 0L) {
      cli::cli_abort(
        c(
          "{.arg layers} {.val {unknown}} {?is/are} not in these tiles.",
          "i" = "Use {.fn vector_tile_layers} to list them."
        ),
        call = error_call
      )
    }

    present <- layers
  }

  out <- lapply(present, function(layer) {
    pieces <- lapply(compact(lapply(decoded, function(tile) tile[[layer]])), function(piece) {
      piece[!sf::st_is_empty(sf::st_geometry(piece)), , drop = FALSE]
    })

    pieces <- pieces[vapply(pieces, nrow, integer(1)) > 0L]

    if (length(pieces) == 0L) {
      return(NULL)
    }

    # a layer is LINESTRING in one tile and MULTILINESTRING in the next, which
    # rbind_results() promotes for us
    rbind_results(pieces, call = error_call)
  })

  names(out) <- present
  out <- compact(out)

  if (is.null(crs)) {
    return(out)
  }

  lapply(out, sf::st_transform, crs = sf::st_crs(crs))
}

#' Layers present in vector tiles
#'
#' @inheritParams read_vector_tiles
#' @returns A character vector of layer names.
#' @family vector tiles
#' @export
#' @examplesIf curl::has_internet() && rlang::is_installed("protolite")
#' vts <- vector_tile_server(open_street_map_url())
#' boston <- wk::rct(-71.07, 42.35, -71.05, 42.36, crs = 4326)
#'
#' vector_tile_layers(get_vector_tiles(vts, boston, level = 14L, progress = FALSE))
vector_tile_layers <- function(x, error_call = rlang::caller_env()) {
  check_vector_tiles(x, error_call)

  sort(unique(unlist(lapply(decode_tiles(x, error_call), names))))
}

check_vector_tiles <- function(x, call = rlang::caller_env()) {
  rlang::check_installed("protolite", "to decode vector tiles.", call = call)

  if (!S7::S7_inherits(x, TileSet) || !identical(tolower(x@format), "pbf")) {
    cli::cli_abort(
      "{.arg x} must be a {.cls TileSet} of vector tiles.",
      call = call
    )
  }

  if (!any(x@tiles[["ok"]])) {
    cli::cli_abort("No tiles downloaded successfully.", call = call)
  }

  invisible(x)
}

# protolite takes tile coordinates as z, x, y where x is the column and y the
# row of the esri tile path
decode_tiles <- function(x, call = rlang::caller_env()) {
  tiles <- x@tiles[x@tiles[["ok"]], , drop = FALSE]

  lapply(seq_len(nrow(tiles)), function(i) {
    protolite::read_mvt_sf(
      tiles[["path"]][i],
      zxy = c(tiles[["level"]][i], tiles[["col"]][i], tiles[["row"]][i])
    )
  })
}
