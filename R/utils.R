class_crs <- S7::new_union(S7::new_S3_class("crs"), NULL)
class_token <- S7::new_union(S7::new_S3_class("httr2_token"), NULL)

`%||%` <- function(x, y) if (is.null(x)) y else x

crs_label <- function(x) {
  if (is.null(x) || is.na(x)) {
    return("unknown")
  }

  x[["input"]] %||% format(x)
}

as_crs <- function(x, call = rlang::caller_env()) {
  if (is.null(x)) {
    return(sf::NA_crs_)
  }

  if (inherits(x, "crs")) {
    return(x)
  }

  if (is.numeric(x) || is.character(x)) {
    return(sf::st_crs(x))
  }

  wkid <- x[["latestWkid"]] %||% x[["wkid"]]

  if (!is.null(wkid)) {
    return(sf::st_crs(as.integer(wkid)))
  }

  wkt <- x[["wkt"]] %||% x[["wkt2"]]

  if (!is.null(wkt)) {
    return(sf::st_crs(wkt))
  }

  cli::cli_abort("Could not read a spatial reference.", call = call)
}

#' @importFrom sf st_bbox
as_tile_bbox <- function(bbox, crs = NULL, call = rlang::caller_env()) {
  if (!inherits(bbox, "bbox")) {
    if (!is.numeric(bbox) || length(bbox) != 4L) {
      bbox <- tryCatch(
        sf::st_bbox(bbox),
        error = function(e) {
          cli::cli_abort(
            "{.arg bbox} must be a {.cls bbox} or a length four numeric.",
            call = call
          )
        }
      )
    } else {
      bbox <- sf::st_bbox(
        stats::setNames(as.double(bbox), c("xmin", "ymin", "xmax", "ymax")),
        crs = crs
      )
    }
  }

  if (is.null(crs) || is.na(crs)) {
    return(bbox)
  }

  if (is.na(sf::st_crs(bbox))) {
    return(sf::st_bbox(stats::setNames(as.double(bbox), names(bbox)), crs = crs))
  }

  if (sf::st_crs(bbox) == crs) {
    return(bbox)
  }

  sf::st_bbox(sf::st_transform(sf::st_as_sfc(bbox), crs))
}

check_size <- function(size, call = rlang::caller_env()) {
  if (!is.numeric(size) || length(size) != 2L || anyNA(size)) {
    cli::cli_abort(
      "{.arg size} must be a length two numeric {.code c(width, height)}.",
      call = call
    )
  }

  size <- as.integer(round(size))

  if (any(size < 1L)) {
    cli::cli_abort("{.arg size} must be positive.", call = call)
  }

  size
}

recycle_common <- function(..., call = rlang::caller_env()) {
  args <- list(...)
  sizes <- unique(lengths(args))
  n <- max(sizes)

  if (!all(sizes %in% c(1L, n))) {
    cli::cli_abort(
      "Arguments must be length 1 or {n}, not {.val {sizes}}.",
      call = call
    )
  }

  lapply(args, function(x) if (length(x) == n) x else rep_len(x, n))
}

rbind_rows <- function(x) {
  do_rbind(lapply(x, as.data.frame))
}

do_rbind <- function(x) {
  Reduce(function(a, b) rbind(a, b), x)
}

compact <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

collapse_num <- function(x) {
  paste0(format(x, scientific = FALSE, trim = TRUE), collapse = ",")
}
