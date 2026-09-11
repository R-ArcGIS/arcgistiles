#' @importFrom arcgisutils compact data_frame rbind_results
NULL

# NULL leads each union so the generated constructor defaults to NULL rather
# than to an S3 class that has no constructor to deparse
class_bbox <- S7::new_union(NULL, S7::new_S3_class("bbox"))
class_token <- S7::new_union(NULL, S7::new_S3_class("httr2_token"))

# S7 would otherwise default a data.frame property to an undeparsable
# constructor call, which R CMD check reports as a codoc mismatch
class_data_frame <- S7::new_property(
  S7::class_data.frame,
  default = quote(data.frame())
)

#' @importFrom sf st_bbox
as_bbox <- function(x, crs = NULL, call = rlang::caller_env()) {
  if (!inherits(x, "bbox")) {
    if (!is.numeric(x)) {
      x <- rlang::try_fetch(
        sf::st_bbox(x),
        error = function(cnd) {
          cli::cli_abort(
            "{.arg bbox} must be a {.cls bbox} or a length four numeric.",
            call = call
          )
        }
      )
    } else if (length(x) != 4L) {
      cli::cli_abort(
        "{.arg bbox} must be a {.cls bbox} or a length four numeric.",
        call = call
      )
    } else {
      x <- sf::st_bbox(
        stats::setNames(as.double(x), c("xmin", "ymin", "xmax", "ymax")),
        crs = crs
      )
    }
  }

  if (is.null(crs) || is.na(crs)) {
    return(x)
  }

  if (is.na(sf::st_crs(x))) {
    return(sf::st_bbox(stats::setNames(as.double(x), names(x)), crs = crs))
  }

  if (sf::st_crs(x) == crs) {
    return(x)
  }

  sf::st_bbox(sf::st_transform(sf::st_as_sfc(x), crs))
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


arc_get <- function(
  url,
  token = NULL,
  path = NULL,
  query = NULL,
  call = rlang::caller_env()
) {
  resp <- arcgisutils::arc_base_req(
    url,
    token,
    path = as.character(path),
    query = c(query, list(f = "json")),
    error_call = call
  ) |>
    httr2::req_perform(error_call = call)

  arcgisutils::detect_errors(
    RcppSimdJson::fparse(httr2::resp_body_string(resp)),
    error_call = call
  )
}
