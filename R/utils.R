#' @importFrom arcgisutils as_bbox compact data_frame rbind_results
#' @importFrom wk rct rct_xmax rct_xmin rct_ymax rct_ymin wk_crs
NULL

# NULL leads each union so the generated constructor defaults to NULL rather
# than to an S3 class that has no constructor to deparse
class_rct <- S7::new_union(NULL, S7::new_S3_class(c("wk_rct", "wk_rcrd")))
class_token <- S7::new_union(NULL, S7::new_S3_class("httr2_token"))

# S7 would otherwise default a data.frame property to an undeparsable
# constructor call, which R CMD check reports as a codoc mismatch
class_data_frame <- S7::new_property(
  S7::class_data.frame,
  default = quote(data.frame())
)

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

crs_label <- function(crs) {
  if (is.null(crs) || all(is.na(crs))) {
    return("unknown")
  }

  sf::st_crs(crs)$input %||% format(crs)
}
