#' @include services.R
NULL

#' Tile export job
#'
#' An asynchronous tile export submitted by [export_tiles_job()].
#'
#' @param url String. Service the job was submitted to.
#' @param job_id String. Identifier the service assigned the job.
#' @param token An `httr2_token`, or `NULL`.
#' @returns A `TileExportJob` object.
#' @family tile export
#' @export
TileExportJob <- S7::new_class(
  "TileExportJob",
  package = "arcgistiles",
  properties = list(
    url = s7x::class_string,
    job_id = s7x::class_string,
    token = class_token
  )
)

S7::method(print, TileExportJob) <- function(x, ...) {
  cli::cli_text("{.cls TileExportJob} {.val {x@job_id}}")
  cli::cli_text("{.url {x@url}}")
  cli::cli_text("{.strong Status:} {job_status(x)}")

  invisible(x)
}

#' Submit a tile export job
#'
#' Asks a service to package its cache tiles for offline use. The service must
#' report `export_tiles_allowed`.
#'
#' @param x A [MapServer] or [VectorTileServer].
#' @inheritParams tile_grid
#' @param levels Integer or string. Levels to export, either a vector of level
#'   ids or a range such as `"1-4,7-9"`.
#' @param export_by String or [export_by()]. How `levels` is interpreted.
#' @param tile_package Bool. Export a tile package rather than a cache raster
#'   dataset. Map services only.
#' @param storage_format String or [storage_format()]. `"CompactV2"` yields a
#'   `.tpkx`, `"Compact"` a `.tpk`. Map services only.
#' @param optimize Bool. Recompress JPEG tiles to shrink the download.
#' @param compression_quality Integer. Compression factor between 0 and 100,
#'   used when `optimize` is `TRUE`.
#' @param area_of_interest An `sf` or `sfc` polygon. Supersedes `bbox`.
#' @param ... Additional query parameters passed to the service.
#' @returns A [TileExportJob].
#' @family tile export
#' @export
#' @examples
#' \dontrun{
#' vts <- vector_tile_server(open_street_map_url())
#' job <- export_tiles_job(vts, service_bbox(vts), levels = 0:3)
#' }
export_tiles_job <- function(
  x,
  bbox = NULL,
  levels,
  export_by = "LevelID",
  tile_package = TRUE,
  storage_format = "CompactV2",
  optimize = FALSE,
  compression_quality = NULL,
  area_of_interest = NULL,
  ...,
  error_call = rlang::caller_env()
) {
  check_bool(tile_package, call = error_call)
  check_bool(optimize, call = error_call)

  if (!isTRUE(x@export_tiles_allowed)) {
    cli::cli_abort(
      c(
        "{.val {x@name}} does not allow clients to export tiles.",
        "i" = "Its {.field exportTilesAllowed} property is {.val FALSE}."
      ),
      call = error_call
    )
  }

  vector <- S7::S7_inherits(x, VectorTileServer)

  query <- compact(c(
    list(
      exportExtent = export_extent(bbox, x, error_call),
      levels = collapse_levels(levels, error_call),
      areaOfInterest = if (!vector) aoi_json(area_of_interest, error_call),
      polygon = if (vector) aoi_json(area_of_interest, error_call),
      exportBy = if (!vector) {
        as.character(export_by(as.character(export_by)))
      },
      tilePackage = if (!vector) tolower(tile_package),
      storageFormatType = if (!vector) {
        paste0(
          "esriMapCacheStorageMode",
          as.character(storage_format(as.character(storage_format)))
        )
      },
      optimizeTilesForSize = if (!vector) tolower(optimize),
      compressionQuality = compression_quality,
      f = "json"
    ),
    list(...)
  ))

  submit_tile_job(x, "exportTiles", query, error_call)
}

#' Estimate the size of a tile export
#'
#' Submits the same request as [export_tiles_job()] but returns an estimated
#' download size instead of tiles. Map services only.
#'
#' @inheritParams export_tiles_job
#' @inheritParams job_await
#' @returns A list with the estimated `size` in bytes and `tile_count`.
#' @family tile export
#' @export
#' @examples
#' \dontrun{
#' estimate_export_tiles_size(ms, service_bbox(ms), levels = 0:5)
#' }
estimate_export_tiles_size <- function(
  x,
  bbox = NULL,
  levels,
  export_by = "LevelID",
  tile_package = TRUE,
  area_of_interest = NULL,
  interval = 2,
  timeout = 3600,
  error_call = rlang::caller_env()
) {
  query <- compact(list(
    exportExtent = export_extent(bbox, x, error_call),
    levels = collapse_levels(levels, error_call),
    areaOfInterest = aoi_json(area_of_interest, error_call),
    exportBy = as.character(export_by(as.character(export_by))),
    tilePackage = tolower(tile_package),
    f = "json"
  ))

  job <- submit_tile_job(x, "estimateExportTilesSize", query, error_call)
  job_await(job, interval = interval, timeout = timeout, error_call = error_call)

  info <- job_info(job, error_call)

  if (!is.null(info[["estimatedTilesSize"]])) {
    return(list(
      size = as.double(info[["estimatedTilesSize"]]),
      tile_count = as.double(info[["estimatedNumTiles"]] %||% NA_real_)
    ))
  }

  result <- job_result(job, "out_service_url", error_call = error_call)

  list(
    size = as.double(result[["totalSize"]] %||% NA_real_),
    tile_count = as.double(result[["totalTileCount"]] %||% NA_real_)
  )
}

submit_tile_job <- function(x, operation, query, call = rlang::caller_env()) {
  res <- arcgisutils::fetch_layer_metadata(x@url, x@token, path = operation, query = query, call = call)

  job_id <- res[["jobId"]]

  if (is.null(job_id)) {
    cli::cli_abort(
      "The service did not return a job id.",
      call = call
    )
  }

  TileExportJob(url = x@url, job_id = job_id, token = x@token)
}

#' Tile export job status
#'
#' @param job A [TileExportJob].
#' @inheritParams tile_grid
#' @returns The job's status string, such as `"esriJobSucceeded"`.
#' @family tile export
#' @export
#' @examples
#' \dontrun{
#' job_status(job)
#' }
job_status <- function(job, error_call = rlang::caller_env()) {
  job_info(job, error_call)[["jobStatus"]] %||% NA_character_
}

#' Messages from a tile export job
#'
#' @inheritParams job_await
#' @returns A data frame of job messages, or an empty data frame.
#' @family tile export
#' @export
#' @examples
#' \dontrun{
#' job_messages(job)
#' }
job_messages <- function(job, error_call = rlang::caller_env()) {
  messages <- job_info(job, error_call)[["messages"]]

  if (is.null(messages) || length(messages) == 0L) {
    return(data_frame(data.frame(
      type = character(),
      description = character()
    )))
  }

  if (!is.data.frame(messages)) {
    messages <- rbind_results(messages, call = error_call)
  }

  data_frame(messages, call = error_call)
}

job_info <- function(job, call = rlang::caller_env()) {
  arcgisutils::fetch_layer_metadata(job@url, job@token, path = c("jobs", job@job_id), call = call)
}

#' Wait for a tile export job
#'
#' Polls until the job succeeds, fails, or `timeout` elapses.
#'
#' @inheritParams job_status
#' @param interval Double. Seconds between polls.
#' @param timeout Double. Seconds to wait before giving up.
#' @returns The job, invisibly.
#' @family tile export
#' @export
#' @examples
#' \dontrun{
#' job_await(job)
#' }
job_await <- function(
  job,
  interval = 2,
  timeout = 3600,
  error_call = rlang::caller_env()
) {
  check_number_decimal(interval, min = 0, call = error_call)
  check_number_decimal(timeout, min = 0, call = error_call)

  deadline <- Sys.time() + timeout

  repeat {
    status <- job_status(job, error_call)

    if (identical(status, "esriJobSucceeded")) {
      return(invisible(job))
    }

    if (status %in% c("esriJobFailed", "esriJobTimedOut", "esriJobCancelled")) {
      cli::cli_abort(
        c(
          "Tile export job {.val {job@job_id}} did not succeed.",
          "x" = "Status is {.val {status}}."
        ),
        call = error_call
      )
    }

    if (Sys.time() > deadline) {
      cli::cli_abort(
        "Tile export job {.val {job@job_id}} did not finish within {timeout} seconds.",
        call = error_call
      )
    }

    Sys.sleep(interval)
  }
}

#' Result of a tile export job
#'
#' Reads the download URL from the finished job. Vector tile services put it on
#' the job resource, map services expose it as a named result parameter.
#'
#' @inheritParams job_await
#' @param param String. Name of the result parameter to read when the job
#'   resource carries no output URL.
#' @returns The result value, usually a URL string.
#' @family tile export
#' @export
#' @examples
#' \dontrun{
#' job_result(job)
#' }
job_result <- function(
  job,
  param = "out_service_url",
  error_call = rlang::caller_env()
) {
  check_string(param, allow_empty = FALSE, call = error_call)

  output <- job_info(job, error_call)[["output"]][["outputUrl"]]

  if (!is.null(output)) {
    return(output)
  }

  res <- arcgisutils::fetch_layer_metadata(
    job@url,
    job@token,
    path = c("exportTiles", "jobs", job@job_id, "results", param),
    call = error_call
  )

  res[["value"]]
}

#' Download an exported tile package
#'
#' Waits for the job if it has not finished, then writes the tile package to
#' disk.
#'
#' @inheritParams job_await
#' @param file String. Where to write the package. Defaults to a temporary
#'   file named after the job.
#' @returns The path to the downloaded file.
#' @family tile export
#' @export
#' @examples
#' \dontrun{
#' write_tile_package(job, "world.tpkx")
#' }
write_tile_package <- function(
  job,
  file = NULL,
  interval = 2,
  timeout = 3600,
  error_call = rlang::caller_env()
) {
  check_string(file, allow_null = TRUE, allow_empty = FALSE, call = error_call)

  job_await(job, interval = interval, timeout = timeout, error_call = error_call)
  url <- job_result(job, error_call = error_call)

  if (!is.character(url) || length(url) != 1L) {
    cli::cli_abort(
      "The job result is not a downloadable URL.",
      call = error_call
    )
  }

  # a signed download URL carries a long query string, so the extension has to
  # come from the path alone
  ext <- tools::file_ext(httr2::url_parse(url)[["path"]] %||% "")
  file <- file %||% tempfile(fileext = if (nzchar(ext)) paste0(".", ext) else ".tpkx")

  arcgisutils::arc_base_req(url, job@token, error_call = error_call) |>
    httr2::req_perform(path = file, error_call = error_call)

  file
}

#' Export tiles to a tile package
#'
#' Submits a tile export, waits for it, and downloads the result.
#'
#' @inheritParams export_tiles_job
#' @inheritParams write_tile_package
#' @returns The path to the downloaded tile package.
#' @family tile export
#' @export
#' @examples
#' \dontrun{
#' export_tiles(vts, service_bbox(vts), levels = 0:3, file = "world.vtpk")
#' }
export_tiles <- function(
  x,
  bbox = NULL,
  levels,
  file = NULL,
  interval = 2,
  timeout = 3600,
  ...,
  error_call = rlang::caller_env()
) {
  job <- export_tiles_job(
    x,
    bbox = bbox,
    levels = levels,
    ...,
    error_call = error_call
  )

  write_tile_package(
    job,
    file = file,
    interval = interval,
    timeout = timeout,
    error_call = error_call
  )
}

# the envelope form carries the spatial reference, which the comma separated
# form cannot
export_extent <- function(bbox, x, call = rlang::caller_env()) {
  if (is.null(bbox)) {
    return(NULL)
  }

  bbox <- as_bbox(bbox, x@crs, error_call = call)

  yyjsonr::write_json_str(
    arcgisutils::as_extent(sf::st_bbox(bbox), call = call),
    auto_unbox = TRUE
  )
}

collapse_levels <- function(levels, call = rlang::caller_env()) {
  if (is.character(levels)) {
    check_string(levels, allow_empty = FALSE, call = call)
    return(levels)
  }

  if (!is.numeric(levels) || anyNA(levels)) {
    cli::cli_abort(
      "{.arg levels} must be a numeric vector or a range like {.val 1-4,7-9}.",
      call = call
    )
  }

  toString(as.integer(levels))
}

aoi_json <- function(area_of_interest, call = rlang::caller_env()) {
  if (is.null(area_of_interest)) {
    return(NULL)
  }

  arcgisutils::as_esri_geometry(sf::st_union(sf::st_geometry(area_of_interest))[[1L]])
}
