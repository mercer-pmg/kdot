# Refresh Reporting Data Cache and Wait for Update

#' Refresh Reporting Data Cache
#'
#' POSTs to the Reporting/Data/Cache/Refresh endpoint to trigger a cache rebuild
#' for the given date range. The cache update runs asynchronously; use
#' \code{\link{wait_for_cache_refresh}} to poll until the data has changed.
#'
#' @param start_date Start date in ISO format (e.g., "2025-12-01")
#' @param end_date End date in ISO format (e.g., "2026-01-31")
#' @param token API authentication token (default: get_token())
#'
#' @return Invisible TRUE on success
#' @export
refresh_reporting_cache <- function(start_date, end_date, token = get_token()) {
  payload <- list(
    dateRange = list(
      `$type` = "date-range",
      startDate = start_date,
      endDate = end_date
    )
  )

  result <- orion_request(
    method = "POST",
    endpoint = "/api/v1/Reporting/Data/Cache/Refresh",
    json_data = payload,
    token = token,
    error_context = "Refreshing reporting cache",
    timeout = 60L
  )

  if (result$success || identical(result$status_code, 202L)) {
    return(invisible(TRUE))
  }
  stop("Cache refresh request failed: ", result$error)
}

#' Wait for Cache Refresh to Complete
#'
#' Polls the Homepage/Performance endpoint every \code{interval_min} minutes until
#' the returned data differs from the baseline, or until \code{max_wait_min}
#' elapses. Call this after \code{\link{refresh_reporting_cache}}; capture the
#' baseline (from \code{\link{fetch_homepage_performance}} and
#' \code{\link{build_database_performance_table}} / \code{\link{build_benchmark_performance_table}})
#' before posting the refresh request.
#'
#' @param token API authentication token (default: get_token())
#' @param baseline_db Baseline database performance table (from
#'   \code{build_database_performance_table})
#' @param baseline_bm Baseline benchmark performance table (from
#'   \code{build_benchmark_performance_table})
#' @param interval_min Minutes between polls (default: 3)
#' @param max_wait_min Maximum minutes to wait before timing out (default: 120)
#'
#' @return TRUE if data changed (cache updated), FALSE if timeout
#' @export
wait_for_cache_refresh <- function(
  token = get_token(),
  baseline_db,
  baseline_bm,
  interval_min = 3L,
  max_wait_min = 120L
) {
  interval_sec <- interval_min * 60L
  max_wait_sec <- max_wait_min * 60L
  start_time <- Sys.time()
  poll_num <- 0L

  while (TRUE) {
    poll_num <- poll_num + 1L
    elapsed_sec <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    if (elapsed_sec >= max_wait_sec) {
      message(
        "[",
        format(Sys.time(), "%H:%M"),
        "] Poll ",
        poll_num,
        " - timeout after ",
        round(elapsed_sec / 60, 1),
        " min"
      )
      return(FALSE)
    }

    if (poll_num > 1L) {
      Sys.sleep(interval_sec)
    }

    result <- tryCatch(
      fetch_homepage_performance(token = token),
      error = function(e) {
        message(
          "[",
          format(Sys.time(), "%H:%M"),
          "] Poll ",
          poll_num,
          " - fetch error: ",
          e$message
        )
        return(NULL)
      }
    )
    if (is.null(result)) {
      next
    }

    current_db <- build_database_performance_table(result)
    current_bm <- build_benchmark_performance_table(result)

    db_changed <- !isTRUE(all.equal(
      as.data.frame(current_db),
      as.data.frame(baseline_db),
      check.attributes = FALSE
    ))
    bm_changed <- !isTRUE(all.equal(
      as.data.frame(current_bm),
      as.data.frame(baseline_bm),
      check.attributes = FALSE
    ))

    if (db_changed || bm_changed) {
      message(
        "[",
        format(Sys.time(), "%H:%M"),
        "] Poll ",
        poll_num,
        " - data changed, cache updated"
      )
      return(TRUE)
    }

    message(
      "[",
      format(Sys.time(), "%H:%M"),
      "] Poll ",
      poll_num,
      " - no change, waiting ",
      interval_min,
      " min..."
    )
  }
}

#' Refresh Homepage Cache and Wait for Update
#'
#' Full flow: store baseline, POST refresh, poll until data changes. All messages
#' print to console. Call once for a clean run.
#'
#' @param start_date Start date in ISO format (e.g., "2025-02-26")
#' @param end_date End date in ISO format (e.g., "2026-02-26")
#' @param token API authentication token (default: get_token())
#' @param interval_min Minutes between polls (default: 3)
#' @param max_wait_min Maximum minutes to wait (default: 120)
#'
#' @return TRUE if cache updated, FALSE if timeout
#' @export
refresh_homepage_cache_and_wait <- function(
  start_date,
  end_date,
  token = get_token(),
  interval_min = 3L,
  max_wait_min = 120L
) {
  message("[", format(Sys.time(), "%H:%M"), "] Fetching baseline...")
  result <- fetch_homepage_performance(token = token)
  baseline_db <- build_database_performance_table(result)
  baseline_bm <- build_benchmark_performance_table(result)
  message(
    "[",
    format(Sys.time(), "%H:%M"),
    "] Baseline stored (",
    nrow(baseline_db),
    " db rows, ",
    nrow(baseline_bm),
    " bm rows)"
  )

  message(
    "[",
    format(Sys.time(), "%H:%M"),
    "] POSTing cache refresh for ",
    start_date,
    " to ",
    end_date,
    "..."
  )
  refresh_reporting_cache(start_date, end_date, token)
  message("[", format(Sys.time(), "%H:%M"), "] Refresh request sent")

  updated <- wait_for_cache_refresh(
    token = token,
    baseline_db = baseline_db,
    baseline_bm = baseline_bm,
    interval_min = interval_min,
    max_wait_min = max_wait_min
  )

  if (updated) {
    message("[", format(Sys.time(), "%H:%M"), "] Cache updated.")
  } else {
    message(
      "[",
      format(Sys.time(), "%H:%M"),
      "] Cache did not update within ",
      max_wait_min,
      " min."
    )
  }
  invisible(updated)
}
