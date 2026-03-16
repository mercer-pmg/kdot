# Fetch Homepage Performance from Orion API

#' Fetch Homepage Performance from Orion API
#'
#' POSTs to the Homepage/Performance endpoint and returns the raw API response.
#' Use \code{\link{build_database_performance_table}} to convert the result to a tibble.
#'
#' @param token API authentication token (default: get_token())
#'
#' @return Raw API response from the Homepage/Performance endpoint
#' @export
fetch_homepage_performance <- function(token = get_token()) {
  payload <- list(
    repIds = integer(),
    benchmark = list(entity = "", entityId = 0L)
  )

  res <- orion_request_json(
    method = "POST",
    endpoint = "/api/v1/Homepage/Performance",
    json_data = payload,
    token = token,
    error_context = "Fetching homepage performance",
    simplify_vector = TRUE
  )

  if (!res$success) {
    stop("API request failed: ", res$error)
  }

  res
}

#' Fetch Homepage Performance and Return Table
#'
#' Convenience function that fetches from the Homepage/Performance endpoint
#' and returns a flattened tibble. Equivalent to
#' \code{build_database_performance_table(fetch_homepage_performance(token))}.
#'
#' @param token API authentication token (default: get_token())
#'
#' @return Tibble with columns: level, type, startDate, endDate, performance
#' @export
post_homepage_performance <- function(token = get_token()) {
  result <- fetch_homepage_performance(token = token)
  build_database_performance_table(result)
}
