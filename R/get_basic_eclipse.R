# Get Basic Eclipse Function

#' Generic GET request to Orion Eclipse API
#'
#' Performs a GET request to any Eclipse API endpoint using the Eclipse JWT.
#'
#' @param url Full Eclipse API URL (e.g. \code{https://api.orioneclipse.com/api/v2/...}).
#' @param eclipse_token JWT from \code{\link{get_eclipse_token}}.
#'
#' @return Parsed JSON response (data frame or list, depending on endpoint).
#'
#' @importFrom httr2 request req_method req_headers req_error req_perform resp_status resp_body_string resp_body_json
#' @export
#'
#' @examples
#' \dontrun{
#' eclipse_token <- get_eclipse_token(get_token())
#' data <- get_basic_eclipse("https://api.orioneclipse.com/api/v2/...", eclipse_token)
#' }
get_basic_eclipse <- function(url, eclipse_token) {
  req <- httr2::request(url) |>
    httr2::req_method("GET") |>
    httr2::req_headers(
      Authorization = paste("Bearer", trimws(eclipse_token)),
      Accept = "application/json"
    ) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      stop("Eclipse GET request failed: ", e$message)
    }
  )

  status <- httr2::resp_status(resp)
  if (status != 200) {
    err_body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
    stop(sprintf("Eclipse API error %d: %s", status, err_body))
  }

  httr2::resp_body_json(resp, simplifyVector = TRUE)
}
