# Get Eclipse Token Function

#' Get Eclipse token from Orion Connect token
#'
#' Exchanges an Orion Connect token for an Eclipse JWT. Use the returned token
#' for Eclipse API calls (e.g., ESG Themes, trading).
#'
#' @param orion_token Orion Connect token from \code{\link{get_token}} or Basic Auth/OAuth.
#' @param auth_scheme Authorization scheme: \code{"Session"} (default), \code{"Bearer"}, or \code{""} for raw token.
#'   Override with \code{MA_ORION_AUTH_SCHEME} env var.
#' @param base_url Eclipse API base URL (default: \code{https://api.orioneclipse.com}).
#'
#' @return Character string containing the Eclipse JWT (\code{eclipse_access_token}).
#'
#' @importFrom httr2 request req_method req_headers req_error req_perform resp_status resp_body_string resp_body_json
#' @export
#'
#' @examples
#' \dontrun{
#' orion_token <- get_token()
#' eclipse_token <- get_eclipse_token(orion_token)
#' }
get_eclipse_token <- function(orion_token,
                              auth_scheme = Sys.getenv("MA_ORION_AUTH_SCHEME", "Session"),
                              base_url = "https://api.orioneclipse.com") {
  orion_token <- trimws(orion_token)
  url <- paste0(base_url, "/v1/admin/token")
  auth_val <- if (nzchar(trimws(auth_scheme))) {
    paste(trimws(auth_scheme), orion_token)
  } else {
    orion_token
  }

  req <- httr2::request(url) |>
    httr2::req_method("GET") |>
    httr2::req_headers(
      Authorization = auth_val,
      Accept = "application/json"
    ) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      stop("Eclipse token request failed: ", e$message)
    }
  )

  status <- httr2::resp_status(resp)
  if (status != 200) {
    err_body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
    hint <- ""
    if (grepl("401|Unauthorized", err_body, ignore.case = TRUE)) {
      hint <- "\nTip: Orion Connect token may be expired (~10h). Obtain a fresh token via Basic Auth or OAuth."
    }
    stop(sprintf("Eclipse token error %d: %s%s", status, err_body, hint))
  }

  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  token <- body$eclipse_access_token
  if (is.null(token) || !nzchar(trimws(token))) {
    stop(
      "Eclipse token response missing eclipse_access_token. Keys: ",
      paste(names(body), collapse = ", ")
    )
  }
  as.character(token)
}
