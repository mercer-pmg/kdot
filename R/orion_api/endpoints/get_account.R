# Get Account Function

#' Get Account Information
#'
#' Retrieves detailed account information from the Orion API.
#'
#' @param account_id The account ID
#' @param token API authentication token
#' @param expand Optional expansion parameter (e.g., "Sma" for SMA details)
#'
#' @return A standardized result list with account data or error message
#'
#' @export
#'
#' @examples
#' \dontrun{
#' token <- get_token()
#' result <- get_account(account_id = "12345", token = token)
#' if (result$success) {
#'   account_data <- result$data
#' }
#' }
get_account <- function(account_id, token, expand = NULL) {
    params <- if (!is.null(expand)) list(expand = expand) else NULL

    orion_request_json(
        method = "GET",
        endpoint = paste0("/api/v1/Portfolio/Accounts/Verbose/", account_id),
        params = params,
        token = token,
        error_context = paste("Account ID", account_id),
        simplify_vector = TRUE
    )
}
