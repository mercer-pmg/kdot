# Get Accounts By Number Function

#' Search Accounts by Account Number
#'
#' Searches for accounts by account number.
#'
#' @param account_number The account number to search for
#' @param token API authentication token
#' @param exact_match Logical indicating whether to require exact match (default: FALSE)
#'
#' @return A standardized result list with matching accounts data or error message
#'
#' @export
#'
#' @examples
#' \dontrun{
#' token <- get_token()
#' result <- get_accounts_by_number(account_number = "12345", token = token)
#' if (result$success) {
#'   accounts <- result$data
#' }
#' }
get_accounts_by_number <- function(account_number, token, exact_match = FALSE) {
    res <- orion_request_json(
        method = "GET",
        endpoint = paste0("/api/v1/Portfolio/Accounts/Simple/Search/Number/", account_number),
        params = list(exactMatch = exact_match),
        token = token,
        error_context = paste("Searching for account number", account_number),
        simplify_vector = TRUE
    )

    if (!res$success) {
        return(standardize_result(FALSE, res$error, list()))
    }

    accounts_data <- res$data
    # Handle both list and single dict responses
    if (is.data.frame(accounts_data)) {
        accounts_list <- lapply(1:nrow(accounts_data), function(i) as.list(accounts_data[i, ]))
    } else if (is.list(accounts_data) && !is.null(accounts_data$id)) {
        accounts_list <- list(accounts_data)
    } else {
        accounts_list <- accounts_data
    }
    standardize_result(TRUE, NULL, accounts_list)
}
