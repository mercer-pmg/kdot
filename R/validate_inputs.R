# Validate Inputs Function

#' Validate Account ID and Token Inputs
#'
#' Validates account ID format and token presence for API requests.
#'
#' @param account_id The account ID to validate
#' @param token The API token to validate
#' @param account_id_label Optional label for error messages (default: "Account ID")
#'
#' @return A list with:
#' \itemize{
#'   \item \code{valid}: Logical indicating if validation passed
#'   \item \code{error}: Error message if validation failed
#'   \item \code{account_id}: Validated account ID (if valid)
#'   \item \code{token}: Validated token (if valid)
#' }
#'
#' @export
validate_inputs <- function(account_id, token, account_id_label = "Account ID") {
    if (is.null(account_id) || length(account_id) == 0) {
        return(list(valid = FALSE, error = paste(account_id_label, "cannot be NULL or empty")))
    }

    account_id <- trimws(as.character(account_id))

    if (account_id == "" || !grepl("^[0-9]+$", account_id)) {
        return(list(valid = FALSE, error = paste("Please enter a valid", account_id_label)))
    }

    if (is.null(token) || length(token) == 0 || trimws(as.character(token)) == "") {
        return(list(valid = FALSE, error = "API token not found"))
    }

    return(list(valid = TRUE, account_id = account_id, token = trimws(as.character(token))))
}
