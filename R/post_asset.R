# Post Asset Function

#' Create Asset in Account
#'
#' Creates a new asset (product) in an account.
#'
#' @param account_id The account ID
#' @param account_number The account number
#' @param product_id The product ID to add as an asset
#' @param token API authentication token
#'
#' @return A list with:
#' \itemize{
#'   \item \code{success}: Logical indicating if operation succeeded
#'   \item \code{error}: Error message if operation failed
#'   \item \code{data}: Response data
#'   \item \code{asset_id}: The created asset ID (if successful)
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' token <- get_token()
#' result <- post_asset(
#'   account_id = "12345",
#'   account_number = "ACC001",
#'   product_id = 9599293,
#'   token = token
#' )
#' if (result$success) {
#'   asset_id <- result$asset_id
#' }
#' }
post_asset <- function(account_id, account_number, product_id, token) {
    payload <- list(
        portfolio = list(
            accountNumber = account_number,
            productId = product_id,
            accountId = as.integer(account_id),
            isManaged = TRUE,
            isActive = TRUE,
            isAdvisorOnly = TRUE,
            status = "Manually Managed",
            currentShares = 0,
            currentValue = 0
        )
    )

    res <- orion_request_json(
        method = "POST",
        endpoint = "/api/v1/Portfolio/Assets/Verbose",
        json_data = payload,
        token = token,
        error_context = paste("Injecting product", product_id, "into account", account_id),
        simplify_vector = TRUE
    )

    if (!res$success) {
        return(list(success = FALSE, error = res$error, data = NULL, asset_id = NULL))
    }

    response_data <- res$data
    asset_id <- as.integer(response_data$id)
    return(list(success = TRUE, error = NULL, data = response_data, asset_id = asset_id))
}
