# Process Single Account Product Function

#' Process Product in Account
#'
#' Complete workflow to add a product to an account and configure SMA settings.
#' This function:
#' \itemize{
#'   \item Fetches account information
#'   \item Checks if product already exists in account
#'   \item Creates asset if it doesn't exist
#'   \item Updates SMA settings
#' }
#'
#' @param account_id The account ID
#' @param product_id The product ID to add to the account
#' @param token API authentication token
#'
#' @return A list with:
#' \itemize{
#'   \item \code{account_id}: The account ID
#'   \item \code{product_id}: The product ID
#'   \item \code{status}: Status of the operation ("Success", "Error", "Partial Success", etc.)
#'   \item \code{message}: Detailed status message
#'   \item \code{asset_id}: The asset ID (if successful)
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' token <- get_token()
#' result <- process_single_account_product(
#'   account_id = "12345",
#'   product_id = 9599293,
#'   token = token
#' )
#' if (result$status == "Success") {
#'   message(result$message)
#' }
#' }
process_single_account_product <- function(account_id, product_id, token) {
    result <- list(
        account_id = account_id,
        product_id = product_id,
        status = "Unknown",
        message = "",
        asset_id = NULL
    )

    # Step 1: Fetch account data to get account number
    account_result <- get_account(account_id, token, expand = "Sma")

    if (!account_result$success) {
        result$status <- "Error"
        result$message <- paste("Failed to fetch account:", account_result$error)
        return(result)
    }

    account_data <- account_result$data

    # Extract account number
    account_number <- account_data$number

    if (is.null(account_number)) {
        result$status <- "Error"
        result$message <- "Account number not found in account data"
        return(result)
    }

    # Step 2: Check if product already exists in account
    check_result <- find_asset_id_by_product(account_id, product_id, token)

    if (!is.null(check_result$error) && check_result$error != "") {
        result$status <- "Error"
        result$message <- paste("Error checking assets:", check_result$error)
        return(result)
    }

    asset_id <- NULL

    if (check_result$found) {
        # Product already exists, use existing asset ID
        asset_id <- check_result$asset_id
        result$status <- "Skipped (already exists)"
        result$message <- paste("Product already exists in account with Asset ID:", asset_id)
    } else {
        # Step 3: Inject asset
        inject_result <- post_asset(account_id, account_number, product_id, token)

        if (!inject_result$success) {
            result$status <- "Error"
            result$message <- paste("Failed to inject asset:", inject_result$error)
            return(result)
        }

        asset_id <- inject_result$asset_id

        if (is.null(asset_id)) {
            result$status <- "Error"
            result$message <- "Asset injected but Asset ID not found in response"
            return(result)
        }

        result$status <- "Injected"
        result$message <- paste("Asset injected successfully, Asset ID:", asset_id)
    }

    result$asset_id <- asset_id

    # Step 4: Update SMA settings
    sma_result <- put_account_sma(account_id, TRUE, asset_id, token)

    if (!sma_result$success) {
        result$status <- "Partial Success"
        result$message <- paste0(result$message, " | SMA update failed: ", sma_result$error)
        return(result)
    }

    if (result$status == "Skipped (already exists)") {
        result$status <- "Success (already existed)"
    } else {
        result$status <- "Success"
    }
    result$message <- paste0(result$message, " | SMA settings updated successfully")

    return(result)
}
