# Check Account SMA Function

#' Check Account SMA Settings
#'
#' Retrieves and returns the Separately Managed Account (SMA) settings for an account.
#'
#' @param account_id The account ID
#' @param token API authentication token
#'
#' @return A list with:
#' \itemize{
#'   \item \code{account_id}: The account ID
#'   \item \code{account_name}: Account name
#'   \item \code{account_number}: Account number
#'   \item \code{isSMA}: Logical indicating if account is an SMA
#'   \item \code{eclipseSMA}: Eclipse SMA value
#'   \item \code{smaAssetID}: SMA asset ID (if applicable)
#'   \item \code{status}: Status of the operation ("Success", "Error", "Unknown")
#'   \item \code{message}: Status message
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' token <- get_token()
#' result <- check_account_sma(account_id = "12345", token = token)
#' if (result$status == "Success") {
#'   is_sma <- result$isSMA
#'   sma_asset_id <- result$smaAssetID
#' }
#' }
check_account_sma <- function(account_id, token) {
    result <- list(
        account_id = account_id,
        account_name = NULL,
        account_number = NULL,
        isSMA = FALSE,
        eclipseSMA = "None",
        smaAssetID = NULL,
        status = "Unknown",
        message = ""
    )

    account_result <- get_account(account_id, token, expand = "Sma")

    if (!account_result$success) {
        result$status <- "Error"
        result$message <- account_result$error
        return(result)
    }

    account_data <- account_result$data

    result$account_name <- ifelse(is.null(account_data$name), "N/A", account_data$name)
    result$account_number <- account_data$number

    sma_values <- extract_sma_values(account_data)
    result$isSMA <- sma_values$isSMA
    result$eclipseSMA <- sma_values$eclipseSMA
    result$smaAssetID <- sma_values$smaAssetID

    result$status <- "Success"
    result$message <- "SMA settings retrieved successfully"

    return(result)
}
