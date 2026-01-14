# Check Account SMA Function

#' Check Account SMA Settings
#'
#' Checks the Separately Managed Account (SMA) settings for an account.
#'
#' @param account_id The account ID
#' @param token API authentication token
#'
#' @return A list with:
#' \itemize{
#'   \item \code{account_id}: The account ID
#'   \item \code{account_number}: The account number
#'   \item \code{account_name}: The account name
#'   \item \code{isSMA}: Logical indicating if account is an SMA
#'   \item \code{eclipseSMA}: Character string with Eclipse SMA value
#'   \item \code{smaAssetID}: Integer asset ID for SMA (NULL if not applicable)
#'   \item \code{status}: Status message ("Success" or "Error")
#'   \item \code{message}: Detailed message
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' token <- get_token()
#' result <- check_account_sma(
#'   account_id = "12345",
#'   token = token
#' )
#' if (result$status == "Success") {
#'   cat("Account is SMA:", result$isSMA, "\n")
#' }
#' }
check_account_sma <- function(account_id, token) {
    # Get account data with SMA expansion
    account_result <- get_account(account_id, token, expand = "Sma")

    if (!account_result$success) {
        return(list(
            account_id = account_id,
            account_number = NULL,
            account_name = NULL,
            isSMA = FALSE,
            eclipseSMA = "False",
            smaAssetID = NULL,
            status = "Error",
            message = account_result$error
        ))
    }

    account_data <- account_result$data

    # Extract SMA values
    sma_values <- extract_sma_values(account_data)

    # Extract account details
    account_number <- if (!is.null(account_data$accountNumber)) as.character(account_data$accountNumber) else NULL
    account_name <- if (!is.null(account_data$name)) as.character(account_data$name) else NULL

    # Build status message
    status_msg <- paste0(
        "isSMA: ", sma_values$isSMA,
        ", eclipseSMA: ", sma_values$eclipseSMA,
        ifelse(!is.null(sma_values$smaAssetID), paste0(", smaAssetID: ", sma_values$smaAssetID), ", smaAssetID: None")
    )

    list(
        account_id = account_id,
        account_number = account_number,
        account_name = account_name,
        isSMA = sma_values$isSMA,
        eclipseSMA = sma_values$eclipseSMA,
        smaAssetID = sma_values$smaAssetID,
        status = "Success",
        message = status_msg
    )
}
