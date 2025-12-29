# Put Account SMA Function

#' Update Account SMA Settings
#'
#' Updates the Separately Managed Account (SMA) settings for an account.
#'
#' @param account_id The account ID
#' @param is_sma Logical indicating if account should be marked as SMA
#' @param sma_asset_id The asset ID to use for SMA (can be NULL)
#' @param token API authentication token
#' @param eclipse_sma Character string for Eclipse SMA setting (default: "False")
#'
#' @return A standardized result list indicating success or error
#'
#' @export
#'
#' @examples
#' \dontrun{
#' token <- get_token()
#' result <- put_account_sma(
#'   account_id = "12345",
#'   is_sma = TRUE,
#'   sma_asset_id = 67890,
#'   token = token
#' )
#' if (result$success) {
#'   message("SMA settings updated successfully")
#' }
#' }
put_account_sma <- function(account_id, is_sma, sma_asset_id, token, eclipse_sma = "False") {
    sma_payload <- list(
        id = NULL,
        modelingInfo = NULL,
        sma = list(
            isSma = is_sma,
            eclipseSMA = eclipse_sma,
            smaAssetId = sma_asset_id
        )
    )

    result <- orion_request(
        method = "PUT",
        endpoint = paste0("/api/v1/Portfolio/Accounts/UpdateDoNotTradeAndSma/", account_id),
        json_data = sma_payload,
        token = token,
        error_context = paste("Updating SMA settings for Account ID", account_id)
    )

    standardize_result(result$success, if (!result$success) result$error else NULL, NULL)
}
