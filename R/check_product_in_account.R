# Check Product In Account Function

#' Check if Product Exists in Account
#'
#' Checks if a product (by product ID) exists as an asset in an account.
#' This is a backward-compatible wrapper around \code{find_asset_id_by_product()}.
#'
#' @param account_id The account ID
#' @param product_id The product ID to check
#' @param token API authentication token
#'
#' @return A list with:
#' \itemize{
#'   \item \code{exists}: Logical indicating if product exists in account
#'   \item \code{asset_id}: The asset ID if found, NULL otherwise
#'   \item \code{error}: Error message if check failed
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' token <- get_token()
#' result <- check_product_in_account(
#'   account_id = "12345",
#'   product_id = 9599293,
#'   token = token
#' )
#' if (result$exists) {
#'   asset_id <- result$asset_id
#' }
#' }
check_product_in_account <- function(account_id, product_id, token) {
    res <- find_asset_id_by_product(account_id, product_id, token)
    list(exists = res$found, asset_id = res$asset_id, error = res$error)
}
