# Find Asset ID By Product Function

#' Find Asset ID by Product ID in Account
#'
#' Searches for an asset ID associated with a specific product ID within an account.
#'
#' @param account_id The account ID
#' @param product_id The product ID to search for
#' @param token API authentication token
#'
#' @return A list with:
#' \itemize{
#'   \item \code{found}: Logical indicating if product was found
#'   \item \code{asset_id}: The asset ID if found, NULL otherwise
#'   \item \code{error}: Error message if search failed
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' token <- get_token()
#' result <- find_asset_id_by_product(
#'   account_id = "12345",
#'   product_id = 9599293,
#'   token = token
#' )
#' if (result$found) {
#'   asset_id <- result$asset_id
#' }
#' }
find_asset_id_by_product <- function(account_id, product_id, token) {
    assets_result <- get_account_assets(account_id, token)

    if (!assets_result$success) {
        return(list(found = FALSE, asset_id = NULL, error = assets_result$error))
    }

    assets <- assets_result$data

    for (asset in assets) {
        if (is.list(asset)) {
            asset_product_id <- asset$productId
            asset_id <- asset$id

            if (!is.null(asset_product_id) && asset_product_id == product_id && !is.null(asset_id)) {
                return(list(found = TRUE, asset_id = asset_id, error = NULL))
            }
        }
    }

    return(list(found = FALSE, asset_id = NULL, error = NULL))
}
