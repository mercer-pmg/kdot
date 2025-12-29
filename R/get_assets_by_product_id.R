# Get Assets By Product ID Function

#' Get Assets by Product ID
#'
#' Retrieves all assets (across all accounts) for one or more product IDs.
#'
#' @param product_id Single product ID or vector/list of product IDs
#' @param token API authentication token
#' @param return_tibble Logical indicating whether to return results as a tibble (default: TRUE)
#'
#' @return A standardized result list with assets data (as tibble or list) or error message
#'
#' @importFrom dplyr bind_rows
#'
#' @export
#'
#' @examples
#' \dontrun{
#' token <- get_token()
#' # Single product ID
#' result <- get_assets_by_product_id(product_id = 9599293, token = token)
#' if (result$success) {
#'   assets <- result$data
#' }
#'
#' # Multiple product IDs
#' result <- get_assets_by_product_id(
#'   product_id = c(9599293, 1234567),
#'   token = token
#' )
#' }
get_assets_by_product_id <- function(product_id, token, return_tibble = TRUE) {
    # Handle single product ID or vector/list of product IDs
    if (length(product_id) == 1) {
        result <- .get_assets_by_product_id_single(product_id, token)
        if (!result$success) {
            return(standardize_result(FALSE, result$error, NULL))
        }
        
        if (return_tibble && length(result$data) > 0) {
            assets_tibble <- dplyr::bind_rows(result$data)
            return(standardize_result(TRUE, NULL, assets_tibble))
        }
        return(standardize_result(TRUE, NULL, result$data))
    }
    
    # Handle multiple product IDs
    all_assets <- list()
    errors <- list()
    
    for (pid in product_id) {
        result <- .get_assets_by_product_id_single(pid, token)
        if (result$success) {
            all_assets <- c(all_assets, result$data)
        } else {
            errors[[as.character(pid)]] <- result$error
        }
    }
    
    if (length(all_assets) == 0) {
        error_msg <- if (length(errors) > 0) {
            paste("Failed to retrieve assets for all product IDs:", paste(names(errors), collapse = ", "))
        } else {
            "No assets found for any product ID"
        }
        return(standardize_result(FALSE, error_msg, NULL))
    }
    
    if (return_tibble) {
        assets_tibble <- dplyr::bind_rows(all_assets)
        return(standardize_result(TRUE, NULL, assets_tibble))
    }
    
    standardize_result(TRUE, NULL, all_assets)
}

# Helper function to get assets for a single product ID
.get_assets_by_product_id_single <- function(product_id, token) {
    res <- orion_request_json(
        method = "GET",
        endpoint = "/api/v1/Portfolio/Assets",
        params = list(productId = product_id),
        token = token,
        error_context = paste("Loading assets for Product ID", product_id),
        simplify_vector = FALSE
    )

    if (!res$success) {
        return(list(success = FALSE, error = res$error, data = list()))
    }

    list(success = TRUE, error = NULL, data = parse_assets_response(res$data))
}
