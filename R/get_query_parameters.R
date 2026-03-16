# Get Query Parameters Function

#' Get Query Parameters for Custom Reporting Query
#'
#' Retrieves the query definition and parameters for a specified query ID.
#'
#' @param query_id The query ID
#' @param token API token (optional, will use \code{get_token()} if not provided)
#'
#' @return A standardized result list with success status, error message (if any), and query data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- get_query_parameters(query_id = 40790)
#' if (result$success) {
#'   query_data <- result$data
#' }
#' }
get_query_parameters <- function(query_id, token = NULL) {
    if (is.null(token)) {
        token <- get_token()
        if (is.null(token)) {
            return(standardize_result(FALSE, "API token not found", NULL))
        }
    }

    result <- orion_request_json(
        method = "GET",
        endpoint = paste0("/api/v1/Reporting/Custom/", query_id),
        token = token,
        error_context = paste("Fetching parameters for Query", query_id),
        simplify_vector = TRUE
    )

    return(result)
}
