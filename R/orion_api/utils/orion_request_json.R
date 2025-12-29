# Orion Request JSON Function

#' Make JSON API Request and Parse Response
#'
#' High-level function that makes an API request and parses the JSON response.
#'
#' @param method HTTP method ("GET", "POST", "PUT", "PATCH", "DELETE")
#' @param endpoint API endpoint path
#' @param token API authentication token
#' @param error_context Character string describing the context for error messages
#' @param params Query parameters (for GET requests)
#' @param json_data JSON data to send in request body (for POST, PUT, PATCH)
#' @param simplify_vector Logical indicating whether to simplify JSON vectors (default: TRUE)
#' @param timeout Request timeout in seconds (default: 30 seconds)
#'
#' @return A standardized result list with parsed data or error message
#'
#' @keywords internal
orion_request_json <- function(method,
                               endpoint,
                               token,
                               error_context = "",
                               params = NULL,
                               json_data = NULL,
                               simplify_vector = TRUE,
                               timeout = NULL) {
    result <- orion_request(
        method = method,
        endpoint = endpoint,
        params = params,
        json_data = json_data,
        token = token,
        error_context = error_context,
        timeout = timeout
    )

    parse_result <- parse_json_response(result, error_context = error_context, simplify_vector = simplify_vector)
    if (!parse_result$success) {
        return(standardize_result(FALSE, parse_result$error, NULL))
    }

    standardize_result(TRUE, NULL, parse_result$data)
}
