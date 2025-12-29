# Parse JSON Response Function

#' Parse JSON Response from API Request
#'
#' Parses the JSON response body from an API request result.
#'
#' @param result The result object from \code{orion_request()}
#' @param error_context Character string describing the context for error messages
#' @param simplify_vector Logical indicating whether to simplify JSON vectors (default: TRUE)
#'
#' @return A standardized result list with parsed data or error message
#'
#' @importFrom httr2 resp_body_json resp_body_string
#'
#' @keywords internal
parse_json_response <- function(result, error_context = "parsing response", simplify_vector = TRUE) {
    if (!result$success) {
        return(list(success = FALSE, error = result$error, data = NULL))
    }

    if (is.null(result$response)) {
        return(list(success = FALSE, error = paste("Error", error_context, ": No response object available"), data = NULL))
    }

    tryCatch(
        {
            parsed_data <- httr2::resp_body_json(result$response, simplifyVector = simplify_vector)
            return(list(success = TRUE, error = NULL, data = parsed_data))
        },
        error = function(e) {
            # Try to get raw response for debugging
            raw_response <- tryCatch(
                httr2::resp_body_string(result$response),
                error = function(e2) "[Unable to read response]"
            )
            return(list(
                success = FALSE,
                error = paste("Error", error_context, ":", e$message, "\nRaw response:", substr(raw_response, 1, 200)),
                data = NULL
            ))
        }
    )
}
