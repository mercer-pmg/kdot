# Standardize Result Function

#' Standardize API Result Format
#'
#' Creates a standardized result list for API function returns.
#'
#' @param success Logical indicating if the operation was successful
#' @param error Character string with error message (NULL if successful)
#' @param data The data returned from the operation (NULL if error)
#'
#' @return A list with \code{success}, \code{error}, and \code{data} fields
#'
#' @keywords internal
standardize_result <- function(success, error = NULL, data = NULL) {
    list(success = success, error = error, data = data)
}
