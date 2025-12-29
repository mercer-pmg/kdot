# Get Token Function

#' Get Orion API Token
#'
#' Retrieves the Orion API token from function input or environment variable.
#'
#' @param token_input Optional token string. If provided and valid, returns this token.
#'   If the token matches the masked value (all asterisks), falls back to environment variable.
#'
#' @return Character string containing the API token, or NULL if not found.
#'
#' @details
#' This function checks for a token in the following order:
#' \itemize{
#'   \item Function input parameter (if provided and not masked)
#'   \item Environment variable \code{MA_ORION_API_TOKEN}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' token <- get_token()
#' # Or provide token directly
#' token <- get_token("your-token-here")
#' }
get_token <- function(token_input = NULL) {
    # Check input token first
    if (!is.null(token_input)) {
        token_trimmed <- trimws(token_input)
        masked_value <- paste(rep("*", TOKEN_MASK_LENGTH), collapse = "")
        if (nzchar(token_trimmed) && token_trimmed != masked_value) {
            return(token_trimmed)
        }
    }

    # Fall back to environment variable
    env_token <- Sys.getenv("MA_ORION_API_TOKEN")
    if (nzchar(env_token)) env_token else NULL
}
