# Get Token Function

#' Get Orion API Token
#'
#' Retrieves the Orion API token from function input or Windows User registry.
#'
#' @param token_input Optional token string. If provided and valid, returns this token.
#'   If the token matches the masked value (all asterisks), falls back to registry/env.
#'
#' @return Character string containing the API token, or NULL if not found.
#'
#' @details
#' This function checks for a token in the following order:
#' \itemize{
#'   \item Function input parameter (if provided and not masked)
#'   \item On Windows: \code{MA_ORION_API_TOKEN} from User registry (current value, not inherited)
#'   \item On other OS: environment variable \code{MA_ORION_API_TOKEN}
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
        masked_value <- paste(rep("*", 200L), collapse = "")
        if (nzchar(token_trimmed) && token_trimmed != masked_value) {
            return(token_trimmed)
        }
    }

    # On Windows, read from User registry (current value, not inherited from parent process)
    if (.Platform$OS.type == "windows") {
      reg_token <- tryCatch({
        out <- system2("powershell",
          args = c("-NoProfile", "-Command",
            "[Environment]::GetEnvironmentVariable('MA_ORION_API_TOKEN','User')"),
          stdout = TRUE, stderr = FALSE)
        trimws(out[1])
      }, error = function(e) character(0))
      if (length(reg_token) > 0 && nzchar(reg_token)) return(reg_token)
      return(NULL)
    }

    # Non-Windows: use environment variable
    env_token <- Sys.getenv("MA_ORION_API_TOKEN")
    if (nzchar(env_token)) env_token else NULL
}

#' Get Token Mask Length
#'
#' Returns the length of asterisks used to mask API tokens in UI displays.
#'
#' @return Integer value (200) representing the token mask length
#'
#' @export
#'
#' @examples
#' get_token_mask_length()
get_token_mask_length <- function() {
  200L
}
