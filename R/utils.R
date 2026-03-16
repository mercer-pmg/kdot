# Internal utilities (not exported)

#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x
