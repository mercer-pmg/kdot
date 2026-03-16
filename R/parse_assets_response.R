# Parse Assets Response Function

#' Parse Assets Response Data
#'
#' Converts assets response data to a consistent list format.
#'
#' @param assets_data The assets data to parse (data.frame or list)
#'
#' @return A list of asset objects
#'
#' @keywords internal
parse_assets_response <- function(assets_data) {
    if (is.data.frame(assets_data)) {
        lapply(1:nrow(assets_data), function(i) as.list(assets_data[i, ]))
    } else if (is.list(assets_data)) {
        assets_data
    } else {
        list()
    }
}
