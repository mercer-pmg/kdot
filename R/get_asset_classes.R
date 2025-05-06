#' Get a list of CMA asset classes
#'
#' @returns a character vector
#' @export
#'
#'
get_asset_classes <- function(){

  Asset <- NULL

  asset_classes <- dplyr::pull(cma_return_risk, Asset)

  return(asset_classes)
}
