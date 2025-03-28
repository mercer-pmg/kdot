#' Calculate the expected average yield for a portfolio
#'
#' @param allocation The asset class allocation of a portfolio.
#'
#' @returns numeric
#' @export
#'
#' @examples
#' x <- "Market Series ETF 070"
#' y <- get_portfolio_allocation(x)
#' expected_yield(y)

expected_yield <- function(allocation){

  Asset <- `Average Yield` <- Weight <- yield_contribution <- NULL

  cma_data <- dplyr::select(cma_return_risk, Asset, `Average Yield`)

  allocation <- dplyr::left_join(allocation, cma_data, by = "Asset")

  yield <- allocation |>
    dplyr::mutate(yield_contribution = Weight*`Average Yield`) |>
    dplyr::summarise(expected_yield = sum(yield_contribution)) |>
    dplyr::pull(expected_yield)

  return(yield)

}
