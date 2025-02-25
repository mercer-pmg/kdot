#' Calculate the expected return for a portfolio
#'
#' @param allocation The asset allocation of a portfolio
#'
#' @return numeric
#' @export
#'
#' @examples
#' x <- "Market Series ETF 070"
#' y <- get_portfolio_allocation(x)
#' expected_return <- expected_return(y)
#'
expected_return <- function(allocation){

  Asset <- Weight <- `Arithmetic Return` <- return_contribution <- NULL

  cma_data <- dplyr::select(cma_return_risk, Asset, `Arithmetic Return`)

  allocation <- allocation |> dplyr::left_join(cma_data, by = "Asset")

  portfolio_expected_return <- allocation |>
    dplyr::mutate(return_contribution = Weight*`Arithmetic Return`) |>
    dplyr::summarise(expected_return = sum(return_contribution)) |>
    dplyr::pull(expected_return)

  return(portfolio_expected_return)

}
