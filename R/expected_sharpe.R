#' Calculate the expected Sharpe Raio of a portfolio
#'
#' @param allocation The asset allocation of a portfolio.
#'
#' @return a numeric vector.
#' @export
#'
#' @examples
#' x <- get_portfolio_allocation("Multifactor Series ETF 080")
#' sharpe <- expected_sharpe(x)
#'
expected_sharpe <- function(allocation){

  Asset <- `Arithmetic Return` <- NULL

  exp_ret <- expected_return(allocation)
  exp_risk <- expected_risk(allocation)
  rfr <- cma_return_risk |> dplyr::filter(Asset == "U.S. Cash") |> dplyr::pull(`Arithmetic Return`)

  exp_sharpe <- (exp_ret - rfr)/exp_risk

  return(exp_sharpe)

}
