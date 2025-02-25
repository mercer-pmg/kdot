#' Get a portfolio's full holdings history
#'
#' @param portfolio_name character
#'
#' @return tibble
#' @export
#'
#' @examples
#' x <- "Multifactor Series ETF 060"
#' portfolio_holdings_history(x)

portfolio_holdings_history <- function(portfolio_name){

  date <- symbol <- target <- portfolio <- NULL

  dat <- sleeve_holdings |>
    dplyr::bind_rows(model_portfolio_holdings) |>
    dplyr::filter(portfolio == portfolio_name) |>
    dplyr::select(date, symbol, target)

  return(dat)

}
