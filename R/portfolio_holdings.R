#' Get security-level portfolio holdings.
#'
#' @param portfolio_name character
#' @param date character
#'
#' @return a tibble
#' @export
#'
#' @examples
#' x <- "Multifactor Series ETF 060"
#' portfolio_holdings(x)
#' portfolio_holdings(x, "2022-01-23")
#'
portfolio_holdings <- function(portfolio_name, date = Sys.Date()){

  portfolio <- symbol <- target <- NULL

  holdings_date <- date |> lubridate::as_date()

  dat <- sleeve_holdings |>
    dplyr::bind_rows(model_portfolio_holdings) |>
    dplyr::filter(portfolio == portfolio_name) |>
    dplyr::filter(date <= holdings_date) |>
    dplyr::slice_max(date) |>
    dplyr::select(symbol, target)

  return(dat)

}
