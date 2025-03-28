#' Get security-level portfolio holdings.
#'
#' @param strategy_name character
#' @param date character
#'
#' @return a tibble
#' @export
#'
#'
#'
portfolio_holdings <- function(strategy_name, date = Sys.Date()){

  portfolio <- symbol <- target <- Strategy <- Date <- NULL
  Ticker <- Target <- NULL

  holdings_date <- date |> lubridate::as_date()

  dat <- strategy_holdings |>
    dplyr::filter(Strategy == strategy_name) |>
    dplyr::filter(Date <= holdings_date) |>
    dplyr::slice_max(Date) |>
    dplyr::select(Ticker, Target)

  return(dat)

}
