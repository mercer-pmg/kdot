#' Create portfolio to be uploaded to Y Charts
#'
#' @param portfolio_name character
#'
#' @return a tibble
#' @export
#'
#'
portfolio_upload_ycharts <- function(portfolio_name){

  Ticker <- Target <- symbol <- Date <- Symbol <- `Target Weight` <- NULL

  dat <- kdot::portfolio_holdings_history(portfolio_name) |>
    dplyr::mutate(Target = Target |> round(4))

  cash_allocation <- dat |>
    dplyr::group_by(Date) |>
    dplyr::summarise(
      Target = 1 - sum(Target),
      Target = Target |> round(4)) |>
    dplyr::mutate(Ticker = "$:CASH")

  dat <- dat |>
    dplyr::bind_rows(cash_allocation) |>
    dplyr::arrange(Date) |>
    dplyr::mutate(
      Date = Date |> format("%m/%d/%Y"),
      Symbol = Ticker,
      `Target Weight` = Target |> round(4)) |>
    dplyr::select(Date, Symbol, `Target Weight`)

  readr::write_csv(dat, file = paste(portfolio_name, "- YCharts Upload.csv"))

  return(dat)

}
