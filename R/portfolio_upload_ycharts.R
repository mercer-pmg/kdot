#' Create portfolio to be uploaded to Y Charts
#'
#' @param portfolio_name character
#'
#' @return a tibble
#' @export
#'
#'
portfolio_upload_ycharts <- function(portfolio_name){

  target <- symbol <- Date <- Symbol <- `Target Weight` <- NULL

  dat <- kdot::portfolio_holdings_history(portfolio_name) |>
    dplyr::mutate(target = target |> round(4))

  cash_allocation <- dat |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      target = 1 - sum(target),
      target = target |> round(4)) |>
    dplyr::mutate(symbol = "$:CASH")

  dat <- dat |>
    dplyr::bind_rows(cash_allocation) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      Date = date |> format("%m/%d/%Y"),
      Symbol = symbol,
      `Target Weight` = target |> round(4)) |>
    dplyr::select(Date, Symbol, `Target Weight`)

  readr::write_csv(dat, file = paste(portfolio_name, "- YCharts Upload.csv"))

  return(dat)

}
