#' Create portfolio to be uploaded to FactSet
#'
#' @param portfolio_name a character
#'
#' @return a tibble.
#' @export
#'
#'
portfolio_upload_factset <- function(portfolio_name){

  DATE <- SYMBOL <- WEIGHT <- symbol <- target <- NULL


  dat <- kdot::portfolio_holdings_history(portfolio_name) |>
    dplyr::mutate(target = target |> round(4)) |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      DATE = date |> format("%Y%m%d"),
      SYMBOL = symbol,
      WEIGHT = target |> round(4)) |>
    dplyr::select(SYMBOL, DATE, WEIGHT)

  readr::write_csv(dat, file = paste(portfolio_name, "- FactSet Upload.csv"))

  return(dat)

}
