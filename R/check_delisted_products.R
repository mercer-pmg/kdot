#' Check which product tickers appear in Bloomberg delisted securities list
#'
#' Reads a Bloomberg delisted assets CSV (headers in row 4, data from row 6,
#' security ID in column 2) and compares against product tickers from the
#' query 10635 Orion export. Bloomberg security IDs (e.g. "AAPL US Equity")
#' are normalized by taking the text before the first space or trim.
#'
#' @param product_tickers Character vector of tickers from the query 10635
#'   Orion export (e.g. `take_export()$Ticker`).
#' @param delisted_csv_path Path to the Bloomberg delisted assets CSV.
#'
#' @returns Character vector of product tickers that appear in the delisted
#'   list. Empty vector means all clear (no matches).
#' @export
#'
#'
check_delisted_products <- function(product_tickers, delisted_csv_path) {
  delisted_raw <- readr::read_csv(
    delisted_csv_path,
    skip = 5,
    col_names = FALSE,
    show_col_types = FALSE
  )

  if (ncol(delisted_raw) < 2 || nrow(delisted_raw) == 0) {
    return(character(0))
  }

  delisted_tickers <- delisted_raw[[2]] |>
    as.character() |>
    stringr::str_extract("^[^\\s]+") |>
    trimws() |>
    unique()
  delisted_tickers <- delisted_tickers[!is.na(delisted_tickers) & delisted_tickers != ""]

  product_tickers_norm <- product_tickers |>
    as.character() |>
    trimws() |>
    unique()
  product_tickers_norm <- product_tickers_norm[!is.na(product_tickers_norm) & product_tickers_norm != ""]

  intersect(product_tickers_norm, delisted_tickers)
}
