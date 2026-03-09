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
#' @returns A tibble with columns: cleaned_ticker, security_id (col 2 raw),
#'   announced_date (col 3), effective_date (col 4), name (col 6 with
#'   "Name: " stripped). Only rows where the product ticker matches a
#'   delisted security. Empty tibble means all clear.
#' @export
#'
#'
check_delisted_products <- function(product_tickers, delisted_csv_path) {
  empty_result <- tibble::tibble(
    cleaned_ticker = character(),
    security_id = character(),
    announced_date = character(),
    effective_date = character(),
    name = character()
  )

  delisted_raw <- readr::read_csv(
    delisted_csv_path,
    skip = 5,
    col_names = FALSE,
    show_col_types = FALSE
  )

  if (ncol(delisted_raw) < 6 || nrow(delisted_raw) == 0) {
    return(empty_result)
  }

  delisted_df <- tibble::tibble(
    cleaned_ticker = trimws(stringr::str_extract(
      as.character(delisted_raw[[2]]),
      "^[^\\s]+"
    )),
    security_id = as.character(delisted_raw[[2]]),
    announced_date = as.character(delisted_raw[[3]]),
    effective_date = as.character(delisted_raw[[4]]),
    name = stringr::str_remove(as.character(delisted_raw[[6]]), "^Name:\\s*")
  )

  product_tickers_norm <- product_tickers |>
    as.character() |>
    trimws() |>
    unique()
  product_tickers_norm <- product_tickers_norm[
    !is.na(product_tickers_norm) & product_tickers_norm != ""
  ]

  delisted_df |>
    dplyr::filter(cleaned_ticker %in% product_tickers_norm)
}
