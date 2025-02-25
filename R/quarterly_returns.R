#' Calculate calendar quarter returns of a security
#'
#' @param x a Bloomberg security id
#' @param start.date a date in YYYY-MM-DD format
#' @param end.date a date in YYYY-MM-DD format
#'
#' @return a tibble
#' @export
#'
#'
quarterly_returns <- function(x, start.date, end.date = Sys.Date()){

  PX_LAST <- price <- quarter <- year <- period <- last_day <- NULL
  start_price <- end_price <- ret <- index <- security <- NULL

  # Connect to Bloomberg API
  Rblpapi::blpConnect()

  # Retrieve all values between start and end dates
  xx <- Rblpapi::bdh(
    securities = x,
    fields     = "PX_LAST",
    start.date = lubridate::as_date(start.date),
    end.date   = lubridate::as_date(end.date))

  xx <- xx |>
    dplyr::mutate(price = PX_LAST) |>
    dplyr::select(date, price)

  # Trim to quarter-end values
  xx <- xx |>
    dplyr::mutate(
      year    = date |> lubridate::year(),
      quarter = date |> lubridate::quarter(),
      period = paste0("Q", quarter) |> paste(year, sep = "\n")) |>
    dplyr::group_by(period) |>
    dplyr::mutate(last_day = max(date)) |>
    dplyr::filter(date == last_day) |>
    dplyr::ungroup()

  # Calculate percent change between quarter-ends
  xx <- xx |>
    dplyr::mutate(
      end_price = price,
      start_price = dplyr::lag(price)) |>
    tidyr::drop_na(start_price) |>
    dplyr::mutate(ret = end_price/start_price-1) |>
    dplyr::select(date, ret, period)

  # Organize data
  xx <- xx |>
    dplyr::arrange(dplyr::desc(date)) |>
    dplyr::mutate(security = x) |>
    dplyr::select(security, date, period, ret)

  return(xx)
}
