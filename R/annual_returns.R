#' Calculate calendar year returns of a security
#'
#' @param x A Bloomberg security id.
#' @param start.date A date.
#' @param end.date A date.
#'
#' @return A tibble.
#' @export
#'

annual_returns <- function(x, start.date, end.date = Sys.Date()){

  PX_LAST <- price <- period <- last_date <- start_price <- end_price <- NULL
  ret <- security <- last_day <- NULL

  # Connect to Bloomberg API
  Rblpapi::blpConnect()

  # Retrieve all values between start and end dates
  xx <- Rblpapi::bdh(
    securities = x,
    fields     = "PX_LAST",
    start.date = lubridate::as_date(start.date),
    end.date   = end.date)

  xx <- xx |>
    dplyr::mutate(price = PX_LAST) |>
    dplyr::select(date, price)

  # Trim to year-end values
  xx <- xx |>
    dplyr::mutate(period = date |> lubridate::year()) |>
    dplyr::group_by(period) |>
    dplyr::mutate(last_day = max(date)) |>
    dplyr::filter(date == last_day) |>
    dplyr::ungroup()

  # Calculate percent change between year-ends
  xx <- xx |>
    dplyr::mutate(
      end_price   = price,
      start_price = dplyr::lag(price)) |>
    tidyr::drop_na(start_price) |>
    dplyr::mutate(ret = end_price/start_price-1) |>
    dplyr::select(date, ret, period)

  # Organize data
  xx <- xx |>
    dplyr::arrange(dplyr::desc(date)) |>
    dplyr::mutate(security = x) |>
    # dplyr::mutate(period = paste0("\n",period)) |>
    dplyr::mutate(period = paste0(period)) |>
    dplyr::select(security, date, period, ret)

  return(xx)
}
