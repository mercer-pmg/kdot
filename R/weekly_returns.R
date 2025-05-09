#' Calculate calendar week returns of a security
#'
#' @param x A Bloomberg security id.
#' @param start.date A date.
#' @param end.date A date.
#'
#' @return A tibble.
#' @export
#'
#'
weekly_returns <- function(x, start.date, end.date = Sys.Date()){

  PX_LAST <- price <- year <- week <- period <- last_day <- NULL
  start_price <- end_price <- ret <- security <- NULL

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

  # Trim to week-end values
  xx <- xx |>
    dplyr::mutate(
      week   = date |> lubridate::week(),
      year   = date |> lubridate::year(),
      period = paste0(year,"\nWeek ",week)) |>
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
    dplyr::select(security, date, period, ret)

  return(xx)
}
