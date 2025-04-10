#' Calculate the return (net or gross) of a MA strategy
#'
#' @param strategy_name the name of an MA strategy
#' @param start yyyy-mm-dd
#' @param end yyyy-mm-dd
#' @param type either "net" or "gross"
#'
#' @returns a numeric
#' @export
#'
strategy_return <- function(
    strategy_name,
    start,
    end,
    type = "net") {

  Strategy <- strategy <- Date <- Value <- ycharts_id <- NULL

  # Translate strategy name into YCharts portfolio id.
  strategy_name <- strategy_ids |>
    dplyr::filter(strategy == strategy_name) |>
    dplyr::pull(ycharts_id)

  # Read either gross or net indexes dataset.
  if(type == "net"){
    index <- readr::read_csv(
      file = "~/Data/strategy_indexes_net.csv",
      show_col_types = FALSE)
  } else {
    if(type == "gross"){
      index <- readr::read_csv(
        file = "~/Data/strategy_indexes_gross.csv",
        show_col_types = FALSE)
    }
  }

  # Filter data to the relevant strategy index.
  index <- index |> dplyr::filter(Strategy == strategy_name)

  # Find start and end values for holding period.
  start <- index |> dplyr::filter(Date == lubridate::as_date(start)) |> dplyr::pull(Value)
  end   <- index |> dplyr::filter(Date == lubridate::as_date(end)) |> dplyr::pull(Value)

  # Calculate return.
  strat_return <- end/start - 1

  return(strat_return)

}
