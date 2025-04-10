#' YTD strategy return
#'
#' @param strategy_name the name of an MA strategy
#' @param type "gross" or "net" for fees
#'
#' @returns a numeric
#' @export
#'

strategy_return_ytd <- function(strategy_name, type = "net"){

  start_dt <- Sys.Date() |> lubridate::floor_date(unit = "year")
  start_dt <- start_dt - 1

  end_dt <- Sys.Date() - 1

  ytd_return <- kdot::strategy_return(
    strategy_name = strategy_name,
    start         = start_dt,
    end           = end_dt,
    type          = type)

  return(ytd_return)


}
