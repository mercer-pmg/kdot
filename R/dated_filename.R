#' Generate filename with date
#'
#' @param x root name of the file
#' @param type file extension
#'
#' @returns a string
#' @export
#'
#'
dated_filename <- function(x, type) {

  d <- Sys.Date() |> format("%Y.%m.%d")

  y <- paste0(x, " - ", d, ".", type)

  return(y)
}
