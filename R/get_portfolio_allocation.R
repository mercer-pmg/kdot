#' Get the asset allocation of a model portfolio
#'
#' @param model_name A character vector with, at most, one element.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' x <- "Multifactor Series ETF 060"
#' get_portfolio_allocation(x)
#'
get_portfolio_allocation <- function(model_name){

  path <- paste0(
    "C:/Users/DavidAllen/OneDrive - Mercer Advisors/Documents/MA Models/Model Allocations/",
    model_name,
    ".csv")

  dat <- readr::read_csv(path, show_col_types = FALSE)

  weight_check <- all.equal(sum(dat$Weight), 1)

  if(!weight_check){
    print(
      paste("Caution.", model_name, "Weights do not sum to 100%.")
    )
  }

  return(dat)

}
