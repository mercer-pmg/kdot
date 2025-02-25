#' Calculate the expected risk for a portfolio
#'
#' @param allocation The asset allocation of a portfolio
#'
#' @return a numeric
#' @export
#'
#' @examples
#' x <- "Market Series ETF 070"
#' y <- get_portfolio_allocation(x)
#' exp_return <- expected_risk(y)
#'
expected_risk <- function(allocation){

  Volatility <- NULL

  cma_expected_sd <- cma_return_risk |> dplyr::pull(Volatility)

  asset_classes <- names(cma_cor_mat)

  cov.mat <- diag(cma_expected_sd) %*% as.matrix(cma_cor_mat) %*% diag(cma_expected_sd)
  cov.mat <- cov.mat |> as.data.frame()
  names(cov.mat)     <- asset_classes
  row.names(cov.mat) <- asset_classes

  cov.mat <- cov.mat[allocation$Asset, allocation$Asset]
  cov.mat <- as.matrix(cov.mat)

  expected_risk <- (t(allocation$Weight) %*% cov.mat %*% allocation$Weight)
  expected_risk <- sqrt(expected_risk)[1,1]


  return(expected_risk)

}
