#' Calculate the expected equity beta of a portfolio
#'
#' @param allocation The allocation of a portfolio
#' @param market The relative market used to calculate equity beta
#'
#' @return a numeric
#' @export
#'
#' @examples
#' x <- get_portfolio_allocation("Market Series ETF 080")
#' expected_equity_beta(x)
#'
expected_equity_beta <- function(allocation, market = "AC World Equity"){

  `Asset Class` <- foo <- Asset <- Volatility <- corr_to_mkt <- NULL
  exp_asset_equity_beta <- Weight <- contribution <- NULL

  asset_classes <- names(cma_cor_mat)

  asset_class_mkt_corr <- cma_cor_mat |>
    dplyr::select(tidyselect::all_of(market)) |>
    dplyr::mutate(`Asset Class` = asset_classes) |>
    dplyr::filter(`Asset Class` %in% allocation$Asset) |>
    tidyr::pivot_longer(cols      = -`Asset Class`,
                        names_to  = "foo",
                        values_to = "corr_to_mkt") |>
    dplyr::select(-foo)

  market_sd <- cma_return_risk |>
    dplyr::filter(Asset == market) |>
    dplyr::pull(Volatility)

  asset_class_sd <- cma_return_risk |>
    dplyr::filter(Asset %in% allocation$Asset) |>
    dplyr::mutate(`Asset Class` = Asset) |>
    dplyr::select(`Asset Class`, Volatility)

  expected_asset_class_beta <- asset_class_mkt_corr |>
    dplyr::left_join(asset_class_sd, by = "Asset Class") |>
    dplyr::mutate(exp_asset_equity_beta = (corr_to_mkt*Volatility)/market_sd) |>
    dplyr::select(`Asset Class`, exp_asset_equity_beta) |>
    dplyr::mutate(Asset = `Asset Class`) |>
    dplyr::select(Asset, exp_asset_equity_beta)

  expected_equity_beta <- allocation |>
    dplyr::left_join(expected_asset_class_beta, by = "Asset") |>
    dplyr::mutate(contribution = Weight*exp_asset_equity_beta) |>
    dplyr::pull(contribution) |>
    sum()

  return(expected_equity_beta)

}
