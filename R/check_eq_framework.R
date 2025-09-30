#' Check Equity Framework
#'
#' @param data cleaned orion platform tibble
#' @param exception_patterns apply exception filters: "(ETF, exCore)|(ERISA)|(ETF, Sustainable)|(MF, Social)"
#'
#' @return list with three components: results_all, results_pass, results_fail
#' @export
#'
#' @examples
#' # Load sample data
#' aim <- readr::read_csv("Orion Platform - XXXX.XX.csv") |> clean_orion_platform()
#'
#' # Check framework compliance with MA approved exceptions
#' eq_results <- check_eq_framework(aim, exception_patterns = TRUE)
check_eq_framework <- function(data, exception_patterns = TRUE) {
  strategy <- type <- asset_category <- model_agg <- agg_target <- model_agg_weight <- NULL
  actual_us_lc <- actual_us_sc <- actual_us_ac <- actual_us_mc <- eq_total_allocation <- NULL
  is_us_only <- base_lc_target <- base_sc_target <- equity_framework_pct <- us_eq_target <- NULL
  target_us_lc <- target_us_sc <- target_us_ac <- target_us_mc <- NULL
  us_lc_check <- us_sc_check <- us_ac_check <- us_mc_check <- passed <- NULL

  if (exception_patterns) {
    exception_regex <- "\\(ETF, exCore\\)|\\(ERISA\\)|\\(ETF, Sustainable\\)|\\(MF, Social\\)"
    data <- data |> dplyr::filter(!stringr::str_detect(strategy, stringr::regex(exception_regex)))
  }

  eq_model_agg_summary <- data |>
    dplyr::filter(type %in% c("Market Series", "Multifactor Series", "Income Series")) |>
    dplyr::filter(asset_category == "Equity") |>
    tidyr::replace_na(list(agg_target = 0)) |>
    dplyr::group_by(strategy, type, model_agg) |>
    dplyr::summarise(model_agg_weight = dplyr::first(agg_target), .groups = "drop")

  results_all <- eq_model_agg_summary |>
    dplyr::group_by(strategy, type) |>
    dplyr::summarise(
      actual_us_lc = sum(model_agg_weight[market_cap == "US Large Cap"]),
      actual_us_sc = sum(model_agg_weight[market_cap == "US Small Cap"]),
      actual_us_ac = sum(model_agg_weight[market_cap == "US All Cap"]),
      actual_us_mc = sum(model_agg_weight[market_cap == "US Mid Cap"]),
      eq_total_allocation = sum(model_agg_weight),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      is_us_only = stringr::str_detect(strategy, stringr::regex("US Only")),

      # Base targets
      base_lc_target = dplyr::case_match(type, "Income Series" ~ 60, "Multifactor Series" ~ 60, "Market Series" ~ 48, .default = 0),
      base_sc_target = dplyr::case_match(type, "Income Series" ~ 8, "Multifactor Series" ~ 8, "Market Series" ~ 20, .default = 0),

      # Calc targets
      equity_framework_pct = ifelse(is_us_only, 1.0, 0.68),
      us_eq_target = eq_total_allocation * equity_framework_pct,
      target_us_lc = ifelse(actual_us_lc > 0 | actual_us_sc > 0, us_eq_target * (base_lc_target / 68), 0),
      target_us_sc = ifelse(actual_us_lc > 0 | actual_us_sc > 0, us_eq_target * (base_sc_target / 68), 0),
      target_us_ac = ifelse(actual_us_lc == 0 & actual_us_sc == 0 & actual_us_ac > 0, us_eq_target, 0),
      target_us_mc = 0,

      # 1% tolerance for target compliance
      us_lc_check = abs(actual_us_lc - target_us_lc) < 1,
      us_sc_check = abs(actual_us_sc - target_us_sc) < 1,
      us_ac_check = abs(actual_us_ac - target_us_ac) < 1,
      us_mc_check = abs(actual_us_mc - target_us_mc) < 1,
      passed = us_lc_check & us_sc_check & us_ac_check & us_mc_check
    )

  results_pass <- results_all |> dplyr::filter(passed == TRUE)
  results_fail <- results_all |> dplyr::filter(passed == FALSE)

  return(list(
    results_all = results_all,
    results_pass = results_pass,
    results_fail = results_fail
  ))
}
