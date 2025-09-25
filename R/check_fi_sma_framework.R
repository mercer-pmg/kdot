#' Check Fixed Income SMA Framework Compliance
#'
#' @param data Data frame containing strategy allocation data with columns: strategy, type, asset_category, model_agg, model_series, agg_target, is_SMA
#' @param test_error_injection Logical. If TRUE, injects test errors for validation (default: FALSE)
#'
#' @return A list containing:
#'   - allocation_check: Full allocation check results
#'   - strategies_pass: Strategies that pass the framework
#'   - strategies_fail: Strategies that fail the framework
#' @export
#'
#' @examples
#' # Load sample data
#' aim <- readr::read_csv("Orion Platform - XXXX.XX.csv") |> clean_orion_platform()
#'
#' # Check framework compliance (default behavior)
#' fi_results <- check_fi_sma_framework(aim)
#'
#' # Test with error injection
#' fi_results <- check_fi_sma_framework(aim, test_error_injection = TRUE)
check_fi_sma_framework <- function(data, test_error_injection = FALSE) {
    strategy <- type <- asset_category <- model_agg <- model_series <- agg_target <- is_SMA <- NULL
    has_sma <- model_agg_allocation <- is_sma_group <- sma_allocation <- non_sma_allocation <- NULL
    fi_total_allocation <- expected_sma_pct <- expected_non_sma_pct <- actual_sma_pct <- NULL
    actual_non_sma_pct <- sma_target_met <- non_sma_target_met <- fixed_income_allocation <- NULL

    # Apply test error injection if requested
    if (test_error_injection) {
        data <- data |>
            dplyr::mutate(
                agg_target = dplyr::case_when(
                    strategy == "Multifactor 70 TM (ETF,QUSALVMQ,QIDMVMQ,N7YMUN)" &
                        model_agg == "Nuveen Municipal Ladder 1-7 Year" ~ 30,
                    strategy == "Multifactor 70 TM (ETF,QUSALVMQ,QIDMVMQ,N7YMUN)" &
                        model_agg == "MA Fixed Income TM (ETF)" ~ 0,
                    TRUE ~ agg_target
                )
            )
    }

    # Filter to strategies with SMA
    sma_strategies <- data |>
        dplyr::group_by(strategy) |>
        dplyr::mutate(
            has_sma = any(is_SMA)
        ) |>
        dplyr::ungroup() |>
        dplyr::filter(has_sma == TRUE)

    fi_model_agg_summary <- sma_strategies |>
        dplyr::filter(type %in% c("Market Series", "Multifactor Series", "Income Series")) |>
        dplyr::filter(asset_category == "Fixed Income") |>
        dplyr::group_by(strategy, type, model_agg, model_series) |>
        dplyr::summarise(
            model_agg_allocation = {
                val <- dplyr::first(agg_target)
                if (is.na(val)) 0 else val
            },
            is_sma_group = any(is_SMA),
            .groups = "drop"
        ) |>
        dplyr::arrange(strategy, model_agg)

    # Check allocations
    allocation_check <- fi_model_agg_summary |>
        dplyr::group_by(strategy, type) |>
        dplyr::summarise(
            sma_allocation = sum(model_agg_allocation[is_sma_group]),
            non_sma_allocation = sum(model_agg_allocation[!is_sma_group]),
            fi_total_allocation = sum(model_agg_allocation),
            .groups = "drop"
        ) |>
        dplyr::mutate(
            expected_sma_pct = dplyr::case_when(
                type == "Income Series" ~ 80,
                type %in% c("Market Series", "Multifactor Series") ~ 75
            ),
            expected_non_sma_pct = dplyr::case_when(
                type == "Income Series" ~ 20,
                type %in% c("Market Series", "Multifactor Series") ~ 25
            ),
            actual_sma_pct = round((sma_allocation / fi_total_allocation) * 100, 2),
            actual_non_sma_pct = round((non_sma_allocation / fi_total_allocation) * 100, 2),
            sma_target_met = abs(actual_sma_pct - expected_sma_pct) < 1,
            non_sma_target_met = abs(actual_non_sma_pct - expected_non_sma_pct) < 1,
            fixed_income_allocation = sma_target_met & non_sma_target_met
        )

    strategies_pass <- allocation_check |> dplyr::filter(fixed_income_allocation == TRUE)
    strategies_fail <- allocation_check |> dplyr::filter(fixed_income_allocation == FALSE)

    cat(sprintf(
        "Number of strategies that Pass: %d\nNumber of strategies that FAIL: %d\n",
        nrow(strategies_pass),
        nrow(strategies_fail)
    ))

    return(list(
        allocation_check = allocation_check,
        strategies_pass = strategies_pass,
        strategies_fail = strategies_fail
    ))
}
