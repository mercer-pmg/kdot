#' Check for model_agg crossovers between different RB series'
#'
#' @param data cleaned orion platform tibble
#'
#' @return list with three components: results_all, results_pass, results_fail
#' @export
#'
#' @examples
#' # Load and prep data
#' aim <- readr::read_csv("Orion Platform - XXXX.XX.csv") |>
#'     clean_orion_platform()
#'
#' # Check for violations
#' crossover_results <- check_model_agg_crossover(aim)
check_model_agg_crossover <- function(data) {
    category <- asset_category <- type <- model_agg <- violates_rule <- NULL

    results_all <- data |>
        dplyr::filter(category == "Risk-Based", asset_category == "Equity") |>
        dplyr::filter(type %in% c("Multifactor Series", "Market Series", "Income Series")) |>
        dplyr::mutate(
            violates_rule = dplyr::case_when(
                type == "Multifactor Series" & stringr::str_detect(model_agg, "MA Market|MA Income") ~ TRUE,
                type == "Market Series" & stringr::str_detect(model_agg, "MA Multifactor|MA Income") ~ TRUE,
                type == "Income Series" & stringr::str_detect(model_agg, "MA Market|MA Multifactor") ~ TRUE,
                .default = FALSE
            )
        ) |>
        dplyr::distinct()

    results_pass <- results_all |> dplyr::filter(violates_rule == FALSE)
    results_fail <- results_all |> dplyr::filter(violates_rule == TRUE)

    return(list(
        results_all = results_all,
        results_pass = results_pass,
        results_fail = results_fail
    ))
}
