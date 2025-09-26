#' Check for model_agg crossovers between different RB series'
#'
#' @param data cleaned orion platform tibble
#' @param print_results print results to console
#' @param write_csv write results to CSV file
#' @param csv_filename CSV output filename
#'
#' @return A data frame containing strategies that violate model_agg crossover rules.
#' @export
#'
#' @examples
#' # Load and prep data
#' aim <- readr::read_csv("Orion Platform - XXXX.XX.csv") |>
#'     clean_orion_platform()
#'
#' # Check for violations
#' violations <- check_model_agg_crossover(aim)
#'
#' # Check and save results to CSV
#' violations <- check_model_agg_crossover(aim, write_csv = TRUE, csv_filename = "model_agg_violations.csv")
check_model_agg_crossover <- function(data, print_results = TRUE, write_csv = FALSE, csv_filename = "model_agg_violations.csv") {
    violations <- data |>
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
        dplyr::filter(violates_rule == TRUE) |>
        dplyr::distinct()

    if (print_results) {
        if (nrow(violations) > 0) {
            cat("Found", nrow(violations), "model_agg crossover violations:\n")
            print(violations)
        } else {
            cat("No model_agg crossover violations found.\n")
        }
    }

    if (write_csv) {
        readr::write_csv(violations, csv_filename)
        if (print_results) {
            cat("Results written to:", csv_filename, "\n")
        }
    }

    return(violations)
}
