#' Run AIM Platform Audit
#'
#' @param data cleaned orion platform tibble
#' @param duplicate_threshold distance threshold for duplicate detection (default: 0)
#' @param exception_patterns apply exception filters to equity framework check (default: TRUE)
#' @param test_error_injection inject test errors for FI SMA validation (default: FALSE)
#' @param verbose display console output with audit results (default: TRUE)
#'
#' @return list containing all audit results
#' @export
#'
#' @examples
#' aim <- readr::read_csv("Orion Platform - XXXX.XX.csv") |> kdot::clean_orion_platform()
#' audit_results <- run_aim_audit(aim)
#' audit_results <- run_aim_audit(aim, duplicate_threshold = 2)
run_aim_audit <- function(data, duplicate_threshold = 0, exception_patterns = TRUE, test_error_injection = FALSE, verbose = TRUE) {
    # Run audits
    duplicate_results <- kdot::find_duplicate_strategies(data = data, threshold = duplicate_threshold)
    eq_results <- kdot::check_eq_framework(data = data, exception_patterns = exception_patterns)
    fi_results <- kdot::check_fi_sma_framework(data = data, test_error_injection = test_error_injection)
    crossover_results <- kdot::check_model_agg_crossover(data = data)

    # Build summary
    summary_data <- data.frame(
        Audit_Name = c(
            "Duplicate Strategies",
            "Equity Framework Compliance",
            "Fixed Income SMA Framework",
            "Model Aggregation Crossover"
        ),
        Fail_Count = c(
            nrow(duplicate_results$results_fail),
            nrow(eq_results$results_fail),
            nrow(fi_results$results_fail),
            nrow(crossover_results$results_fail)
        ),
        Status = c(
            ifelse(nrow(duplicate_results$results_fail) == 0, "PASS", "REVIEW"),
            ifelse(nrow(eq_results$results_fail) == 0, "PASS", "REVIEW"),
            ifelse(nrow(fi_results$results_fail) == 0, "PASS", "REVIEW"),
            ifelse(nrow(crossover_results$results_fail) == 0, "PASS", "REVIEW")
        )
    )

    if (verbose) {
        cat("\n")
        cat("================================================================================\n")
        cat("AIM Audit Results\n")
        cat("================================================================================\n")
        cat(sprintf("%-25s %2d failures  |  Threshold: %d\n", "Duplicates:", nrow(duplicate_results$results_fail), duplicate_threshold))
        cat(sprintf("%-25s %2d failures  |  Exclusions: %s\n", "Equity Framework:", nrow(eq_results$results_fail), ifelse(exception_patterns, "YES", "NO")))
        cat(sprintf("%-25s %2d failures  |  Test Errors: %s\n", "FI SMA Framework:", nrow(fi_results$results_fail), ifelse(test_error_injection, "YES", "NO")))
        cat(sprintf("%-25s %2d failures\n", "Crossover:", nrow(crossover_results$results_fail)))
        cat("================================================================================\n")
        cat("View summary: results$summary\n")
        cat("Explore audits: $duplicates | $equity_framework | $fi_sma_framework | $crossover\n")
        cat("Each audit contains: $results_all | $results_pass | $results_fail\n")
        cat("================================================================================\n")
    }

    return(list(
        summary = summary_data,
        duplicates = duplicate_results,
        equity_framework = eq_results,
        fi_sma_framework = fi_results,
        crossover = crossover_results
    ))
}
