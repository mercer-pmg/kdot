#' Find Duplicate Strategies
#'
#' @param data cleaned orion platform tibble
#' @param threshold distance threshold for duplicates (default: 0)
#' @param round decimal places to round weights (default: 6)
#' @param print_csv write CSV file (default: TRUE)
#' @param print_results print results to console (default: TRUE)
#' @param granularity analysis level: "model_agg" or "ticker" (default: "model_agg")
#'
#' @return tibble with duplicate strategy information
#' @export
#'
#' @examples
#' # Load sample data
#' aim <- readr::read_csv("Orion Platform - XXXX.XX.csv")
#'
#' # Find exact duplicates (default)
#' exact_duplicates <- find_duplicate_strategies(aim)
#'
#' # Find near duplicates with ticker-level analysis
#' near_duplicates <- find_duplicate_strategies(aim, threshold = 0.005, granularity = "ticker")
#'
find_duplicate_strategies <- function(data, threshold = 0, round = 6, print_csv = TRUE, print_results = TRUE, granularity = "model_agg") {
  strategy <- ticker <- model_agg <- agg_target <- target <- ticker_weight <- total_weight <- NULL
  strategy_1 <- strategy_2 <- position <- NULL

  if (!granularity %in% c("ticker", "model_agg")) {
    stop("granularity must be either 'ticker' or 'model_agg'")
  }

  if (granularity == "model_agg") {
    # Model aggregation level
    matrix <- data |>
      dplyr::mutate(agg_target = ifelse(is.na(agg_target), 0, round(agg_target, round))) |>
      dplyr::group_by(strategy, model_agg) |>
      dplyr::summarise(total_weight = dplyr::first(agg_target), .groups = "drop") |>
      tidyr::pivot_wider(
        names_from = model_agg,
        values_from = total_weight,
        values_fill = 0
      ) |>
      tibble::column_to_rownames("strategy") |>
      as.matrix()
  } else {
    # Ticker level
    matrix <- data |>
      dplyr::mutate(
        agg_target = ifelse(is.na(agg_target), 0, agg_target) / 100,
        target = target / 100,
        ticker_weight = round(agg_target * target, round)
      ) |>
      dplyr::group_by(strategy, ticker) |>
      dplyr::summarise(total_weight = sum(ticker_weight), .groups = "drop") |>
      tidyr::pivot_wider(
        names_from = ticker,
        values_from = total_weight,
        values_fill = 0
      ) |>
      tibble::column_to_rownames("strategy") |>
      as.matrix()
  }

  if (print_results) {
    cat("Running duplicate analysis with threshold:", threshold, "\n")
  }

  # Calc euclidean distance to determine similarity
  distance_matrix <- dist(matrix, method = "euclidean") |> as.matrix()

  # Set lower triangle of the matrix to NA to prevent finding duplicate pairs (ex: NOT[A-B, B-A], CORRECT[A-B])
  distance_matrix[lower.tri(distance_matrix, diag = TRUE)] <- NA

  duplicates <- which(
    distance_matrix <= threshold & !is.na(distance_matrix),
    arr.ind = TRUE
  )

  if (nrow(duplicates) == 0) {
    if (print_results) {
      cat("No duplicate strategies found with threshold:", threshold, "\n")
    }
    return(tibble::tibble()) # Return empty tibble
  }

  duplicates_summary <- data.frame(
    pair_id = paste0("Pair_", sprintf("%02d", seq_len(nrow(duplicates)))),
    strategy_1 = rownames(distance_matrix)[duplicates[, 1]],
    strategy_2 = colnames(distance_matrix)[duplicates[, 2]],
    distance = distance_matrix[duplicates],
    threshold_used = threshold
  )

  # Get unique strategies and filter matrix to non-zero columns
  unique_strategies <- unique(
    c(duplicates_summary$strategy_1, duplicates_summary$strategy_2)
  )

  duplicates_matrix <- matrix[
    unique_strategies,
    colSums(matrix[unique_strategies, , drop = FALSE]) > 0,
    drop = FALSE
  ]

  duplicates_detailed <- duplicates_summary |>
    tidyr::pivot_longer(
      cols = c(strategy_1, strategy_2),
      names_to = "position",
      values_to = "strategy"
    ) |>
    dplyr::select(-position) |>
    dplyr::left_join(
      duplicates_matrix |>
        as.data.frame() |>
        tibble::rownames_to_column("strategy"),
      by = "strategy"
    )

  # Arrange the output by distance (smallest to largest), then by pair_id
  duplicates_detailed <- duplicates_detailed |>
    dplyr::arrange(distance, pair_id)

  if (print_results) {
    cat("Found", nrow(duplicates_summary), "pairs\n")
  }

  if (print_csv) {
    output_filename <- paste0(
      "duplicates_threshold_",
      gsub("\\.", "_", threshold),
      "_granularity_",
      granularity,
      ".csv"
    )

    readr::write_csv(duplicates_detailed, output_filename)
    if (print_results) {
      cat("Results saved to:", output_filename, "\n")
    }
  }

  return(duplicates_detailed)
}
