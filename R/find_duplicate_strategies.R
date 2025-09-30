#' Find Duplicate Strategies
#'
#' @param data cleaned orion platform tibble
#' @param threshold distance threshold for duplicates (default: 0)
#'
#' @return list with three components: results_all, results_pass, results_fail
#' @export
#'
#' @examples
#' # Load sample data
#' aim <- readr::read_csv("Orion Platform - XXXX.XX.csv")
#'
#' # Find exact duplicates (default)
#' exact_duplicates <- find_duplicate_strategies(aim)
#'
#' # Find near duplicates with threshold
#' near_duplicates <- find_duplicate_strategies(aim, threshold = 2)
#'
find_duplicate_strategies <- function(data, threshold = 0) {
  strategy <- model_agg <- agg_target <- total_weight <- NULL
  strategy_1 <- strategy_2 <- position <- is_duplicate <- NULL

  # Model aggregation level
  matrix <- data |>
    dplyr::mutate(agg_target = ifelse(is.na(agg_target), 0, round(agg_target, 6))) |>
    dplyr::group_by(strategy, model_agg) |>
    dplyr::summarise(total_weight = dplyr::first(agg_target), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = model_agg,
      values_from = total_weight,
      values_fill = 0
    ) |>
    tibble::column_to_rownames("strategy") |>
    as.matrix()

  # Calc euclidean distance to determine similarity
  distance_matrix <- dist(matrix, method = "euclidean") |> as.matrix()

  # Set lower triangle of the matrix to NA to prevent finding duplicate pairs (ex: NOT[A-B, B-A], CORRECT[A-B])
  distance_matrix[lower.tri(distance_matrix, diag = TRUE)] <- NA

  duplicates <- which(
    distance_matrix <= threshold & !is.na(distance_matrix),
    arr.ind = TRUE
  )

  # Get all unique strategies
  all_strategies <- rownames(matrix)

  if (nrow(duplicates) == 0) {
    # No duplicates found - all strategies pass
    results_all <- tibble::tibble(
      strategy = all_strategies,
      is_duplicate = FALSE
    )

    return(list(
      results_all = results_all,
      results_pass = results_all,
      results_fail = tibble::tibble()
    ))
  }

  duplicates_summary <- data.frame(
    pair_id = paste0("Pair_", sprintf("%02d", seq_len(nrow(duplicates)))),
    strategy_1 = rownames(distance_matrix)[duplicates[, 1]],
    strategy_2 = colnames(distance_matrix)[duplicates[, 2]],
    distance = distance_matrix[duplicates],
    threshold_used = threshold
  )

  # Get unique duplicate strategies
  duplicate_strategies <- unique(
    c(duplicates_summary$strategy_1, duplicates_summary$strategy_2)
  )

  # Filter matrix to non-zero columns for duplicates
  duplicates_matrix <- matrix[
    duplicate_strategies,
    colSums(matrix[duplicate_strategies, , drop = FALSE]) > 0,
    drop = FALSE
  ]

  results_fail <- duplicates_summary |>
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
    ) |>
    dplyr::arrange(distance, pair_id)

  # Create results_all with is_duplicate column
  results_all <- tibble::tibble(
    strategy = all_strategies,
    is_duplicate = strategy %in% duplicate_strategies
  )

  # Create results_pass (non-duplicates)
  results_pass <- results_all |> dplyr::filter(is_duplicate == FALSE)

  return(list(
    results_all = results_all,
    results_pass = results_pass,
    results_fail = results_fail
  ))
}
