#' Clean and Categorize Orion Platform Data
#'
#' @param data Orion Platform XXXX-XX.csv
#'
#' @return A tibble with added columns: 'category', 'model_series', 'asset_category', 'market_cap', and 'is_SMA'.
#' @export
#'
clean_orion_platform <- function(data) {
  required_cols <- c("type", "model_agg")
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  categorized_data <- data |>
    dplyr::mutate(
      category = dplyr::case_match(
        type,
        "Blended Strategy" ~ "Blended",
        c("Market Series", "Multifactor Series", "Income Series") ~ "Risk-Based",
        c(
          "Equity Strategies", "Fixed Income Strategies", "Cash Strategies",
          "Alternative Strategies", "Special Situation Strategies"
        ) ~ "Asset Class",
        .default = NA_character_
      ),
      model_series = dplyr::case_when(
        stringr::str_detect(model_agg, "MA Market") ~ "Market Series",
        stringr::str_detect(model_agg, "MA Multifactor") ~ "Multifactor Series",
        stringr::str_detect(model_agg, "MA Income") ~ "Income Series",
        stringr::str_detect(model_agg, "MA Cash Mgmt") ~ "Cash Mgmt",
        stringr::str_detect(model_agg, "Quantitative Portfolio") ~ "Quantitative Portfolios",
        stringr::str_detect(model_agg, "MA Fixed Income") ~ "Fixed Income",
        stringr::str_detect(model_agg, "Ladder \\(ETF\\)$") ~ "Fixed Income ETF Ladder",
        stringr::str_detect(model_agg, "BlackRock|Nuveen|PIMCO") ~ "Third-Party Fixed Income SMA",
        .default = "Other"
      ),
      asset_category = dplyr::case_when(
        stringr::str_detect(model_series, "Series|Quant") ~ "Equity",
        stringr::str_detect(model_series, "Fixed Income") ~ "Fixed Income",
        stringr::str_detect(model_series, "Other") ~ "Other",
        .default = NA_character_
      ),
      market_cap = dplyr::case_when(
        stringr::str_detect(model_agg, "\\bAll Cap\\b") ~ "US All Cap",
        stringr::str_detect(model_agg, "\\bLarge Cap\\b") ~ "US Large Cap",
        stringr::str_detect(model_agg, "\\bSmall Cap\\b") ~ "US Small Cap",
        stringr::str_detect(model_agg, "\\bMid Cap\\b") ~ "US Mid Cap",
        .default = "Other"
      ),
      is_SMA = stringr::str_detect(model_series, "SMA")
    )

  return(categorized_data)
}
