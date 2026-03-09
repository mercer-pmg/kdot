# Fixtures for homepage performance tests (no API calls)

#' Minimal homepage performance response structure for build_*_performance_table
fixture_homepage_result <- function() {
  bm_calc <- list(
    name = "S&P 500",
    entity = "Index",
    entityId = 1L,
    calculations = list(
      list(
        id = "standard",
        series = list(
          list(startDate = "2025-01-01", endDate = "2025-01-31", performance = 0.03)
        )
      )
    )
  )
  list(
    success = TRUE,
    data = list(
      database = list(
        calculations = list(
          calculations = list(
            list(
              id = "standard",
              series = list(
                list(startDate = "2025-01-01", endDate = "2025-01-31", performance = 0.02),
                list(startDate = "2025-02-01", endDate = "2025-02-28", performance = 0.01)
              )
            )
          )
        )
      ),
      benchmarkPerformance = list(
        calculations = data.frame(
          type = "benchmark",
          calculations = I(list(bm_calc)),
          stringsAsFactors = FALSE
        )
      )
    )
  )
}

#' Same as fixture but with different performance values (for "data changed" tests)
fixture_homepage_result_changed <- function() {
  r <- fixture_homepage_result()
  r$data$database$calculations$calculations[[1]]$series[[1]]$performance <- 0.025
  r
}
