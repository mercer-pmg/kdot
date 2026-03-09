test_that("refresh_reporting_cache succeeds when orion_request returns success", {
  mock_orion <- function(
    method,
    endpoint,
    json_data,
    token,
    error_context,
    timeout
  ) {
    expect_equal(method, "POST")
    expect_equal(endpoint, "/api/v1/Reporting/Data/Cache/Refresh")
    expect_equal(json_data$dateRange$startDate, "2025-12-01")
    expect_equal(json_data$dateRange$endDate, "2026-01-31")
    expect_equal(json_data$dateRange$`$type`, "date-range")
    list(success = TRUE, status_code = 200L)
  }

  testthat::local_mocked_bindings(orion_request = mock_orion)
  expect_silent(refresh_reporting_cache(
    "2025-12-01",
    "2026-01-31",
    token = "fake-token"
  ))
  expect_invisible(refresh_reporting_cache(
    "2025-12-01",
    "2026-01-31",
    token = "fake-token"
  ))
})

test_that("refresh_reporting_cache succeeds when orion_request returns 202 Accepted", {
  mock_orion <- function(...) {
    list(success = FALSE, status_code = 202L, error = "Accepted")
  }
  testthat::local_mocked_bindings(orion_request = mock_orion)
  expect_silent(refresh_reporting_cache(
    "2025-12-01",
    "2026-01-31",
    token = "fake-token"
  ))
})

test_that("refresh_reporting_cache errors when orion_request fails", {
  mock_orion <- function(...) {
    list(success = FALSE, status_code = 500L, error = "Server error")
  }
  testthat::local_mocked_bindings(orion_request = mock_orion)
  expect_error(
    refresh_reporting_cache("2025-12-01", "2026-01-31", token = "fake-token"),
    "Cache refresh request failed"
  )
})

test_that("wait_for_cache_refresh returns TRUE when data changes on first poll", {
  result_baseline <- fixture_homepage_result()
  result_changed <- fixture_homepage_result_changed()

  call_count <- 0L
  mock_fetch <- function(token) {
    call_count <<- call_count + 1L
    if (call_count == 1L) result_changed else result_baseline
  }

  testthat::local_mocked_bindings(fetch_homepage_performance = mock_fetch)

  baseline_db <- build_database_performance_table(result_baseline)
  baseline_bm <- build_benchmark_performance_table(result_baseline)

  out <- wait_for_cache_refresh(
    token = "fake",
    baseline_db = baseline_db,
    baseline_bm = baseline_bm,
    interval_min = 0.001,
    max_wait_min = 0.01
  )
  expect_true(out)
})

test_that("wait_for_cache_refresh returns FALSE on timeout when data never changes", {
  result_baseline <- fixture_homepage_result()

  mock_fetch <- function(token) result_baseline

  testthat::local_mocked_bindings(fetch_homepage_performance = mock_fetch)

  baseline_db <- build_database_performance_table(result_baseline)
  baseline_bm <- build_benchmark_performance_table(result_baseline)

  out <- wait_for_cache_refresh(
    token = "fake",
    baseline_db = baseline_db,
    baseline_bm = baseline_bm,
    interval_min = 0.001,
    max_wait_min = 0.02
  )
  expect_false(out)
})

test_that("build_database_performance_table and build_benchmark_performance_table work with fixtures", {
  result <- fixture_homepage_result()
  db_tbl <- build_database_performance_table(result)
  bm_tbl <- build_benchmark_performance_table(result)

  expect_s3_class(db_tbl, "tbl_df")
  expect_equal(nrow(db_tbl), 2L)
  expect_named(
    db_tbl,
    c("level", "type", "startDate", "endDate", "performance")
  )
  expect_equal(db_tbl$performance[1], 0.02)

  expect_s3_class(bm_tbl, "tbl_df")
  expect_equal(nrow(bm_tbl), 1L)
  expect_named(
    bm_tbl,
    c(
      "level",
      "name",
      "entity",
      "entityId",
      "type",
      "startDate",
      "endDate",
      "performance"
    )
  )
  expect_equal(bm_tbl$performance[1], 0.03)
})
