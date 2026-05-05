test_that("update_trading_model_classification returns error when no IDs to apply", {
  out <- update_trading_model_classification(1L, "tok", NULL, NULL)
  expect_false(out$success)
  expect_match(out$message, "No assetClassId or riskCategoryId")
})

test_that("update_trading_model_classification merges IDs and PUTs", {
  put_json <- NULL

  mock_json <- function(
    method,
    endpoint,
    token,
    error_context,
    ...,
    params = NULL,
    json_data = NULL,
    simplify_vector = TRUE,
    timeout = NULL
  ) {
    expect_equal(method, "GET")
    expect_equal(endpoint, "/api/v1/Trading/Models/42")
    list(
      success = TRUE,
      error = NULL,
      data = list(name = "M1", assetClassId = 1L, riskCategoryId = 2L)
    )
  }

  mock_put <- function(
    method,
    endpoint,
    json_data,
    token,
    error_context,
    params = NULL,
    timeout = NULL
  ) {
    expect_equal(method, "PUT")
    expect_equal(endpoint, "/api/v1/Trading/Models/42")
    put_json <<- json_data
    list(success = TRUE, status_code = 200L)
  }

  testthat::local_mocked_bindings(
    orion_request_json = mock_json,
    orion_request = mock_put
  )

  out <- update_trading_model_classification(
    42L,
    "fake-token",
    risk_category_id = 99L
  )

  expect_true(out$success)
  expect_equal(out$model_id, 42L)
  expect_equal(out$model_name, "M1")
  expect_equal(put_json$riskCategoryId, 99L)
  expect_equal(put_json$assetClassId, 1L)
})

test_that("update_trading_model_classification fails GET", {
  mock_json <- function(...) {
    list(success = FALSE, error = "nope", data = NULL)
  }
  mock_put <- function(...) stop("should not PUT")
  testthat::local_mocked_bindings(
    orion_request_json = mock_json,
    orion_request = mock_put
  )

  out <- update_trading_model_classification(1L, "t", risk_category_id = 3L)
  expect_false(out$success)
  expect_equal(out$message, "nope")
})

test_that("batch_update_trading_model_classifications normalizes snake_case", {
  mock_update <- function(
    model_id,
    token,
    asset_class_id = NULL,
    risk_category_id = NULL
  ) {
    list(
      success = TRUE,
      model_id = as.integer(model_id),
      message = "Updated",
      model_name = "x"
    )
  }
  testthat::local_mocked_bindings(
    update_trading_model_classification = mock_update
  )

  df <- data.frame(model_id = 1L, risk_category_id = 9L)
  res <- batch_update_trading_model_classifications(df, "t")
  expect_equal(nrow(res), 1L)
  expect_true(res$success[[1]])
})
