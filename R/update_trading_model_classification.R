# Update Trading Model Classification

#' @noRd
.is_set_id <- function(x) {
  !is.null(x) && length(x) == 1L && !is.na(x)
}

#' Update Orion Trading Model `assetClassId` and/or `riskCategoryId`
#'
#' Fetches the model via GET `/api/v1/Trading/Models/{id}`, applies ID updates,
#' then PUTs the full JSON payload. Uses \code{orion_request()} for the PUT so empty
#' or non-JSON success bodies (e.g. 204) do not fail parsing.
#'
#' @param model_id Orion trading model id (integer-coercible).
#' @param token Orion API bearer token.
#' @param asset_class_id New `assetClassId`, or `NULL` / `NA` to leave unchanged.
#' @param risk_category_id New `riskCategoryId`, or `NULL` / `NA` to leave unchanged.
#'
#' @return A list with `success` (logical), `model_id`, `message` (character),
#'   and `model_name` (character, may be `NA`).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' token <- get_token()
#' update_trading_model_classification(
#'   12345L,
#'   token,
#'   risk_category_id = 99L
#' )
#' }
update_trading_model_classification <- function(
  model_id,
  token,
  asset_class_id = NULL,
  risk_category_id = NULL
) {
  model_id <- suppressWarnings(as.integer(model_id))
  if (is.na(model_id)) {
    return(list(
      success = FALSE,
      model_id = NA_integer_,
      message = "Invalid model_id",
      model_name = NA_character_
    ))
  }

  if (!.is_set_id(asset_class_id) && !.is_set_id(risk_category_id)) {
    return(list(
      success = FALSE,
      model_id = model_id,
      message = "No assetClassId or riskCategoryId to apply (both NULL or NA)",
      model_name = NA_character_
    ))
  }

  endpoint <- paste0("/api/v1/Trading/Models/", model_id)

  get_result <- orion_request_json(
    method = "GET",
    endpoint = endpoint,
    token = token,
    error_context = paste("GET model", model_id),
    simplify_vector = FALSE
  )

  if (!get_result$success) {
    msg <- if (!is.null(get_result$error)) get_result$error else "GET failed"
    return(list(
      success = FALSE,
      model_id = model_id,
      message = as.character(msg),
      model_name = NA_character_
    ))
  }

  model <- get_result$data
  model_name <- if (!is.null(model$name)) {
    as.character(model$name)
  } else {
    NA_character_
  }

  if (.is_set_id(asset_class_id)) {
    model$assetClassId <- as.integer(asset_class_id)
  }
  if (.is_set_id(risk_category_id)) {
    model$riskCategoryId <- as.integer(risk_category_id)
  }

  put_result <- orion_request(
    method = "PUT",
    endpoint = endpoint,
    json_data = model,
    token = token,
    error_context = paste("PUT model", model_id)
  )

  if (!put_result$success) {
    msg <- if (!is.null(put_result$error)) {
      put_result$error
    } else {
      paste("HTTP", put_result$status_code)
    }
    return(list(
      success = FALSE,
      model_id = model_id,
      message = as.character(msg),
      model_name = model_name
    ))
  }

  list(
    success = TRUE,
    model_id = model_id,
    message = "Updated",
    model_name = model_name
  )
}

#' @rdname update_trading_model_classification
#'
#' @param df A data frame with column `modelId` (required). Optional columns
#'   `riskCategoryId` and `assetClassId`. Rows may use `NA` or blanks to skip
#'   a field. Also accepts snake_case names `model_id`, `risk_category_id`,
#'   `asset_class_id` when the camelCase name is absent.
#'
#' @return A data frame with columns `success`, `model_id`, `message`, `model_name`.
#'
#' @export
batch_update_trading_model_classifications <- function(df, token) {
  if (!is.data.frame(df) || nrow(df) < 1L) {
    stop("`df` must be a data frame with at least one row", call. = FALSE)
  }

  nm <- names(df)
  if ("model_id" %in% nm && !"modelId" %in% nm) {
    names(df)[nm == "model_id"] <- "modelId"
    nm <- names(df)
  }
  if ("risk_category_id" %in% nm && !"riskCategoryId" %in% nm) {
    names(df)[nm == "risk_category_id"] <- "riskCategoryId"
    nm <- names(df)
  }
  if ("asset_class_id" %in% nm && !"assetClassId" %in% nm) {
    names(df)[nm == "asset_class_id"] <- "assetClassId"
  }

  if (!"modelId" %in% names(df)) {
    stop(
      "Data frame must contain column `modelId` (or `model_id`)",
      call. = FALSE
    )
  }

  has_risk <- "riskCategoryId" %in% names(df)
  has_ac <- "assetClassId" %in% names(df)
  if (!has_risk && !has_ac) {
    stop(
      "Data frame must contain `riskCategoryId` and/or `assetClassId`",
      call. = FALSE
    )
  }

  .row_id <- function(row, col) {
    if (!col %in% names(row)) {
      return(NULL)
    }
    v <- row[[col]][1L]
    if (is.null(v)) {
      return(NULL)
    }
    if (
      length(v) != 1L ||
        is.na(v) ||
        (is.character(v) && !nzchar(trimws(as.character(v))))
    ) {
      return(NULL)
    }
    suppressWarnings(as.integer(v))
  }

  results <- vector("list", nrow(df))
  for (i in seq_len(nrow(df))) {
    row <- df[i, , drop = FALSE]
    mid <- suppressWarnings(as.integer(row$modelId[1L]))
    ac <- if (has_ac) .row_id(row, "assetClassId") else NULL
    rc <- if (has_risk) .row_id(row, "riskCategoryId") else NULL
    if (is.na(mid)) {
      results[[i]] <- list(
        success = FALSE,
        model_id = NA_integer_,
        message = "Invalid modelId",
        model_name = NA_character_
      )
    } else if (is.null(ac) && is.null(rc)) {
      results[[i]] <- list(
        success = FALSE,
        model_id = mid,
        message = "No riskCategoryId or assetClassId for this row",
        model_name = NA_character_
      )
    } else {
      results[[i]] <- update_trading_model_classification(
        model_id = mid,
        token = token,
        asset_class_id = ac,
        risk_category_id = rc
      )
    }
  }

  do.call(rbind, lapply(results, as.data.frame, stringsAsFactors = FALSE))
}
