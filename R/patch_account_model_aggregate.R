# Patch Account Model Aggregate Function

#' Update Account Model Aggregate
#'
#' Updates the model aggregate assignment for an account.
#'
#' @param account_id The account ID
#' @param model_agg_id The model aggregate ID to assign
#' @param token API authentication token
#'
#' @return A standardized result list indicating success or error
#'
#' @export
#'
#' @examples
#' \dontrun{
#' token <- get_token()
#' result <- patch_account_model_aggregate(
#'   account_id = "12345",
#'   model_agg_id = 100,
#'   token = token
#' )
#' if (result$success) {
#'   message("Model aggregate updated successfully")
#' }
#' }
patch_account_model_aggregate <- function(account_id, model_agg_id, token) {
    result <- orion_request(
        method = "PATCH",
        endpoint = paste0("/api/v1/Portfolio/Accounts/", account_id, "/ModelAgg/", model_agg_id),
        token = token,
        error_context = paste("Updating model aggregate", model_agg_id, "for Account ID", account_id)
    )

    standardize_result(result$success, if (!result$success) result$error else NULL, NULL)
}
