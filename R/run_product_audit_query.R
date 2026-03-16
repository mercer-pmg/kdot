# Run Product Audit Query Function

#' Run Product Audit with Custom Fields and Time-based Custom Fields - ALL
#'
#' Runs Custom Reporting Query 16897. The query has one prompt: Enter Date (Effective Date).
#' Mirrors orion-api \code{run_product_audit_query()}.
#'
#' @param effective_date Effective date for the audit (e.g. "3/16/2026")
#' @param format Output format: "csv", "Table", or "SlickGrid" (default: "Table")
#' @param token API token (optional, will use \code{get_token()} if not provided)
#'
#' @return A tibble containing the product audit results
#'
#' @export
#'
#' @examples
#' \dontrun{
#' results <- run_product_audit_query(effective_date = "3/16/2026")
#' nrow(results)  # number of products pulled
#' }
run_product_audit_query <- function(
  effective_date,
  format = "Table",
  token = NULL
) {
  run_orion_query(
    query_id = 16897L,
    prompt_updates = list("Enter Date" = effective_date),
    format = format,
    token = token
  )
}
