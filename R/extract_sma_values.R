# Extract SMA Values Function

#' Extract SMA Values from Account Data
#'
#' Extracts Separately Managed Account (SMA) values from account data.
#'
#' @param account_data The account data object (list) containing SMA information
#'
#' @return A list with:
#' \itemize{
#'   \item \code{isSMA}: Logical indicating if account is an SMA
#'   \item \code{eclipseSMA}: Character string with Eclipse SMA value
#'   \item \code{smaAssetID}: Integer asset ID for SMA (NULL if not applicable)
#' }
#'
#' @keywords internal
extract_sma_values <- function(account_data) {
    sma <- NULL
    if (is.list(account_data)) sma <- account_data$sma

    if (is.null(sma) || !is.list(sma)) {
        return(list(isSMA = FALSE, eclipseSMA = "False", smaAssetID = NULL))
    }

    is_sma <- if (!is.null(sma$isSma)) as.logical(sma$isSma) else FALSE
    if (length(is_sma) == 0 || is.na(is_sma)) is_sma <- FALSE

    list(
        isSMA = is_sma,
        eclipseSMA = if (!is.null(sma$eclipseSMA)) as.character(sma$eclipseSMA) else "False",
        smaAssetID = if (!is.null(sma$smaAssetId)) as.integer(sma$smaAssetId) else NULL
    )
}
