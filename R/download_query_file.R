# Download Query File Function

#' Download and Read Query Result File from Location URL
#'
#' Downloads a file from the provided location URL and reads it into a tibble.
#' Used for CSV format queries that return a 201 status with location header.
#'
#' @param location_url The URL from the location header
#' @param format File format (default: "csv")
#' @param token API token
#'
#' @return A tibble containing the query results
#'
#' @importFrom httr2 request req_headers req_progress req_timeout req_perform resp_status resp_body_raw
#' @importFrom tibble as_tibble
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # This function is typically called internally by generate_query_results()
#' # Example usage:
#' location_url <- "https://api.orionadvisor.com/query-results/12345.csv"
#' token <- get_token()
#' results <- download_query_file(location_url, format = "csv", token = token)
#' }
download_query_file <- function(location_url, format = "csv", token) {
    # Build request with longer timeout for file downloads
    req <- httr2::request(location_url) |>
        httr2::req_headers(
            "Authorization" = paste("Bearer", token),
            "Accept" = "*/*"
        ) |>
        httr2::req_progress() |>
        httr2::req_timeout(1200L) # 20 minutes for query generation

    resp <- httr2::req_perform(req)
    status_code <- httr2::resp_status(resp)

    if (status_code < 200 || status_code >= 300) {
        stop(sprintf("Failed to download query file: HTTP %d", status_code))
    }

    # Save to temporary file
    temp_file <- tempfile(fileext = paste0(".", format))
    writeBin(httr2::resp_body_raw(resp), temp_file)

    # Read file based on format
    if (format == "csv") {
        df <- read.csv(temp_file, stringsAsFactors = FALSE)
    } else {
        stop(sprintf("Unsupported file format: %s", format))
    }

    # Clean up temp file
    unlink(temp_file)

    return(tibble::as_tibble(df))
}
