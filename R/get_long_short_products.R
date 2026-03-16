#' Get Long/Short Products from S3
#'
#' Retrieves the long/short products CSV file from AWS S3.
#'
#' @returns A data frame containing product information (Product ID, Product Name, Ticker)
#' @export
#'
#' @examples
#' \dontrun{
#' products <- get_long_short_products()
#' }
get_long_short_products <- function() {
  products <- aws.s3::get_object(
    region = Sys.getenv("AWS_DEFAULT_REGION"),
    key = Sys.getenv("AWS_ACCESS_KEY_ID"),
    secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
    object = "long-short-products.csv",
    bucket = "aspen-investing-menu"
  ) |>
    rawToChar() |>
    readr::read_csv(show_col_types = FALSE)

  return(products)
}
