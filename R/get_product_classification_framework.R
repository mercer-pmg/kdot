#' Get Product Classification Framework from S3
#'
#' Retrieves the MA Product Classification Framework CSV file from AWS S3.
#'
#' @returns A data frame containing product classification framework information
#' @export
#'
#' @examples
#' \dontrun{
#' framework <- get_product_classification_framework()
#' }
get_product_classification_framework <- function() {
  framework <- aws.s3::get_object(
    region = Sys.getenv("AWS_DEFAULT_REGION"),
    key = Sys.getenv("AWS_ACCESS_KEY_ID"),
    secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
    object = "MA Product Classification Framework.csv",
    bucket = "aspen-investing-menu"
  ) |>
    rawToChar() |>
    readr::read_csv(show_col_types = FALSE)

  return(framework)
}
