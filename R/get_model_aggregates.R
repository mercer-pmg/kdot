#' Get Model Aggregates from S3
#'
#' Retrieves the model aggregates CSV file from AWS S3.
#'
#' @returns A data frame containing model aggregate information (modelAggId, modelName)
#' @export
#'
#' @examples
#' \dontrun{
#' model_aggs <- get_model_aggregates()
#' }
get_model_aggregates <- function() {
  model_aggs <- aws.s3::get_object(
    region = Sys.getenv("AWS_DEFAULT_REGION"),
    key = Sys.getenv("AWS_ACCESS_KEY_ID"),
    secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
    object = "model-aggs.csv",
    bucket = "aspen-investing-menu"
  ) |>
    rawToChar() |>
    readr::read_csv(show_col_types = FALSE)

  return(model_aggs)
}
