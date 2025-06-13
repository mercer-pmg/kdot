#' Get Aspen Investing Platform
#'
#' @returns a data frame
#' @export
#'
#'
get_platform <- function() {

  platform <- aws.s3::get_object(
    region = Sys.getenv("AWS_DEFAULT_REGION"),
    key    = Sys.getenv("AWS_ACCESS_KEY_ID"),
    secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),

    object = "orion-platform.csv",
    bucket = "aspen-investing-menu") |>
    readBin("character") |>
    readr::read_csv(show_col_types = FALSE)

  return(platform)

}
