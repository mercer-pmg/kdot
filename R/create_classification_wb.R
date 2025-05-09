#' Create a classification workbook to audit Orion products.
#'
#' @param all_local an XLSX exported from Orion Local Products page.
#'
#' @returns a list
#' @export
#'
#'
create_classification_wb <- function(all_local) {

  `Auto Assigned` <- AUM <- CUSIP <- `Product Sub Type Name` <- n <- NULL

  unreviewed <- all_local |> dplyr::filter(`Auto Assigned`)

  unreviewed_aum <- unreviewed |> dplyr::filter(AUM != 0)

  # Remove CUSIPs with underscore, dash
  unreviewed_aum <- unreviewed_aum |>
    dplyr::filter(!stringr::str_detect(CUSIP, "_")) |>
    dplyr::filter(!stringr::str_detect(CUSIP, "-"))

  dat <- unreviewed_aum

  dat <- dat |>
    dplyr::mutate(
      `Product Sub Type Name` = `Product Sub Type Name` |> stringr::str_remove_all("/"),
      `Assigned Asset Class`  = NA)

  keys_n <- dat |>
    dplyr::group_by(`Product Sub Type Name`) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::arrange(dplyr::desc(n)) |>
    dplyr::mutate(
      `Product Sub Type Name` = `Product Sub Type Name` |>
        stringr::str_replace_na()
    )

  dat <- dat |> dplyr::group_by(`Product Sub Type Name`)
  keys <- dplyr::group_keys(dat) |> dplyr::pull() |> stringr::str_replace_na()

  dat <- dat |> dplyr::group_split()
  dat <- dat |> purrr::map(as.data.frame)
  names(dat) <- keys

  return(dat)


}
