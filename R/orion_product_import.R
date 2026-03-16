#' Create Orion Product Local Update import
#'
#' @param sheet_with_assignments filename of Product Classification workbook
#'
#' @returns a tibble
#' @export
#'
#'
orion_product_import <- function(sheet_with_assignments) {
  `ADV Asset Category` <- `Annual Income Rate` <- `Asset Class ID` <-
    `Assigned Asset Class` <- CUSIP <- Color <- `Federally Taxable` <-
      `Has Fees` <- `Is 13F Reportable` <- `Is ADV Reportable` <-
        `Is Auto Assigned` <- `Is Custodial Cash` <- `Is Disabled` <-
          `Is Managed` <- `Moody Bond Rating` <- `Product ID` <-
            `Product Name Override` <- `Product Status` <- `Risk Category ID` <-
              `S&P Bond Rating` <- `State Taxable` <- `Use Global Tax Setting` <- NULL

  path <- as.character(sheet_with_assignments)[[1L]]
  if (is.na(path) || !nzchar(trimws(path))) {
    stop("Invalid file path: path is empty or missing.")
  }
  path <- normalizePath(path, mustWork = FALSE)
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }
  if (file.info(path)$size == 0) {
    stop("File is empty: ", path)
  }

  framework <- product_classification_framework

  sheet_names <- tryCatch(
    openxlsx::getSheetNames(path),
    error = function(e) {
      msg <- conditionMessage(e)
      hint <- " Close the file in Excel if it is open, or ensure it is fully downloaded (e.g. from OneDrive)."
      stop("Cannot read workbook: ", msg, hint, call. = FALSE)
    }
  )
  bad_names <- c("__FDSCACHE__", "Asset Classes", "Status")
  sheet_names <- sheet_names[!sheet_names %in% bad_names]

  get_assignments <- function(sheet_name) {
    print(sheet_name)

    dat <- openxlsx::read.xlsx(
      xlsxFile = path,
      sheet = sheet_name,
      sep.names = " "
    ) |>
      dplyr::select(`Product ID`, CUSIP, `Assigned Asset Class`) |>
      dplyr::mutate(
        CUSIP = as.character(CUSIP),
        `Assigned Asset Class` = as.character(`Assigned Asset Class`)
      )

    return(dat)
  }

  upload <- sheet_names |> purrr::map_df(get_assignments)

  upload <- upload |>
    dplyr::filter(!is.na(`Assigned Asset Class`)) |>
    dplyr::mutate(`Asset Class` = `Assigned Asset Class`) |>
    dplyr::select(-`Assigned Asset Class`)

  upload <- dplyr::left_join(
    x = upload,
    y = framework,
    by = "Asset Class"
  )

  upload <- upload |>
    dplyr::select(`Product ID`, `Risk Category ID`, `Asset Class ID`)

  upload <- upload |>
    dplyr::mutate(
      `Product Name Override` = NA,
      Color = NA,
      `Is Auto Assigned` = FALSE,
      `S&P Bond Rating` = NA,
      `Moody Bond Rating` = NA,
      `Product Status` = NA,
      `Is Disabled` = NA,
      `Is Custodial Cash` = NA,
      `Is Managed` = NA,
      `Use Global Tax Setting` = NA,
      `Federally Taxable` = NA,
      `State Taxable` = NA,
      `Annual Income Rate` = NA,
      `ADV Asset Category` = NA,
      `Is ADV Reportable` = NA,
      `Is 13F Reportable` = NA,
      `Has Fees` = NA
    )

  upload <- upload |>
    dplyr::select(
      `Product ID`,
      `Product Name Override`,
      `Asset Class ID`,
      `Risk Category ID`,
      `Color`,
      `Is Auto Assigned`,
      `S&P Bond Rating`,
      `Moody Bond Rating`,
      `Product Status`,
      `Is Disabled`,
      `Is Custodial Cash`,
      `Is Managed`,
      `Use Global Tax Setting`,
      `Federally Taxable`,
      `State Taxable`,
      `Annual Income Rate`,
      `ADV Asset Category`,
      `Is ADV Reportable`,
      `Is 13F Reportable`,
      `Has Fees`
    )

  return(upload)
}
