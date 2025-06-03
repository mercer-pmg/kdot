#' Create a classification workbook to audit Orion products.
#'
#' @param all_local an XLSX exported from Orion Local Products page.
#'
#' @returns a list
#' @export
#'
#'
create_classification_wb <- function(all_local) {

  AUM <- `Auto Assigned` <- CUSIP <- `Product Sub Type Name` <- n <-
  `Product ID` <- NULL

    # DF of reviewed products
    reviewed   <- all_local |>
      dplyr::filter(!`Auto Assigned`) |>
      dplyr::mutate(`Assigned Asset Class` = NA)

    # DF of unreviewed products
    unreviewed <- all_local |> dplyr::filter(`Auto Assigned`)

    # DF of unreviewed products with AUM
    unreviewed_aum <- unreviewed |>
      dplyr::filter(AUM != 0)


    zero_aum_funds <- unreviewed |>
      dplyr::filter(`Product Sub Type Name` %in% c("ETF", "Mutual Fund")) |>
      dplyr::filter(!`Product ID` %in% unreviewed_aum$`Product ID`) |>
      dplyr::mutate(`Assigned Asset Class` = NA)



    # Remove CUSIPs with underscore, dash from unreviewed_aum DF
    # unreviewed_aum <- unreviewed_aum |>
    #   dplyr::filter(!stringr::str_detect(CUSIP, "_")) |>
    #   dplyr::filter(!stringr::str_detect(CUSIP, "-"))

    # Rename unreviewed_aum
    dat <- unreviewed_aum

    # Prepare dat for group_split by Product Sub Type Name
    dat <- dat |>
      dplyr::mutate(

        # Remove slashes (not comparible with XLSX worksheet name)
        `Product Sub Type Name` = `Product Sub Type Name` |> stringr::str_remove_all("/"),

        # Create empty asset class assignment column
        `Assigned Asset Class`  = NA)

    # Create table of the number of products grouped in each Product Sub Type Name
    keys_n <- dat |>
      dplyr::group_by(`Product Sub Type Name`) |>
      dplyr::summarise(n = dplyr::n()) |>
      dplyr::arrange(dplyr::desc(n)) |>
      dplyr::mutate(
        `Product Sub Type Name` = `Product Sub Type Name` |> stringr::str_replace_na())

    # Create workbook object
    dat  <- dat |> dplyr::group_by(`Product Sub Type Name`)
    keys <- dplyr::group_keys(dat) |> dplyr::pull() |> stringr::str_replace_na()

    dat <- dat |> dplyr::group_split()
    dat <- dat |> purrr::map(as.data.frame)
    names(dat) <- keys



    # Add Segment column to Mutual Fund table

    if("Mutual Fund" %in% names(dat)) {
      dat[["Mutual Fund"]] <- dat[["Mutual Fund"]] |> dplyr::mutate(Segment = NA)
    }


    wb <- openxlsx::createWorkbook()

    # Add Status worksheet that shows the number of products that need
    # assignment by Product Sub Type Name
    openxlsx::addWorksheet(wb, "Status")
    openxlsx::writeData(wb, "Status", keys_n)

    # Create worksheets for each Product Sub Type Name
    for(i in 1:length(dat)) {
      openxlsx::addWorksheet(wb, names(dat)[i])
      openxlsx::writeDataTable(wb, names(dat)[i], dat[[i]])
    }

    # Create worksheet for zero AUM ETFs and Mutual Funds
    openxlsx::addWorksheet(wb, "Zero AUM")
    openxlsx::writeDataTable(wb, "Zero AUM", zero_aum_funds)

    # Create worksheet of all already classified products
    openxlsx::addWorksheet(wb, "Classified")
    openxlsx::writeDataTable(wb, "Classified", reviewed)

    # Add hidden worksheet of current product classification framework
    framework <- product_classification_framework

    openxlsx::addWorksheet(wb, "Asset Classes", visible = FALSE)
    openxlsx::writeData(wb, "Asset Classes", x = framework)

    # Create data validation in Asset Class Assignment columns
    add_classes <- function(x){

      openxlsx::dataValidation(
        wb    = wb,
        sheet = x,
        cols  = 13,
        rows  = 2:(keys_n$n[i]+1),
        type  = "list",
        value = paste0("'Asset Classes'!$D$2:$D$", (nrow(framework)+1)))

      openxlsx::addStyle(
        wb    = wb,
        sheet = x,
        style = openxlsx::createStyle(fgFill = "#C5D9F1"),
        cols  = 13,
        rows  = 2:(keys_n$n[i]+1))

    }

    for(i in 1:length(keys)){
      add_classes(keys_n$`Product Sub Type Name`[i])
    }

    openxlsx::dataValidation(
      wb    = wb,
      sheet = "Classified",
      cols  = 13,
      rows  = 2:(nrow(reviewed)+1),
      type  = "list",
      value = paste0("'Asset Classes'!$D$2:$D$", (nrow(framework)+1))
    )

    openxlsx::addStyle(
      wb    = wb,
      sheet = "Classified",
      style = openxlsx::createStyle(fgFill = "#C5D9F1"),
      cols  = 13,
      rows  = 2:(nrow(reviewed)+1)
    )

    openxlsx::dataValidation(
      wb    = wb,
      sheet = "Zero AUM",
      cols  = 13,
      rows  = 2:(nrow(zero_aum_funds)+1),
      type  = "list",
      value = paste0("'Asset Classes'!$D$2:$D$", (nrow(framework)+1))
    )

    openxlsx::addStyle(
      wb    = wb,
      sheet = "Zero AUM",
      style = openxlsx::createStyle(fgFill = "#C5D9F1"),
      cols  = 13,
      rows  = 2:(nrow(zero_aum_funds)+1)
    )


    # Add FactSet functions
    openxlsx::writeFormula(
      wb       = wb,
      sheet    = "Mutual Fund",
      x        = paste0('FDS(D', 2:(nrow(dat[["Mutual Fund"]])+1),',"FFD_SEG")'),
      startRow = 2,
      startCol = 14
    )


    return(wb)


}
