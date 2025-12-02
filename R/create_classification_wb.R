#' Create a classification workbook to audit Orion products.
#'
#' @param all_local a CSV exported from the Orion Local Products page or transformed Orion Query 10635.
#' @param include_predictions logical, whether to include prediction columns. Defaults to TRUE.
#'
#' @returns an XLSX workbook
#' @export
#'
#'
create_classification_wb <- function(all_local, include_predictions = TRUE) {
  print("Starting classification workbook creation...")

  AUM <- `Auto Assigned` <- CUSIP <- `Product Sub Type Name` <- n <-
    `Product ID` <- `Product Name` <- `Asset Class` <- `Asset Class Description` <- NULL

  # Helper function to predict asset class based on fuzzy string matching
  predict_asset_class <- function(product_name, reviewed_products) {
    # Handle edge cases
    if (is.na(product_name) || product_name == "" || nrow(reviewed_products) == 0) {
      return(list(
        predicted_class = NA_character_,
        confidence = NA_real_,
        matched_name = NA_character_
      ))
    }

    # Filter reviewed products that have an asset class description assigned
    valid_reviewed <- reviewed_products |>
      dplyr::filter(!is.na(`Asset Class Description`), !is.na(`Product Name`))

    if (nrow(valid_reviewed) == 0) {
      return(list(
        predicted_class = NA_character_,
        confidence = NA_real_,
        matched_name = NA_character_
      ))
    }

    # Calculate string similarity scores
    similarities <- stringdist::stringsim(
      product_name,
      valid_reviewed$`Product Name`,
      method = "lv" # Levenshtein distance
    )

    # Find the best match
    best_match_idx <- which.max(similarities)
    best_score <- similarities[best_match_idx]

    return(list(
      predicted_class = valid_reviewed$`Asset Class Description`[best_match_idx],
      confidence = round(best_score, 3),
      matched_name = valid_reviewed$`Product Name`[best_match_idx]
    ))
  }

  # DF of reviewed products
  print("Filtering reviewed products...")
  reviewed <- all_local |>
    dplyr::filter(!`Auto Assigned`) |>
    dplyr::mutate(`Assigned Asset Class` = NA)
  print(paste("  -", nrow(reviewed), "already classified products found"))

  # DF of unreviewed products
  print("Filtering unreviewed products...")
  unreviewed <- all_local |> dplyr::filter(`Auto Assigned`)
  print(paste("  -", nrow(unreviewed), "unreviewed products found"))

  # DF of unreviewed products with AUM
  unreviewed_aum <- unreviewed |>
    dplyr::filter(AUM != 0)
  print(paste("  -", nrow(unreviewed_aum), "unreviewed products with AUM"))


  zero_aum_funds <- unreviewed |>
    dplyr::filter(`Product Sub Type Name` %in% c("ETF", "Mutual Fund")) |>
    dplyr::filter(!`Product ID` %in% unreviewed_aum$`Product ID`) |>
    dplyr::mutate(`Assigned Asset Class` = NA)
  print(paste("  -", nrow(zero_aum_funds), "zero AUM funds (ETF/Mutual Fund)"))

  # Add prediction columns to zero AUM funds (if enabled)
  if (include_predictions) {
    print("Generating predictions for zero AUM funds...")
    zero_predictions <- purrr::map(zero_aum_funds$`Product Name`, ~ predict_asset_class(.x, reviewed))
    zero_aum_funds <- zero_aum_funds |>
      dplyr::mutate(
        `Predicted Asset Class` = purrr::map_chr(zero_predictions, ~ .x$predicted_class),
        `Confidence` = purrr::map_dbl(zero_predictions, ~ .x$confidence),
        `Matched Product Name` = purrr::map_chr(zero_predictions, ~ .x$matched_name)
      )
    print("  - Zero AUM predictions complete")
  }



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
      `Assigned Asset Class`  = NA
    )

  # Add prediction columns (if enabled)
  if (include_predictions) {
    print("Generating predictions for unreviewed products with AUM...")
    predictions <- purrr::map(dat$`Product Name`, ~ predict_asset_class(.x, reviewed))
    dat <- dat |>
      dplyr::mutate(
        `Predicted Asset Class` = purrr::map_chr(predictions, ~ .x$predicted_class),
        `Confidence` = purrr::map_dbl(predictions, ~ .x$confidence),
        `Matched Product Name` = purrr::map_chr(predictions, ~ .x$matched_name)
      )
    print("  - Predictions complete")
  }

  # Create table of the number of products grouped in each Product Sub Type Name
  keys_n <- dat |>
    dplyr::group_by(`Product Sub Type Name`) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::arrange(dplyr::desc(n)) |>
    dplyr::mutate(
      `Product Sub Type Name` = `Product Sub Type Name` |> stringr::str_replace_na()
    )

  # Create workbook object
  dat <- dat |> dplyr::group_by(`Product Sub Type Name`)
  keys <- dplyr::group_keys(dat) |>
    dplyr::pull() |>
    stringr::str_replace_na()

  dat <- dat |> dplyr::group_split()
  dat <- dat |> purrr::map(as.data.frame)
  names(dat) <- keys

  # Sort dat to match keys_n order to ensure styling is applied to correct sheets
  dat <- dat[keys_n$`Product Sub Type Name`]



  # Add Segment column to Mutual Fund table
  # if("Mutual Fund" %in% names(dat)) {
  #   dat[["Mutual Fund"]] <- dat[["Mutual Fund"]] |> dplyr::mutate(Segment = NA)
  # }


  print("Creating workbook...")
  wb <- openxlsx::createWorkbook()

  # Add Status worksheet that shows the number of products that need
  # assignment by Product Sub Type Name
  print("Adding Status worksheet...")
  openxlsx::addWorksheet(wb, "Status")
  openxlsx::writeData(wb, "Status", keys_n)

  # Create worksheets for each Product Sub Type Name
  print("Creating worksheets for each product type...")
  if (length(dat) > 0) {
    for (i in 1:length(dat)) {
      print(paste("  - Adding worksheet:", names(dat)[i], "with", nrow(dat[[i]]), "products"))
      openxlsx::addWorksheet(wb, names(dat)[i])
      openxlsx::writeDataTable(wb, names(dat)[i], dat[[i]])
    }
  }

  # Create worksheet for zero AUM ETFs and Mutual Funds
  print("Adding Zero AUM worksheet...")
  openxlsx::addWorksheet(wb, "Zero AUM")
  openxlsx::writeDataTable(wb, "Zero AUM", zero_aum_funds)

  # Create worksheet of all already classified products
  # Add empty Assigned Asset Class column
  print("Adding Classified worksheet...")
  reviewed_with_col <- reviewed |>
    dplyr::mutate(
      `Assigned Asset Class` = NA
    )

  openxlsx::addWorksheet(wb, "Classified")
  openxlsx::writeDataTable(wb, "Classified", reviewed_with_col)

  # Add hidden worksheet of current product classification framework
  print("Adding hidden Asset Classes reference worksheet...")
  framework <- product_classification_framework

  openxlsx::addWorksheet(wb, "Asset Classes", visible = FALSE)
  openxlsx::writeData(wb, "Asset Classes", x = framework)

  # Create data validation in Asset Class Assignment columns
  print("Applying data validation and styling...")
  add_classes <- function(x) {
    openxlsx::dataValidation(
      wb    = wb,
      sheet = x,
      cols  = 13,
      rows  = 2:(keys_n$n[i] + 1),
      type  = "list",
      value = paste0("'Asset Classes'!$D$2:$D$", (nrow(framework) + 1))
    )

    openxlsx::addStyle(
      wb    = wb,
      sheet = x,
      style = openxlsx::createStyle(fgFill = "#C5D9F1"),
      cols  = 13,
      rows  = 2:(keys_n$n[i] + 1)
    )
  }

  for (i in 1:length(keys)) {
    add_classes(keys_n$`Product Sub Type Name`[i])
  }

  openxlsx::dataValidation(
    wb    = wb,
    sheet = "Classified",
    cols  = 13,
    rows  = 2:(nrow(reviewed_with_col) + 1),
    type  = "list",
    value = paste0("'Asset Classes'!$D$2:$D$", (nrow(framework) + 1))
  )

  openxlsx::addStyle(
    wb    = wb,
    sheet = "Classified",
    style = openxlsx::createStyle(fgFill = "#C5D9F1"),
    cols  = 13,
    rows  = 2:(nrow(reviewed_with_col) + 1)
  )

  openxlsx::dataValidation(
    wb    = wb,
    sheet = "Zero AUM",
    cols  = 13,
    rows  = 2:(nrow(zero_aum_funds) + 1),
    type  = "list",
    value = paste0("'Asset Classes'!$D$2:$D$", (nrow(framework) + 1))
  )

  openxlsx::addStyle(
    wb    = wb,
    sheet = "Zero AUM",
    style = openxlsx::createStyle(fgFill = "#C5D9F1"),
    cols  = 13,
    rows  = 2:(nrow(zero_aum_funds) + 1)
  )


  # Add FactSet functions
  if ("Mutual Fund" %in% names(dat)) {
    print("Adding FactSet formulas to Mutual Fund worksheet...")
    # Column position depends on whether predictions are included
    # Without predictions: column 14, with predictions: column 17 (3 prediction columns)
    factset_col <- if (include_predictions) 17 else 14
    openxlsx::writeFormula(
      wb       = wb,
      sheet    = "Mutual Fund",
      x        = paste0("FDS(D", 2:(nrow(dat[["Mutual Fund"]]) + 1), ',"FFD_SEG")'),
      startRow = 2,
      startCol = factset_col
    )
  }

  # Apply styling to prediction columns (only if predictions are included)
  if (include_predictions) {
    print("Applying styling to prediction columns...")

    # Create explanatory text style for prediction columns (italic, gray text, gray background)
    explanatory_style <- openxlsx::createStyle(
      fontColour = "#7F7F7F",
      fontSize = 10,
      fgFill = "#F2F2F2",
      textDecoration = "italic"
    )

    # Create percentage style for confidence column
    percentage_style <- openxlsx::createStyle(
      fontColour = "#7F7F7F",
      fontSize = 10,
      fgFill = "#F2F2F2",
      textDecoration = "italic",
      numFmt = "0%"
    )

    # Apply explanatory style to each product type sheet
    for (i in 1:length(keys)) {
      sheet_name <- keys_n$`Product Sub Type Name`[i]
      n_rows <- keys_n$n[i]

      print(paste("  - Styling worksheet:", sheet_name))

      # Apply explanatory text style to prediction columns 14 and 16
      openxlsx::addStyle(
        wb = wb,
        sheet = sheet_name,
        style = explanatory_style,
        cols = c(14, 16),
        rows = 2:(n_rows + 1),
        gridExpand = TRUE,
        stack = TRUE
      )

      # Apply percentage style to Confidence column (15)
      openxlsx::addStyle(
        wb = wb,
        sheet = sheet_name,
        style = percentage_style,
        cols = 15,
        rows = 2:(n_rows + 1),
        gridExpand = TRUE,
        stack = TRUE
      )
    }

    # Apply styling to Zero AUM worksheet
    print("  - Styling Zero AUM worksheet")
    openxlsx::addStyle(
      wb = wb,
      sheet = "Zero AUM",
      style = explanatory_style,
      cols = c(14, 16),
      rows = 2:(nrow(zero_aum_funds) + 1),
      gridExpand = TRUE,
      stack = TRUE
    )

    openxlsx::addStyle(
      wb = wb,
      sheet = "Zero AUM",
      style = percentage_style,
      cols = 15,
      rows = 2:(nrow(zero_aum_funds) + 1),
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  # Autofit column widths for all worksheets
  print("Autofitting column widths...")
  all_sheets <- openxlsx::sheets(wb)

  for (sheet_name in all_sheets) {
    print(paste("  - Autofitting columns in:", sheet_name))

    # Get the number of columns in the sheet
    if (sheet_name == "Status") {
      n_cols <- 2 # Status sheet has 2 columns
    } else if (sheet_name == "Asset Classes") {
      n_cols <- ncol(framework)
    } else if (sheet_name == "Classified") {
      n_cols <- ncol(reviewed_with_col)
    } else if (sheet_name == "Zero AUM") {
      n_cols <- ncol(zero_aum_funds)
    } else if (sheet_name %in% names(dat)) {
      n_cols <- ncol(dat[[sheet_name]])
    } else {
      next # Skip if we can't determine columns
    }

    # Set column widths to auto
    openxlsx::setColWidths(
      wb = wb,
      sheet = sheet_name,
      cols = 1:n_cols,
      widths = "auto"
    )
  }

  print("Workbook creation complete!")
  return(wb)
}
