#' Create security performance quilt in XLSX workbook
#'
#' @param wb XLSX workbook object
#' @param sheetName XSLX workbook sheet name.
#' @param indexes character vector of Bloomberg security IDs
#' @param frequency interval of performance calculation
#' @param start.date YYYY-MM-DD
#' @param end.date YYYY-MM-DD
#' @param summary If TRUE, the default displays summary of performance. If TRUE, no summary is displayed.
#' @param vol_frequency The return periods to use to calculated annualized volatility in the summary. By default matches the frequency of the quilt body.
#' @param n periods of performance to display
#' @param first_col "earliest" or "latest"
#' @param startRow row to place quilt
#' @param startCol column to place quilt
#' @param title title of performance quilt
#' @param labels labels to use inside quilt squares
#' @param palette fill colors to use in the quilt. Use Hex codes.
#'
#' @return a XLSX workbook object
#' @export
#'
#'

performance_quilt <- function(
    wb,
    sheetName,
    indexes,
    frequency,
    start.date,
    end.date      = Sys.Date(),
    summary       = TRUE,
    vol_frequency = "match",
    n             = NULL,
    first_col     = "latest",
    startRow      = 2,
    startCol      = 2,
    title         = NULL,
    labels        = NULL,
    palette       = NULL) {

  period <- security <- ret <- col_id <- row_id <- label <- f_return <- NULL
  CUST_TRR_RETURN_ANNUALIZED <- f_vol <- NULL


## CALC BODY--------------------------------------------------------------------

  start.date <- lubridate::as_date(start.date)
  end.date   <- lubridate::as_date(end.date)

  # Adjust for start dates that fall on weekend
  if(lubridate::wday(start.date, week_start = 1) %in% c(6,7)){
    start.date <- start.date - (lubridate::wday(start.date, week_start = 1) - 5)
  }


  # Calculate periodic returns
  returns <- tibble::tibble()

  if(frequency == "Y"){
    for(i in 1: length(indexes)){
      returns <- dplyr::bind_rows(
        returns,
        kdot::annual_returns(
          x          = indexes[i],
          start.date = start.date,
          end.date   = end.date
        )
      )
    }
  }

  if(frequency == "Q"){
    for(i in 1: length(indexes)){
      returns <- dplyr::bind_rows(
        returns,
        kdot::quarterly_returns(
          x          = indexes[i],
          start.date = start.date,
          end.date   = end.date
        )
      )
    }
  }

  if(frequency == "M"){
    for(i in 1: length(indexes)){
      returns <- dplyr::bind_rows(
        returns,
        kdot::monthly_returns(
          x          = indexes[i],
          start.date = start.date,
          end.date   = end.date
        )
      )
    }
  }

  if(frequency == "W"){
    for(i in 1: length(indexes)){
      returns <- dplyr::bind_rows(
        returns,
        kdot::weekly_returns(
          x          = indexes[i],
          start.date = start.date,
          end.date   = end.date
        )
      )
    }
  }

## FORMAT BODY------------------------------------------------------------------

  # Add a skip line to prevent errors when data is written to XLSX ¯\_(ツ)_/¯
  returns <- returns |> dplyr::mutate(period = paste0("\n", period))

  # Join returns with labels
  key     <- tibble::tibble(security = indexes, label = labels)
  returns <- returns |> dplyr::left_join(key, by = "security")

  # Set column IDs
  nrow <- length(indexes)
  ncol <- returns |> dplyr::pull(period) |> unique() |> length()

  if(first_col == "latest"){
    returns <- returns |>
      dplyr::arrange(security, dplyr::desc(date)) |>
      dplyr::mutate(col_id = rep(1:ncol, nrow))
  }

  if(first_col == "earliest"){
    returns <- returns |>
      dplyr::arrange(security, date) |>
      dplyr::mutate(col_id = rep(1:ncol, nrow))
  }

  # Set row IDs
  returns <- returns |>
    dplyr::arrange(period, dplyr::desc(ret)) |>
    dplyr::mutate(row_id = rep(1:nrow, ncol))

  returns <- returns |> dplyr::arrange(col_id, row_id)

  # Format labels
  returns <- returns |>
    dplyr::mutate(
      f_return = scales::label_percent(accuracy = 0.1)(ret),
      label    = paste(label, f_return, sep = "\n"))

  quilt <- returns |>
    dplyr::select(-col_id) |>
    tidyr::pivot_wider(
      id_cols     = row_id,
      names_from  = period,
      values_from = label) |>
    dplyr::arrange(row_id) |>
    dplyr::select(-row_id)


# CALC SUMMARY------------------------------------------------------------------
  if(summary){

    # Set the start and end dates of the summarized period.

    if(frequency == "Y"){
      summary_start <- start.date |> lubridate::ceiling_date("year") - 1

      last_day      <- end.date == lubridate::ceiling_date(end.date, "year") - 1
      if(last_day){
        summary_end <- end.date
      } else{
        summary_end <- end.date |> lubridate::floor_date("year") - 1
      }
    }

    if(frequency == "Q"){
      summary_start <- start.date |> lubridate::ceiling_date("quarter") - 1

      last_day      <- end.date == lubridate::ceiling_date(end.date, "quarter") - 1
      if(last_day){
        summary_end <- end.date
      } else{
        summary_end <- end.date |> lubridate::floor_date("quarter") - 1
      }
    }

    if(frequency == "M"){
      summary_start <- start.date |> lubridate::ceiling_date("month") - 1

      last_day      <- end.date == lubridate::ceiling_date(end.date, "month") - 1
      if(last_day){
        summary_end <- end.date
      } else{
        summary_end <- end.date |> lubridate::floor_date("month") - 1
      }
    }

    if(frequency == "W"){
      summary_start <- start.date |> lubridate::ceiling_date("week") - 1

      last_day      <- end.date == lubridate::ceiling_date(end.date, "week") - 1
      if(last_day){
        summary_end <- end.date
      } else{
        summary_end <- end.date |> lubridate::floor_date("week") - 1
      }
    }


    # Calculate volatility of returns over summary period
    returns_vol <- tibble::tibble()

    if(vol_frequency == "match"){returns_vol <- returns}

    if(vol_frequency == "Y"){
      for(i in 1: length(indexes)){
        returns_vol <- dplyr::bind_rows(
          returns_vol,
          kdot::annual_returns(
            x          = indexes[i],
            start.date = summary_start,
            end.date   = summary_end
          )
        )
      }
    }

    if(vol_frequency == "Q"){
      for(i in 1: length(indexes)){
        returns_vol <- dplyr::bind_rows(
          returns_vol,
          kdot::quarterly_returns(
            x          = indexes[i],
            start.date = summary_start,
            end.date   = summary_end
          )
        )
      }
    }

    if(vol_frequency == "M"){
      for(i in 1: length(indexes)){
        returns_vol <- dplyr::bind_rows(
          returns_vol,
          kdot::monthly_returns(
            x          = indexes[i],
            start.date = summary_start,
            end.date   = summary_end
          )
        )
      }
    }

    if(vol_frequency == "W"){
      for(i in 1: length(indexes)){
        returns_vol <- dplyr::bind_rows(
          returns_vol,
          kdot::weekly_returns(
            x          = indexes[i],
            start.date = summary_start,
            end.date   = summary_end
          )
        )
      }
    }

    # Set number of periods per year to use when calculating annual volatility
    if(vol_frequency == "match"){vol_frequency <- frequency}

    if(vol_frequency == "Y"){periods_per_year <- 1}
    if(vol_frequency == "Q"){periods_per_year <- 4}
    if(vol_frequency == "M"){periods_per_year <- 12}
    if(vol_frequency == "W"){periods_per_year <- 52}

    # Calculate annual volatility of each index over the summary period
    vol <- returns_vol |>
      dplyr::group_by(security) |>
      dplyr::summarise(vol = stats::sd(ret)*sqrt(periods_per_year)) |>
      dplyr::arrange(dplyr::desc(vol)) |>
      dplyr::left_join(key, by = "security") |>
      dplyr::mutate(row_id = 1:length(indexes) + startRow + 1) |>
      dplyr::mutate(
        f_vol    = scales::label_percent(accuracy = 0.1)(vol),
        f_vol    = paste(label, f_vol, sep = "\n"))

    over <- c("CUST_TRR_START_DT" = summary_start |> format("%Y%m%d"),
              "CUST_TRR_END_DT"   = summary_end   |> format("%Y%m%d"))

    returns_summary <-

      # Retrieve annualized returns from Bloomberg
      Rblpapi::bdp(
        securities = indexes,
        fields     = "CUST_TRR_RETURN_ANNUALIZED",
        overrides  = over) |>

      # Rename column of annualized returns
      dplyr::mutate(ret = CUST_TRR_RETURN_ANNUALIZED) |>
      dplyr::select(-CUST_TRR_RETURN_ANNUALIZED) |>

      # Add column of index names
      tibble::rownames_to_column("security") |>

      # Join annualized returns data with labels
      dplyr::left_join(key, by = "security") |>

      # Arrange annualized return in descending order
      dplyr::arrange(dplyr::desc(ret)) |>

      # Create quilt labels
      dplyr::mutate(
        f_return = scales::label_percent(accuracy = 0.1)(ret/100),
        f_return    = paste(label, f_return, sep = "\n")) |>

      # Add row ids
      dplyr::mutate(row_id = 1:length(indexes) + startRow + 1)

    # Write summary to XLSX workbook
    summary_quilt <- returns_summary |>
      dplyr::left_join(vol, by = "row_id") |>
      dplyr::select(f_return, f_vol) |>
      dplyr::mutate(
        `\nAnn` = f_return,
        `\nVol` = f_vol) |>
      dplyr::select(-f_return, -f_vol)


}

## WRITE DATA------------------------------------------------------------------

    ## Write title
    if(is.null(title)){title <- "Performance Quilt"}

    openxlsx::writeData(
      wb       = wb,
      sheet    = sheetName,
      x        = title,
      startCol = startCol,
      startRow = startRow
    )

    # Write quilt body
    openxlsx::writeData(
      wb       = wb,
      sheet    = sheetName,
      x        = quilt,
      startCol = startCol,
      startRow = startRow+1
    )

    # Rename last column header if necessary (YTD, MTD, QTD, etc.)
    last_day <- TRUE

    last_day <- if(frequency == "Y"){
      end.date == end.date |> lubridate::ceiling_date("year") - 1
    } else {
      if(frequency == "Q"){
        end.date == end.date |> lubridate::ceiling_date("quarter") - 1
      } else{
        if(frequency == "M"){
          end.date == end.date |> lubridate::ceiling_date("month") - 1
        } else{
          if(frequency == "W"){
            end.date == end.date |> lubridate::ceiling_date("week") - 1
          }
        }
      }
    }

    if(!last_day){
      openxlsx::writeData(
        wb       = wb,
        sheet    = sheetName,
        x        = paste0(frequency,"TD"),
        startCol = startCol + ncol - 1,
        startRow = startRow+1
      )
    }

    # Write quilt summary
    if(summary){

      if(frequency == "Y"){
        summary_header <- paste(
          (summary_start + 1) |> lubridate::year(),
          summary_end   |> lubridate::year(),
          sep = " - "
        )
      }

      if(frequency == "Q"){
        summary_header <- paste(
          paste(
            paste0(
              "Q",
              lubridate::quarter(summary_start+1)
            ),
            lubridate::year(summary_start+1)
          ),
          paste(
            paste0(
              "Q",
              lubridate::quarter(summary_end)
            ),
            lubridate::year(summary_end)
          ),
          sep = " - "
        )
      }



      openxlsx::writeData(
        wb       = wb,
        sheet    = sheetName,
        x        = summary_header,
        startCol = startCol + ncol + 1,
        startRow = startRow
      )

      openxlsx::writeData(
        wb       = wb,
        sheet    = sheetName,
        x        = summary_quilt,
        startCol = startCol+ ncol + 1,
        startRow = startRow+1
      )
    }


## STYLE-----------------------------------------------------------------------
    ## Add XLSX styles
    styles <- kdot::xlsx_styles()

    f <- function(x){openxlsx::createStyle(fgFill = x)}

    if(is.null(palette)){
      palette <- styles[["quilt_fills"]]
    } else {
      palette <- palette |> purrr::map(f)
    }

    openxlsx::addStyle(
      wb    = wb,
      sheet = sheetName,
      style = styles[["quilt_title"]],
      rows  = startRow,
      cols  = startCol)

    openxlsx::addStyle(
      wb    = wb,
      sheet = sheetName,
      style = styles[["quilt_header"]],
      cols  = startCol:(startCol+ncol),
      rows  = startRow+1)

    returns <- returns |>
      dplyr::mutate(
        col_id = col_id+startCol-1,
        row_id = row_id+startRow+1)

    for(i in 1:length(indexes)){
      openxlsx::addStyle(
        wb    = wb,
        sheet = sheetName,
        style = palette[[i]],
        cols  = startCol - 1 + (1:ncol),
        rows  = returns |>
          dplyr::filter(security == indexes[i]) |>
          dplyr::pull(row_id))}

    openxlsx::addStyle(
      wb         = wb,
      sheet      = sheetName,
      style      = styles[["quilt_body"]],
      rows       = (startRow+1):(startRow+nrow+1),
      cols       = startCol:(startCol+ncol),
      gridExpand = TRUE,
      stack      = TRUE)

    openxlsx::addStyle(
      wb    = wb,
      sheet = sheetName,
      style = styles[["quilt_header"]],
      cols  = startCol:(startCol+ncol),
      rows  = startRow+1)

    openxlsx::setRowHeights(
      wb      = wb,
      sheet   = sheetName,
      rows    = (startRow+2):(startRow+1+nrow),
      heights = 61.50)

    # Style quilt summary columns
    if(summary){

      for(i in 1:length(indexes)){
        openxlsx::addStyle(
          wb    = wb,
          sheet = sheetName,
          style = palette[[i]],
          cols  = startCol + ncol + 1,
          rows  = returns_summary |>
            dplyr::filter(security == indexes[i]) |>
            dplyr::pull(row_id)
        )
      }

      for(i in 1:length(indexes)){
        openxlsx::addStyle(
          wb    = wb,
          sheet = sheetName,
          style = palette[[i]],
          cols  = startCol + ncol + 2,
          rows  = vol |>
            dplyr::filter(security == indexes[i]) |>
            dplyr::pull(row_id)
        )
      }

      openxlsx::addStyle(
        wb         = wb,
        sheet      = sheetName,
        style      = styles[["quilt_body"]],
        rows       = (startRow+1):(startRow+nrow+1),
        cols       = startCol + ncol + 1:2,
        gridExpand = TRUE,
        stack      = TRUE
      )

      openxlsx::addStyle(
        wb    = wb,
        sheet = sheetName,
        style = styles[["quilt_header"]],
        cols  = startCol + ncol + 1:2,
        rows  = startRow+1
      )

      openxlsx::setColWidths(
        wb = wb,
        sheet = sheetName,
        cols = startCol + ncol,
        widths = .5
      )

      openxlsx::setRowHeights(
        wb = wb,
        sheet = sheetName,
        rows = startRow + 1,
        heights = 25
      )

      openxlsx::mergeCells(
        wb = wb,
        sheet = sheetName,
        cols = startCol + ncol + 1:2,
        rows = startRow
      )

      openxlsx::addStyle(
        wb    = wb,
        sheet = sheetName,
        style = styles[["quilt_summary_header"]],
        cols  = startCol + ncol + 1:2,
        rows  = startRow
      )


    }











  return(wb)

}
