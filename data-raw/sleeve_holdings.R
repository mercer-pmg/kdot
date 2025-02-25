## code to prepare `sleeve_holdings` dataset goes here

process_env_holdings_export <- function(portfolio_name, exports_folder_path){

  export_path <- paste(exports_folder_path, portfolio_name, sep = "/") |>
    paste0(".csv")

  # Read in export.
  portfolio_holdings_export <- readr::read_csv(
    file           = export_path,
    show_col_types = FALSE)

  # Format column names.
  dat <- portfolio_holdings_export |> dplyr::rename_all(stringr::str_to_lower)

  # Create date column
  dat <- dat |>
    dplyr::mutate(
      date = `history date` |> lubridate::as_date(format = "%b %d, %Y"))

  # Determine if each portfolio holding is a sleeve or a security.
  sleeve_or_security <- function(x){

    symbol <- x |> purrr::map_chr(stringr::str_split_i, pattern = " -- ", i = 1)
    name   <- x |> purrr::map_chr(stringr::str_split_i, pattern = " -- ", i = 2)

    test1 <- nchar(symbol) == 8
    test2 <- stringr::str_detect(symbol, "-" )
    test3 <- stringr::str_sub(name, end = 3) == "MA "

    result <- sum(test1, test2, test3)

    if(result == 0){
      y <- "security"
    } else {
      if(result == 3){
        y <- "sleeve"
      } else{
        y <- "unsure"
      }
    }

    return(y)
  }

  dat <- dat |>
    dplyr::mutate(
      type = position |> purrr::map_chr(sleeve_or_security))

  # Split position column into position symbol and position name.
  dat <- dat |>
    dplyr::mutate(
      symbol = position |> stringr::str_split_i(" -- ", i = 1),
      name   = position |> stringr::str_split_i(" -- ", i = 2))

  # Format target column
  dat <- dat |>
    dplyr::mutate(
      target = target |> stringr::str_remove("%") |> as.numeric(),
      target = target/100)

  # Drop unnecessary columns, rows with a target weight, and duplicate rows.
  dat <- dat |>
    dplyr::select(date, instruction, symbol, name, type, target) |>
    tidyr::drop_na(target) #|>
    # dplyr::distinct()

  # Some exports have multiple holdings entries on the same day with different
  # target weights. We deal with these cases by keeping just the final entry for
  # that holding made that day. The assumption is that the final entry was the
  # target weight carried into the future. We we define the function to do this
  # work, and then apply it to our data and remove holdings with a zero target
  # weight.

  remove_dups <- function(df){

    # Find the multiple date/symbol entries
    dup_entries <- df |>
      dplyr::group_by(date, symbol) |>
      dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
      dplyr::filter(n != 1)

    waiting_room <- df

    # If the data does not have multiple date/symbol entries, just return the
    # data. If the data does have multiple data/symbol entries, for each
    # instance: isolate the instance, keep only the final entry, and return just
    # the final entry to the rest of the data.

    if(length(dup_entries$date) == 0){} else {

      for(i in 1:length(dup_entries$date)){

        # Isolate instance
        waiting_room <- waiting_room |>
          dplyr::filter(
            date != dup_entries$date[i] | symbol != dup_entries$symbol[i])

        isolated_instance <- dat |>
          dplyr::filter(
            date == dup_entries$date[i] & symbol == dup_entries$symbol[i])

        # Takes the last value for a security's target weight
        final_entry <- isolated_instance |> tail(1)

        # Return final entry to the rest of the data.
        waiting_room <- waiting_room |> dplyr::bind_rows(final_entry) |> dplyr::arrange(date)

      }
    }

    dat <- waiting_room

    return(dat)
  }

  dat <- dat |> remove_dups() |> dplyr::filter(target != 0)

  # Some model portfolio holdings are sleeves of holdings.Typically, sleeve
  # holdings are securities, though it's possible that some sleeve holdings are
  # sleeves themselves. In some cases we want to identify the security level
  # holdings of a model portfolio or sleeve. Therefore, we need break open the
  # sleeve holding to reveal the security holdings inside. We define two
  # functions used to break open these sleeve holdings and return the relevant
  # security-level holdings, and then apply it to our data. Note: the function
  # relies on the processed sleeve holdings data, so sleeves should be processed
  # before model portfolio.

  holdings_on_date <- function(model, on_date){
    on_date <- lubridate::as_date(on_date)

    dat <- readr::read_csv(
      file = paste0(ex, model, ".csv"),
      show_col_types = FALSE) |>
      dplyr::filter(date <= on_date) |>
      dplyr::slice_max(date) |>
      dplyr::mutate(date = on_date)

    return(dat)
  }

  explode_sleeves <- function(x, df = dat_sleeves){

    # Get the sleeve holdings on date x.
    y <- df |>
      dplyr::filter(date == x) |>
      dplyr::mutate(weight = target) |>
      dplyr::filter(weight > 0) |>
      dplyr::select(start_date, end_date, name, weight)

    all_change_dates <- c(x) # Date x is always a change date

    # Get the dates when each sleeve had a change in its holdings.
    for(i in 1:length(y$name)){

      ch_ch_changes <- readr::read_csv(
        paste0(parent_path, y$name[i],".csv"),
        show_col_types = FALSE) |>
        dplyr::filter(date >= y$start_date[i]) |>
        dplyr::filter(date < y$end_date[i]) |>
        dplyr::pull(date) |>
        unique()

      all_change_dates <- c(all_change_dates, ch_ch_changes)
    }

    all_change_dates <- unique(all_change_dates)

    # Get the holdings of each sleeve on each change date.
    sleeves_and_dates <- tidyr::expand_grid(all_change_dates, unique(y$name))
    names(sleeves_and_dates) <- c("date", "sleeve")

    holdings <- tibble::tibble()

    for(i in 1:length(sleeves_and_dates$date)){

      lando <- holdings_on_date(
        sleeves_and_dates$sleeve[i],
        sleeves_and_dates$date[i]) |>
        dplyr::mutate(name = sleeves_and_dates$sleeve[i])

      holdings <- dplyr::bind_rows(holdings, lando) |>
        dplyr::group_by(symbol)

    }

    # Multiply each sleeve's holdings' target weight by the sleeve's weight in
    # the portfolio.
    holdings <- holdings |>
      dplyr::left_join(y, by = "name") |>
      dplyr::mutate(model_weight = target*weight)

    return(holdings)
  }

  # Identify the period each target weight was applicable.
  change_dates <- dat |> dplyr::pull(date) |> unique()

  start_end_dates <- tibble::tibble(
    date       = change_dates,
    start_date = change_dates,
    end_date   = start_date |> dplyr::lead())

  # Add start and end dates to our data.
  dat <- dat |> dplyr::left_join(start_end_dates, by = "date")

  # Isolate the sleeve holdings in the data.
  dat_sleeves <- dat |> dplyr::filter(type == "sleeve")

  if(nrow(dat_sleeves) != 0){
    boom <- dat_sleeves |>
      dplyr::pull(date) |>
      unique() |>
      purrr::map_df(explode_sleeves, df = dat_sleeves) |>
      dplyr::group_by(date, symbol) |>
      dplyr::summarise(
        target  = sum(model_weight),
        .groups = "drop")
  } else {
    boom <- tibble::tibble()
  }

  # Combine the broken sleeves with the non-sleeve holdings.
  dat <- dat |>
    dplyr::filter(type != "sleeve") |>
    dplyr::bind_rows(boom) |>
    dplyr::arrange(date)

  # Remove unnecessary columns
  dat <- dat |> dplyr::select(date, symbol, target)

  # Round target weights
  dat <- dat |> dplyr::mutate(target = target |> round(5))

  # Now we want to check the total portfolio weight on each date in our data.
  # Our models have a 0.5% allocation to cash by default, so non-cash holdings
  # should equal 99.5% plus/minus a reasonable threshold to account for
  # rounding.

  # Calculate total target weights by date.
  check_weights <- dat |>
    dplyr::group_by(date) |>
    dplyr::summarise(total_weight = sum(target)) |>
    dplyr::mutate(within_threshold = abs(total_weight - 0.995) < 10^(-3))

  # Check whether total target weights by date equal 99.5% +/- a threshold.
  lg_check_weights <- check_weights |>
    dplyr::pull(within_threshold) |>
    prod() |>
    as.logical()

  # If the total portfolio weights are within the tolerance range, write data
  # to file. If not, alert with message and print the problem dates for
  # further investigation.

  if(lg_check_weights){

    dat <- dat |>
      dplyr::mutate(portfolio = portfolio_name) |>
      dplyr::select(date, portfolio, symbol, target) |>
      dplyr::filter(target != 0)

    return(dat)

  } else{
    print(
      paste(
        portfolio_name,
        "- Weights are incorrect. File has not been written to file."))

    print(check_weights |>
            dplyr::filter(!within_threshold) |>
            dplyr::select(-within_threshold))

    return()
  }
}
where_the_exports_are <- "C:/Users/DavidAllen/OneDrive - Mercer Advisors/Documents/MA Models/Model Portfolio Holdings/ENV Exports - July 2024"

exports <- tibble::tibble(
  filename = dir(where_the_exports_are)) |>
  dplyr::mutate(
    name = filename |> stringr::str_remove_all(".csv"),
    type = name |> stringr::str_sub(end = 3),
    type = dplyr::if_else(type == "MA ", "sleeve", "model portfolio"))

sleeves <- exports |>
  dplyr::filter(type == "sleeve") |>
  dplyr::pull(name)

sleeve_holdings <- sleeves |>
  purrr::map_df(
    process_env_holdings_export,
    exports_folder_path = where_the_exports_are)
