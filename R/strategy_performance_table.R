strategy_performance_table <- function(
    suite_name,
    portfolios = c(100, 80, 60, 40),
    platform = NA) {

  model <- portfolio <- strategy <- type <- tax_managed <- model_agg_id <-
    product <- ticker <- target <- agg_target <- model_agg <- NULL

  if(is.na(platform)) {
    platform <- kdot::get_platform()
  }

  dat <- platform |>
    dplyr::filter(model == suite_name) |>
    dplyr::filter(portfolio %in% portfolios)

  foo <- dat |>
    dplyr::select(-strategy, -type, -model, -tax_managed, -model_agg_id) |>
    dplyr::mutate(
      product = paste0(product, " (", ticker, ")"),
      target  = target/100*agg_target/100) |>
    dplyr::select(-ticker, -agg_target) |>
    tidyr::pivot_wider(
      id_cols     = c(model_agg, product),
      names_from  = portfolio,
      values_from = target,
      values_fill = 0) |>
    dplyr::select(model_agg, product, as.character(portfolios))

  # Rank and order model aggregates
  foo <- foo |>
    dplyr::mutate(
      rank = NA,

      rank = dplyr::if_else(
        condition = stringr::str_detect(model_agg, "US Large Cap"),
        true      = 10,
        false     = rank
      ),

      rank = dplyr::if_else(
        condition = stringr::str_detect(model_agg, "US Mid Cap"),
        true      = 20,
        false     = rank
      ),

      rank = dplyr::if_else(
        condition = stringr::str_detect(model_agg, "US Small Cap"),
        true      = 30,
        false     = rank
      ),

      rank = dplyr::if_else(
        condition = stringr::str_detect(model_agg, "Int'l Developed"),
        true      = 40,
        false     = rank
      ),

      rank = dplyr::if_else(
        condition = stringr::str_detect(model_agg, "Emerging Markets"),
        true      = 50,
        false     = rank
      ),

      rank = dplyr::if_else(
        condition = stringr::str_detect(model_agg, "Non Traditional"),
        true      = 60,
        false     = rank
      ),

      rank = dplyr::if_else(
        condition = stringr::str_detect(model_agg, "^MA Private"),
        true      = 70,
        false     = rank
      ),

      rank = dplyr::if_else(
        condition = stringr::str_detect(model_agg, "^MA Fixed Income"),
        true      = 80,
        false     = rank
      ),

      rank = dplyr::if_else(
        condition = stringr::str_detect(model_agg, "High Yield"),
        true      = 90,
        false     = rank
      )

    )

  foo <- foo |> dplyr::arrange(rank) |> dplyr::select(-rank)

  breakup <- function(df) {

    print(df)

    model_aggs <- df |> dplyr::pull(model_agg) |> unique()
    ls <- list()

    for(i in 1:length(model_aggs)) {

      products <- df |>
        dplyr::filter(model_agg == model_aggs[i]) |>
        dplyr::select(-model_agg)

      ls[[i]] <- list(
        model_agg = model_aggs[i],
        field     = products,
        rows      = nrow(products)
        )

    }

    return(ls)

  }

  foo <- breakup(foo)

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Performance")

  start_row = 1

  for(i in 1:length(foo)) {

    openxlsx::writeData(wb, "Performance", foo[[i]]$model_agg, startRow = start_row)
    openxlsx::writeData(wb, "Performance", foo[[i]]$field, startRow = start_row+1, colNames = FALSE)

    start_row <- start_row + foo[[i]]$rows + 1

  }

  openxlsx::saveWorkbook(wb, kdot::dated_filename("Performance", "xlsx"), overwrite = TRUE)






  return(foo)

}
