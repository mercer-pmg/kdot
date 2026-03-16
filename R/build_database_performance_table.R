# Build Performance Table from API Response

#' Build Performance Table from API Response
#'
#' Converts the raw response from \code{\link{fetch_homepage_performance}} into
#' a flattened tibble with level, type, startDate, endDate, performance.
#'
#' @param result Raw API response from \code{\link{fetch_homepage_performance}}
#' @param level Level label for the calculations (default: "database")
#'
#' @return Tibble with columns: level, type, startDate, endDate, performance
#' @export
build_database_performance_table <- function(result, level = "database") {
  calcs <- result$data$database$calculations$calculations
  if (is.null(calcs)) {
    stop("No database calculations found in response")
  }
  .flatten_performance_calculations(calcs, level = level)
}

#' @keywords internal
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
.flatten_performance_calculations <- function(calcs, level = "database") {
  if (is.null(calcs) || length(calcs) == 0) {
    return(tibble::tibble(
      level = character(),
      type = character(),
      startDate = character(),
      endDate = character(),
      performance = double()
    ))
  }

  if (is.data.frame(calcs)) {
    calcs <- lapply(seq_len(nrow(calcs)), function(i) as.list(calcs[i, ]))
  }

  # Unwrap [[obj1, obj2]] -> [obj1, obj2]
  if (length(calcs) == 1L) {
    first <- calcs[[1]]
    if (is.list(first) && length(first) > 0 && !is.data.frame(first)) {
      fe <- first[[1]]
      if (
        is.recursive(fe) && ("series" %in% names(fe) || "Series" %in% names(fe))
      ) {
        calcs <- first
      }
    }
  }

  out <- lapply(seq_along(calcs), function(i) {
    item <- calcs[[i]]
    if (is.data.frame(item)) {
      item <- as.list(item)
    }
    if (!is.recursive(item)) {
      return(NULL)
    }

    id <- item[["id"]] %||% item[["Id"]] %||% ""
    id <- if (length(id) > 1) id[[1]] else id
    type <- if (grepl("cumulative", as.character(id), ignore.case = TRUE)) {
      "cumulative"
    } else {
      "standard"
    }

    series <- item[["series"]] %||% item[["Series"]] %||% list()
    if (length(series) == 0) {
      return(NULL)
    }
    if (is.data.frame(series)) {
      series <- lapply(seq_len(nrow(series)), function(ii) {
        as.list(series[ii, ])
      })
    }

    lapply(seq_along(series), function(s_idx) {
      s <- series[[s_idx]]
      if (!is.recursive(s)) {
        return(NULL)
      }
      block_type <- if (length(series) == 2L && s_idx == 2L) {
        "cumulative"
      } else {
        type
      }
      tibble::tibble(
        level = level,
        type = block_type,
        startDate = as.character(
          s[["startDate"]] %||% s[["StartDate"]] %||% NA_character_
        ),
        endDate = as.character(
          s[["endDate"]] %||% s[["EndDate"]] %||% NA_character_
        ),
        performance = as.numeric(
          s[["performance"]] %||% s[["Performance"]] %||% NA_real_
        )
      )
    })
  })

  flat <- unlist(out, recursive = FALSE)
  dplyr::bind_rows(Filter(Negate(is.null), flat))
}
