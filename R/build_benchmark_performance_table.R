# Build Benchmark Performance Table from API Response

#' Build Benchmark Performance Table from API Response
#'
#' Converts the raw response from \code{\link{fetch_homepage_performance}} into
#' a flattened tibble of benchmark performance with level (always "benchmark"),
#' name, entity, entityId, type, startDate, endDate, performance.
#'
#' @param result Raw API response from \code{\link{fetch_homepage_performance}}
#'
#' @return Tibble with columns: level, name, entity, entityId, type, startDate, endDate, performance
#' @export
build_benchmark_performance_table <- function(result) {
  calcs <- result$data$benchmarkPerformance$calculations
  if (is.null(calcs)) {
    stop("No benchmark performance calculations found in response")
  }
  .flatten_benchmark_performance_calculations(calcs)
}

#' @keywords internal
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
.flatten_benchmark_performance_calculations <- function(calcs) {
  if (is.null(calcs) || length(calcs) == 0) {
    return(tibble::tibble(
      level = character(),
      name = character(),
      entity = character(),
      entityId = integer(),
      type = character(),
      startDate = character(),
      endDate = character(),
      performance = double()
    ))
  }

  benchmarks <- .extract_benchmark_list(calcs)
  if (is.null(benchmarks) || length(benchmarks) == 0) {
    return(tibble::tibble(
      level = character(),
      name = character(),
      entity = character(),
      entityId = integer(),
      type = character(),
      startDate = character(),
      endDate = character(),
      performance = double()
    ))
  }

  if (is.data.frame(benchmarks)) {
    benchmarks <- lapply(seq_len(nrow(benchmarks)), function(i) {
      as.list(benchmarks[i, ])
    })
  }

  out <- lapply(seq_along(benchmarks), function(i) {
    bm <- benchmarks[[i]]
    if (is.data.frame(bm)) {
      bm <- as.list(bm)
    }
    if (!is.recursive(bm)) {
      return(NULL)
    }

    name <- as.character(bm[["name"]] %||% NA_character_)
    entity <- as.character(bm[["entity"]] %||% NA_character_)
    entity_id <- as.integer(bm[["entityId"]] %||% NA_integer_)

    inner_calcs <- bm[["calculations"]] %||% list()
    if (length(inner_calcs) == 0) {
      return(NULL)
    }
    if (is.data.frame(inner_calcs)) {
      inner_calcs <- lapply(seq_len(nrow(inner_calcs)), function(ii) {
        as.list(inner_calcs[ii, ])
      })
    }

    lapply(seq_along(inner_calcs), function(c_idx) {
      calc <- inner_calcs[[c_idx]]
      if (is.data.frame(calc)) {
        calc <- as.list(calc)
      }
      if (!is.recursive(calc)) {
        return(NULL)
      }

      id <- calc[["id"]] %||% ""
      id <- if (length(id) > 1) id[[1]] else id
      type <- if (grepl("cumulative", as.character(id), ignore.case = TRUE)) {
        "cumulative"
      } else {
        "standard"
      }

      series <- calc[["series"]] %||% calc[["calculations"]] %||% list()
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
          level = "benchmark",
          name = name,
          entity = entity,
          entityId = entity_id,
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
  })

  flat <- unlist(out, recursive = FALSE)
  dplyr::bind_rows(Filter(Negate(is.null), flat))
}

#' @keywords internal
.extract_benchmark_list <- function(calcs) {
  if (is.data.frame(calcs)) {
    all_benchmarks <- list()
    for (i in seq_len(nrow(calcs))) {
      row <- as.list(calcs[i, ])
      inner <- row[["calculations"]]
      if (!is.null(inner) && length(inner) > 0) {
        if (is.data.frame(inner)) {
          inner <- lapply(seq_len(nrow(inner)), function(j) as.list(inner[j, ]))
        }
        fe <- inner[[1]]
        if (is.recursive(fe) && any(c("entity", "name") %in% names(fe))) {
          all_benchmarks <- c(all_benchmarks, inner)
        }
      }
    }
    if (length(all_benchmarks) > 0) return(all_benchmarks)
  }

  x <- calcs
  if (is.data.frame(x)) {
    x <- lapply(seq_len(nrow(x)), function(i) as.list(x[i, ]))
  }
  while (length(x) > 0) {
    first <- x[[1]]
    if (is.data.frame(first)) {
      first <- as.list(first[1, ])
    }
    if (!is.recursive(first)) {
      return(x)
    }
    inner <- first[["calculations"]]
    if (!is.null(inner) && length(inner) > 0) {
      fe <- if (is.data.frame(inner)) inner[1, ] else inner[[1]]
      if (is.recursive(fe)) {
        if (any(c("entity", "name") %in% names(fe))) {
          return(inner)
        }
        x <- inner
      } else {
        return(x)
      }
    } else {
      return(x)
    }
  }
  x
}
