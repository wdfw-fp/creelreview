#' @name effort_checks
#' @title Angler effort count QAQC checks
#' @description Each function returns `list(result, detail)` where `result` is
#'   a one-row summary tibble and `detail` is flagged records or `NULL` on pass.
NULL


#' Check for effort end time before start time
#' @param data List of creel data from [creelutils::fetch_dwg()].
#' @returns list(result, detail)
#' @export
effort_end.time.start.time <- function(data) {

  flagged <- data$effort |>
    dplyr::filter(.data$effort_end_time < .data$effort_start_time)

  result <- create_results_table(
    pass           = nrow(flagged) == 0,
    critical       = TRUE,
    check_category = "record",
    check_type     = "effort",
    error_count    = nrow(flagged),
    message        = if (nrow(flagged) > 0)
      glue::glue("{nrow(flagged)} effort counts where end time < start time.")
    else "All effort end times are \u2265 start times." # >= symbol
  )

  detail <- if (nrow(flagged) > 0) {
    flagged |>
      dplyr::select("event_date", "water_body", "location", "count_sequence",
                    "count_type", "section_num", "effort_start_time",
                    "effort_end_time", "creel_event_id")
  }

  list(result = result, detail = detail)
}

#' Check for missing effort location
#' @param data List of creel data from [creelutils::fetch_dwg()].
#' @returns list(result, detail)
#' @export
effort_na.location <- function(data) {

  flagged <- data$effort |>
    dplyr::filter(is.na(.data$location) | .data$location == "")

  result <- create_results_table(
    pass           = nrow(flagged) == 0,
    critical       = TRUE,
    check_category = "record",
    check_type     = "effort",
    error_count    = nrow(flagged),
    message        = if (nrow(flagged) > 0)
      glue::glue("{nrow(flagged)} effort records with missing location.")
    else "All effort locations present."
  )

  detail <- if (nrow(flagged) > 0) {
    flagged |>
      dplyr::select("event_date", "water_body", "location", "section_num",
                    "count_type", "count_sequence", "creel_event_id")
  }

  list(result = result, detail = detail)
}

#' Check for missing count_quantity values
#' @param data List of creel data from [creelutils::fetch_dwg()].
#' @returns list(result, detail)
#' @export
effort_na.count.quantity <- function(data) {

  flagged <- data$effort |>
    dplyr::filter(is.na(.data$count_quantity))

  result <- create_results_table(
    pass           = nrow(flagged) == 0,
    critical       = TRUE,
    check_category = "record",
    check_type     = "effort",
    error_count    = nrow(flagged),
    message        = if (nrow(flagged) > 0)
      glue::glue("{nrow(flagged)} effort records with missing count_quantity.")
    else "All count_quantity values present."
  )

  detail <- if (nrow(flagged) > 0) {
    flagged |>
      dplyr::select("event_date", "water_body", "location", "section_num",
                    "count_type", "count_quantity", "creel_event_id")
  }

  list(result = result, detail = detail)
}

#' Check for missing count_quantity values
#' @param data List of creel data from [creelutils::fetch_dwg()].
#' @returns list(result, detail)
#' @export
effort_na.count.type <- function(data) {

  flagged <- data$effort |>
    dplyr::filter(is.na(.data$count_type) | .data$count_type == "")

  result <- create_results_table(
    pass           = nrow(flagged) == 0,
    critical       = TRUE,
    check_category = "record",
    check_type     = "effort",
    error_count    = nrow(flagged),
    message        = if (nrow(flagged) > 0)
      glue::glue("{nrow(flagged)} effort records with missing count_type.")
    else "All count_type values present."
  )

  detail <- if (nrow(flagged) > 0) {
    flagged |>
      dplyr::select("event_date", "water_body", "location", "section_num",
                    "count_type", "count_sequence", "creel_event_id")
  }

  list(result = result, detail = detail)
}

# 5 - Count sequence check
#  identify anomalies in daily effort counts by comparing them to the mode within specified location and event_id groups.
# Flag values that exceed or fall below the mode and returns flagged rows and a summary.
