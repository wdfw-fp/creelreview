#' Check for missing trip_status
#' @param data List of creel data from [creelutils::fetch_dwg()].
#' @returns `list(result, detail)` where `result` is
#'   a one-row summary tibble and `detail` is flagged records or `NULL` on pass.
#' @export
interview_na.trip.status <- function(data) {
  flagged <- data$interview |>
    dplyr::filter(is.na(.data$trip_status))

  result <- create_results_table(
    pass           = nrow(flagged) == 0,
    critical       = FALSE,
    check_category = "record",
    check_type     = "interview",
    error_count    = nrow(flagged),
    message        = if (nrow(flagged) > 0)
      glue::glue("{nrow(flagged)} interviews with missing trip_status.")
    else "All trip statuses present."
  )

  detail <- if (nrow(flagged) > 0) {
    flagged |>
      dplyr::select("event_date", "water_body", "section_num",
                    "interview_number", "trip_status", "creel_event_id")
  }

  list(result = result, detail = detail)
}


#' Check for fishing end time after interview time
#' @param data List of creel data from [creelutils::fetch_dwg()].
#' @returns `list(result, detail)` where result is
#'   a one-row summary tibble and `detail` is flagged records or `NULL` on pass.
#' @export
interview_end.time.interview.time <- function(data) {
  flagged <- data$interview |>
    dplyr::filter(.data$fishing_end_time > .data$interview_time)

  result <- create_results_table(
    pass           = nrow(flagged) == 0,
    critical       = FALSE,
    check_category = "record",
    check_type     = "interview",
    error_count    = nrow(flagged),
    message        = if (nrow(flagged) > 0)
      glue::glue("{nrow(flagged)} interviews where fishing end time > interview time.")
    else "All fishing end times are at or before the interview time."
  )

  detail <- if (nrow(flagged) > 0) {
    flagged |>
      dplyr::select("event_date", "water_body", "interview_number",
                    "fishing_end_time", "interview_time", "creel_event_id")
  }

  list(result = result, detail = detail)
}

#' Check for fishing start time after end time
#' @param data List of creel data from [creelutils::fetch_dwg()].
#' @returns `list(result, detail)` where `result` is
#'    a one-row summary tibble and `detail` is flagged records or `NULL` on pass.
#' @export
interview_start.time.end.time <- function(data) {
  flagged <- data$interview |>
    dplyr::filter(.data$fishing_start_time > .data$fishing_end_time)

  result <- create_results_table(
    pass           = nrow(flagged) == 0,
    critical       = FALSE,
    check_category = "record",
    check_type     = "interview",
    error_count    = nrow(flagged),
    message        = if (nrow(flagged) > 0)
      glue::glue("{nrow(flagged)} interviews where start time > end time.")
    else "All fishing start times are before the end time."
  )

  detail <- if (nrow(flagged) > 0) {
    flagged |>
      dplyr::select("event_date", "water_body", "interview_number",
                    "fishing_start_time", "fishing_end_time", "creel_event_id")
  }

  list(result = result, detail = detail)
}

#' Check for trailer count exceeding vehicle count
#' @param data List of creel data from [creelutils::fetch_dwg()].
#' @returns `list(result, detail)` where `result` is
#'    a one-row summary tibble and `detail` is flagged records or `NULL` on pass.
#' @export
interview_trailer.count.vehicle.count <- function(data) {
  flagged <- data$interview |>
    dplyr::filter(.data$trailer_count > .data$vehicle_count)

  result <- create_results_table(
    pass           = nrow(flagged) == 0,
    critical       = FALSE,
    check_category = "record",
    check_type     = "interview",
    error_count    = nrow(flagged),
    message        = if (nrow(flagged) > 0)
      glue::glue("{nrow(flagged)} interviews where trailer count > vehicle count.")
    else "All trailer counts are \u2264 vehicle counts."
  )

  detail <- if (nrow(flagged) > 0) {
    flagged |>
      dplyr::select("event_date", "water_body", "interview_number",
                    "trailer_count", "vehicle_count", "creel_event_id")
  }

  list(result = result, detail = detail)
}

#' Check for angler count exceeding group count
#' @param data List of creel data from [creelutils::fetch_dwg()].
#' @returns `list(result, detail)` where `result` is
#'    a one-row summary tibble and `detail` is flagged records or `NULL` on pass.
#' @export
interview_angler.count.group.count <- function(data) {
  flagged <- data$interview |>
    dplyr::filter(.data$angler_count > .data$total_group_count)

  result <- create_results_table(
    pass           = nrow(flagged) == 0,
    critical       = FALSE,
    check_category = "record",
    check_type     = "interview",
    error_count    = nrow(flagged),
    message        = if (nrow(flagged) > 0)
      glue::glue("{nrow(flagged)} interviews where angler count > group count.")
    else "All angler counts are \u2264 total group counts."
  )

  detail <- if (nrow(flagged) > 0) {
    flagged |>
      dplyr::select("event_date", "water_body", "interview_number",
                    "angler_count", "total_group_count", "creel_event_id")
  }

  list(result = result, detail = detail)
}

#' Check for vehicle count exceeding group count
#' @param data List of creel data from [creelutils::fetch_dwg()].
#' @returns `list(result, detail)` where `result` is
#'   a one-row summary tibble and `detail` is flagged records or `NULL` on pass.
#' @export
interview_vehicle.count.group.count <- function(data) {
  flagged <- data$interview |>
    dplyr::filter(.data$vehicle_count > .data$total_group_count)

  result <- create_results_table(
    pass           = nrow(flagged) == 0,
    critical       = FALSE,
    check_category = "record",
    check_type     = "interview",
    error_count    = nrow(flagged),
    message        = if (nrow(flagged) > 0)
      glue::glue("{nrow(flagged)} interviews where vehicle count > group count.")
    else "All vehicle counts are \u2264 total group counts."
  )

  detail <- if (nrow(flagged) > 0) {
    flagged |>
      dplyr::select("event_date", "water_body", "interview_number",
                    "vehicle_count", "total_group_count", "creel_event_id")
  }

  list(result = result, detail = detail)
}

#' Check for missing previously_interviewed values
#' @param data List of creel data from [creelutils::fetch_dwg()].
#' @returns `list(result, detail)` where `result` is
#'    a one-row summary tibble and `detail` is flagged records or `NULL` on pass.
#' @export
interview_na.previously.interviewed <- function(data) {
  flagged <- data$interview |>
    dplyr::filter(is.na(.data$previously_interviewed))

  result <- create_results_table(
    pass           = nrow(flagged) == 0,
    critical       = FALSE,
    check_category = "record",
    check_type     = "interview",
    error_count    = nrow(flagged),
    message        = if (nrow(flagged) > 0)
      glue::glue("{nrow(flagged)} missing previously_interviewed values.")
    else "All previously_interviewed values present."
  )

  detail <- if (nrow(flagged) > 0) {
    flagged |>
      dplyr::select("event_date", "water_body", "section_num",
                    "interview_number", "previously_interviewed",
                    "creel_event_id")
  }

  list(result = result, detail = detail)
}

#' Check for missing fishing_location values
#' @param data List of creel data from [creelutils::fetch_dwg()].
#' @returns `list(result, detail)` where `result` is
#'    a one-row summary tibble and `detail` is flagged records or `NULL` on pass.
#' @export
interview_na.fishing.location <- function(data) {
  flagged <- data$interview |>
    dplyr::filter(is.na(.data$fishing_location))

  result <- create_results_table(
    pass           = nrow(flagged) == 0,
    critical       = FALSE,
    check_category = "record",
    check_type     = "interview",
    error_count    = nrow(flagged),
    message        = if (nrow(flagged) > 0)
      glue::glue("{nrow(flagged)} missing fishing_location values.")
    else "All fishing_location values present."
  )

  detail <- if (nrow(flagged) > 0) {
    flagged |>
      dplyr::select("event_date", "water_body", "section_num",
                    "interview_number", "fishing_location",
                    "creel_event_id")
  }

  list(result = result, detail = detail)
}

#' Check for incomplete trips with a fishing end time
#' @param data List of creel data from [creelutils::fetch_dwg()].
#' @returns `list(result, detail)` where `result` is
#'    a one-row summary tibble and `detail` is flagged records or `NULL` on pass.
#' @export
interview_incomplete.trip.fishing.end.time <- function(data) {
  flagged <- data$interview |>
    dplyr::filter(.data$trip_status == "Incomplete" & !is.na(.data$fishing_end_time))

  result <- create_results_table(
    pass           = nrow(flagged) == 0,
    critical       = FALSE,
    check_category = "record",
    check_type     = "interview",
    error_count    = nrow(flagged),
    message        = if (nrow(flagged) > 0)
      glue::glue("{nrow(flagged)} incomplete trips with a fishing end time.")
    else "No incomplete trips have a fishing end time."
  )

  detail <- if (nrow(flagged) > 0) {
    flagged |>
      dplyr::select("event_date", "water_body", "interview_number",
                    "trip_status", "fishing_start_time", "fishing_end_time",
                    "creel_event_id")
  }

  list(result = result, detail = detail)
}

#' Check for interview time before fishing start time
#' @param data List of creel data from [creelutils::fetch_dwg()].
#' @returns `list(result, detail)` where `result` is
#'    a one-row summary tibble and `detail` is flagged records or `NULL` on pass.
#' @export
interview_end.time.before.start.time <- function(data) {
  flagged <- data$interview |>
    dplyr::filter(.data$fishing_end_time < .data$fishing_start_time)

  result <- create_results_table(
    pass           = nrow(flagged) == 0,
    critical       = FALSE,
    check_category = "record",
    check_type     = "interview",
    error_count    = nrow(flagged),
    message        = if (nrow(flagged) > 0)
      glue::glue("{nrow(flagged)} interviews where end time is before start time.")
    else "All fishing end times are after the start time."
  )

  detail <- if (nrow(flagged) > 0) {
    flagged |>
      dplyr::select("event_date", "water_body", "interview_number",
                    "fishing_start_time", "fishing_end_time",
                    "creel_event_id")
  }

  list(result = result, detail = detail)
}

#' DUPE??
#' @param data List of creel data from [creelutils::fetch_dwg()].
#' @returns `list(result, detail)` where `result` is
#'    a one-row summary tibble and `detail` is flagged records or `NULL` on pass.
#' @export
interview_interview.before.fishing.start.time <- function(data) {
  flagged <- data$interview |>
    dplyr::filter(.data$interview_time < .data$fishing_start_time)

  result <- create_results_table(
    pass           = nrow(flagged) == 0,
    critical       = FALSE,
    check_category = "record",
    check_type     = "interview",
    error_count    = nrow(flagged),
    message        = if (nrow(flagged) > 0)
      glue::glue("{nrow(flagged)} interviews where interview time is before fishing start.")
    else "All interview times are after the fishing start time."
  )

  detail <- if (nrow(flagged) > 0) {
    flagged |>
      dplyr::select("event_date", "water_body", "interview_number",
                    "interview_time", "fishing_start_time",
                    "creel_event_id")
  }

  list(result = result, detail = detail)
}

#' Check for missing target_species values
#' @param data List of creel data from [creelutils::fetch_dwg()].
#' @returns `list(result, detail)` where `result` is
#'    a one-row summary tibble and `detail` is flagged records or `NULL` on pass.
#' @export
interview_na.target.species <- function(data) {
  flagged <- data$interview |>
    dplyr::filter(is.na(.data$target_species))

  result <- create_results_table(
    pass           = nrow(flagged) == 0,
    critical       = FALSE,
    check_category = "record",
    check_type     = "interview",
    error_count    = nrow(flagged),
    message        = if (nrow(flagged) > 0)
      glue::glue("{nrow(flagged)} missing target_species values.")
    else "All target_species values present."
  )

  detail <- if (nrow(flagged) > 0) {
    flagged |>
      dplyr::select("event_date", "water_body", "section_num",
                    "interview_number", "target_species",
                    "creel_event_id")
  }

  list(result = result, detail = detail)
}

#' Check for missing boat_used values
#' @param data List of creel data from [creelutils::fetch_dwg()].
#' @returns `list(result, detail)` where `result` is
#'    a one-row summary tibble and `detail` is flagged records or `NULL` on pass.
#' @export
interview_na.boat.used <- function(data) {
  flagged <- data$interview |>
    dplyr::filter(is.na(.data$boat_used))

  result <- create_results_table(
    pass           = nrow(flagged) == 0,
    critical       = FALSE,
    check_category = "record",
    check_type     = "interview",
    error_count    = nrow(flagged),
    message        = if (nrow(flagged) > 0)
      glue::glue("{nrow(flagged)} missing boat_used values.")
    else "All boat_used values present."
  )

  detail <- if (nrow(flagged) > 0) {
    flagged |>
      dplyr::select("event_date", "water_body", "section_num",
                    "interview_number", "boat_used",
                    "creel_event_id")
  }

  list(result = result, detail = detail)
}
