#' Run all registered QAQC and structural checks
#'
#' @description
#' Runs every registered check function against a creel dataset and returns
#' a summary table with stoplight status colors alongside the full check outputs.
#'
#' @param data List of creel data components as returned by
#'   [creelutils::fetch_dwg()].
#' @param conn Database connection from [creelutils::connect_creel_db()].
#'
#' @returns Named list with two elements:
#'   - `summary`: tibble with one row per check (pass, critical, status_color, etc.)
#'   - `checks`: named list of full check outputs, each `list(summary, checks)`
#' @importFrom rlang .data
#' @export
run_all_checks <- function(data, conn) {

  # List of check functions
  checks <- list(
    # Interviews
    interview_na.trip.status                      = interview_na.trip.status,
    interview_end.time.interview.time             = interview_end.time.interview.time,
    interview_start.time.end.time                 = interview_start.time.end.time,
    interview_trailer.count.vehicle.count         = interview_trailer.count.vehicle.count,
    interview_angler.count.group.count            = interview_angler.count.group.count,
    interview_vehicle.count.group.count           = interview_vehicle.count.group.count,
    interview_na.previously.interviewed           = interview_na.previously.interviewed,
    interview_na.fishing.location                 = interview_na.fishing.location,
    interview_incomplete.trip.fishing.end.time    = interview_incomplete.trip.fishing.end.time,
    interview_end.time.before.start.time          = interview_end.time.before.start.time,
    interview_interview.before.fishing.start.time = interview_interview.before.fishing.start.time,
    interview_na.target.species                   = interview_na.target.species,
    interview_na.boat.used                        = interview_na.boat.used,

    # Effort counts
    effort_end.time.start.time                    = effort_end.time.start.time,
    effort_na.location                            = effort_na.location,
    effort_na.count.quantity                       = effort_na.count.quantity,
    effort_na.count.type                          = effort_na.count.type
  )

  outputs <- list()

  # Loop through checks and store results
  for (check_name in names(checks)) {
    outputs[[check_name]] <- tryCatch(
      checks[[check_name]](data),
      error = function(e) { # Handle any check failures
        list(
          result = create_results_table(
            pass           = FALSE,
            critical       = TRUE,
            check_category = "error",
            check_type     = "internal-error",
            error_count    = NA_integer_,
            message        = paste0("Check '", check_name, "' errored: ", conditionMessage(e))
          ),
          detail = NULL
        )
      }
    )
  }

  summary <- purrr::map(outputs, "result") |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      status_color = dplyr::case_when(
        .data$pass & .data$critical     ~ "green",   # Critical pass
        .data$pass & !.data$critical    ~ "green",   # Non-critical pass
        !.data$pass & !.data$critical   ~ "yellow",  # Non-critical fail
        !.data$pass & .data$critical    ~ "red",     # Critical fail
        TRUE                            ~ "grey"
      )
    )

  list(
    summary = summary, # results table
    checks  = outputs  # itemized list
  )
}


#' Create a standardized results row for a QAQC check
#'
#' @param pass Logical. `TRUE` if the check passed.
#' @param critical Logical. `TRUE` if failure is critical (red), `FALSE` for
#'   non-critical (yellow).
#' @param check_category `"record"` or `"structural"`.
#' @param check_type Domain: `"interview"`, `"catch"`, `"effort"`,
#'   `"effort_census"`, `"effort_index"`, `"completeness"`, `"fishery_config"`.
#' @param error_count Integer. Number of errors found.
#' @param message Character. Human-readable result description.
#'
#' @returns One-row tibble.
#' @importFrom tibble tibble
#' @keywords internal
create_results_table <- function(
    pass,
    critical,
    check_category,
    check_type,
    error_count,
    message
) {
  tibble::tibble(
    pass            = pass,
    critical        = critical,
    check_category  = check_category,
    check_type      = check_type,
    error_count     = error_count,
    message         = message
  )
}

#' Build FishApps event links from creel_event_id
#'
#' @param creel_event_id Character vector of event IDs.
#' @returns Character vector of HTML anchor tags.
#' @keywords internal
fishapps_link <- function(creel_event_id) {
  paste0("https://apps.wdfw-fish.us/creel/creel_event/", creel_event_id)
}
