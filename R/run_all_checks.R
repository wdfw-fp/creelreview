#' run_all_checks
#'
#' @param data creel data
#' @importFrom dplyr add_row mutate case_when bind_rows
#' @returns results table
#' @export
run_all_checks <- function(data) {

  # Initialize list to store results
  results_list <- list()

  # List of check functions
  # check_name = check_name allows names(check) on a list
  checks <- list(

    # interview_checks.R
    interview_na.trip.status = interview_na.trip.status,
    interview_end.time.interview.time = interview_end.time.interview.time,
    interview_start.time.end.time = interview_start.time.end.time,
    interview_trailer.count.vehicle.count = interview_trailer.count.vehicle.count,
    interview_angler.count.group.count = interview_angler.count.group.count,
    interview_vehicle.count.group.count = interview_vehicle.count.group.count,
    interview_na.previously.interviewed = interview_na.previously.interviewed,
    interview_na.fishing.location = interview_na.fishing.location,
    interview_incomplete.trip.fishing.end.time = interview_incomplete.trip.fishing.end.time,
    interview_end.time.before.start.time = interview_end.time.before.start.time,
    interview_interview.before.fishing.start.time = interview_interview.before.fishing.start.time,
    # interview_complete.trip.missing.end.time = interview_complete.trip.missing.end.time !!!
    interview_na.target.species = interview_na.target.species,
    interview_na.boat.used = interview_na.boat.used,

    # catch_checks.R


    # effort_checks.R
    effort_end.time.start.time = effort_end.time.start.time,
    effort_na.location = effort_na.location,
    effort_na.count.quantity = effort_na.count.quantity,
    effort_na.count.type = effort_na.count.type
  )

  # Loop through checks and store results in the results list
  for (check_name in names(checks)) {
    check_function <- checks[[check_name]]

    # Call the function and store the result in the results list
    check_result <- check_function(data)
    results_list[[check_name]] <- check_result
  }

  # Combine all check results into a single tibble using bind_rows
  results <- bind_rows(results_list)

  # Add stoplight status colors
  results <- results |>
    mutate(
      status_color = case_when(
        pass == TRUE & critical == TRUE ~ "green", #Critical pass
        pass == TRUE & critical == FALSE ~ "green", #Non-critical pass
        pass == FALSE & critical == FALSE ~ "yellow", #Non-critical fail
        pass == FALSE & critical == TRUE ~ "red", #Critical fail
        TRUE ~ "black" #Unknown status
      )
    )

  return(results)
}

#' create_results_table
#'
#' This helper function creates a results table for each QAQC check.
#'
#' @param pass logical / TRUE if the check passed, FALSE if it failed
#' @param critical logical / TRUE if the check is critical, FALSE if it is not
#' @param qaqc_check_type character / type of QAQC check
#' @param error_count numeric / number of errors found for a given check
#' @param message character / message to display in results table
#' @returns template results table
#' @importFrom tibble tibble
#' @keywords internal
create_results_table <- function(pass, critical, qaqc_check_type, error_count, message) {

  table <- tibble::tibble(
    pass = pass,
    critical = critical,
    qaqc_check_type = qaqc_check_type,
    error_count = error_count,
    message = message
  )

  return(table)
}
