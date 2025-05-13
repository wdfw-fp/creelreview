# 1 - Effort end time < start time



#' Effort count end time before start time
#'
#' @param data list that includes an 'effort' list of creel data
#'
#' @returns table describing the check applied, status, error count, and includes a message
#' @export
effort_end.time.start.time <- function(data) {

  effort <- data$effort
  error_count <- sum(effort$effort_end_time < effort$effort_start_time, na.rm = TRUE)

  if (error_count > 0) {
    return(create_results_table(
      pass = FALSE,
      critical = TRUE,
      qaqc_check_type = "effort",
      error_count = error_count,
      message = glue::glue("There are {error_count} rows where the end time is less than the start time.")
    ))
  } else {
    return(create_results_table(
      pass = TRUE,
      critical = TRUE,
      qaqc_check_type = "effort",
      error_count = error_count,
      message = "All end times are greater than or equal to the start times."
    ))
  }
}

# 2 - Effort location is NULL or NA
effort_na.location <- function(data) {

  effort <- data$effort
  error_count <- sum(is.na(effort$location) | effort$location == "", na.rm = TRUE)

  if (error_count > 0) {
    return(create_results_table(
      pass = FALSE,
      critical = TRUE,
      qaqc_check_type = "effort",
      error_count = error_count,
      message = glue::glue("There are {error_count} rows where the location is NULL or NA.")
    ))
  } else {
    return(create_results_table(
      pass = TRUE,
      critical = TRUE,
      qaqc_check_type = "effort",
      error_count = error_count,
      message = "The 'locations' field has no missing values."
    ))
  }
}

# 3 - Effort count quantity is NULL or NA
effort_na.count.quantity <- function(data) {

  effort <- data$effort
  error_count <- sum(is.na(effort$count_quantity) | effort$count_quantity == "", na.rm = TRUE)

  if (error_count > 0) {
    return(create_results_table(
      pass = FALSE,
      critical = TRUE,
      qaqc_check_type = "effort",
      error_count = error_count,
      message = glue::glue("There are {error_count} rows where the count quantity is NULL or NA.")
    ))
  } else {
    return(create_results_table(
      pass = TRUE,
      critical = TRUE,
      qaqc_check_type = "effort",
      error_count = error_count,
      message = "The 'count_quantity' field has no missing values."
    ))
  }
}

# 4 - Effort count type is NULL or NA
effort_na.count.type <- function(data) {

  effort <- data$effort
  error_count <- sum(is.na(effort$count_type) | effort$count_type == "", na.rm = TRUE)

  if (error_count > 0) {
    return(create_results_table(
      pass = FALSE,
      critical = TRUE,
      qaqc_check_type = "effort",
      error_count = error_count,
      message = glue::glue("There are {error_count} rows where the count type is NULL or NA.")
    ))
  } else {
    return(create_results_table(
      pass = TRUE,
      critical = TRUE,
      qaqc_check_type = "effort",
      error_count = error_count,
      message = "The 'count_type' field has no missing values."
    ))
  }
}

# 5 - Count sequence check
#  identify anomalies in daily effort counts by comparing them to the mode within specified location and event_id groups.
# Flag values that exceed or fall below the mode and returns flagged rows and a summary.
