# Missing trip status, all interviews should have a trip status of either "Incomplete" or "Complete"
interview_missing.trip.status <- function(data) {

  interview <- data$interview
  error_count <- sum(is.na(interview$trip_status), na.rm = TRUE)

  if (error_count > 0) {
    return(create_results_table(
      pass = FALSE,
      critical = FALSE,
      qaqc_check_type = "",
      error_count = error_count,
      message = glue::glue("There are {error_count} missing trip statuses in the interviews.")
    ))
  } else {
    return(create_results_table(
      pass = TRUE,
      critical = FALSE,
      qaqc_check_type = "",
      error_count = error_count,
      message = "All trip statuses are present in all interviews."
    ))
  }
}

# Fishing end time > interview time
interview_end.time.interview.time <- function(data) {

  interview <- data$interview
  error_count <- sum(interview$fishing_end_time > interview$interview_time, na.rm = TRUE)

  if (error_count > 0) {
    return(create_results_table(
      pass = FALSE,
      critical = FALSE,
      qaqc_check_type = "",
      error_count = error_count,
      message = glue::glue("There are {error_count} interviews where the fishing end time is after the interview time.")
    ))
  } else {
    return(create_results_table(
      pass = TRUE,
      critical = FALSE,
      qaqc_check_type = "",
      error_count = error_count,
      message = "All fishing end times are before the interview time."
    ))
  }
}

# Fishing start time > fishing end time
interview_start.time.end.time <- function(data) {

  interview <- data$interview
  error_count <- sum(interview$fishing_start_time > interview$fishing_end_time, na.rm = TRUE)

  if (error_count > 0) {
    return(create_results_table(
      pass = FALSE,
      critical = FALSE,
      qaqc_check_type = "",
      error_count = error_count,
      message = glue::glue("There are {error_count} interviews where the fishing start time is after the fishing end time.")
    ))
  } else {
    return(create_results_table(
      pass = TRUE,
      critical = FALSE,
      qaqc_check_type = "",
      error_count = error_count,
      message = "All fishing start times are before the fishing end time."
    ))
  }
}

# Trailer Count > Vehicle Count
interview_trailer.count.vehicle.count <- function(data) {

  interview <- data$interview
  error_count <- sum(interview$trailer_count > interview$vehicle_count, na.rm = TRUE)

  if (error_count > 0) {
    return(create_results_table(
      pass = FALSE,
      critical = FALSE,
      qaqc_check_type = "",
      error_count = error_count,
      message = glue::glue("There are {error_count} interviews where the trailer count is greater than the vehicle count.")
    ))
  } else {
    return(create_results_table(
      pass = TRUE,
      critical = FALSE,
      qaqc_check_type = "",
      error_count = error_count,
      message = "All trailer counts are less than or equal to the vehicle count."
    ))
  }
}

# Angler Count > Group Count
interview_angler.count.group.count <- function(data) {

  interview <- data$interview
  error_count <- sum(interview$angler_count > interview$total_group_count, na.rm = TRUE)

  if (error_count > 0) {
    return(create_results_table(
      pass = FALSE,
      critical = FALSE,
      qaqc_check_type = "",
      error_count = error_count,
      message = glue::glue("There are {error_count} interviews where the angler count is greater than the group count.")
    ))
  } else {
    return(create_results_table(
      pass = TRUE,
      critical = FALSE,
      qaqc_check_type = "",
      error_count = error_count,
      message = "All angler counts are less than or equal to the group count."
    ))
  }
}
