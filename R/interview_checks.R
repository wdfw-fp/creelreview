# 1, Missing trip status, all interviews should have a trip status of either "Incomplete" or "Complete"
interview_na.trip.status <- function(data) {

  interview <- data$interview
  error_count <- sum(is.na(interview$trip_status))

  if (error_count > 0) {
    return(create_results_table(
      pass = FALSE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = glue::glue("There are {error_count} missing trip statuses in the interviews.")
    ))
  } else {
    return(create_results_table(
      pass = TRUE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = "All trip statuses are present in all interviews."
    ))
  }
}

# 2, Fishing end time > interview time
interview_end.time.interview.time <- function(data) {

  interview <- data$interview
  error_count <- sum(interview$fishing_end_time > interview$interview_time, na.rm = TRUE)

  if (error_count > 0) {
    return(create_results_table(
      pass = FALSE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = glue::glue("There are {error_count} interviews where the fishing end time is after the interview time.")
    ))
  } else {
    return(create_results_table(
      pass = TRUE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = "All fishing end times are before the interview time."
    ))
  }
}

# 3, Fishing start time > fishing end time
interview_start.time.end.time <- function(data) {

  interview <- data$interview
  error_count <- sum(interview$fishing_start_time > interview$fishing_end_time, na.rm = TRUE)

  if (error_count > 0) {
    return(create_results_table(
      pass = FALSE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = glue::glue("There are {error_count} interviews where the fishing start time is after the fishing end time.")
    ))
  } else {
    return(create_results_table(
      pass = TRUE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = "All fishing start times are before the fishing end time."
    ))
  }
}

# 4, Trailer Count > Vehicle Count
interview_trailer.count.vehicle.count <- function(data) {

  interview <- data$interview
  error_count <- sum(interview$trailer_count > interview$vehicle_count, na.rm = TRUE)

  if (error_count > 0) {
    return(create_results_table(
      pass = FALSE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = glue::glue("There are {error_count} interviews where the trailer count is greater than the vehicle count.")
    ))
  } else {
    return(create_results_table(
      pass = TRUE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = "All trailer counts are less than or equal to the vehicle count."
    ))
  }
}

# 5, Angler Count > Group Count
interview_angler.count.group.count <- function(data) {

  interview <- data$interview
  error_count <- sum(interview$angler_count > interview$total_group_count, na.rm = TRUE)

  if (error_count > 0) {
    return(create_results_table(
      pass = FALSE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = glue::glue("There are {error_count} interviews where the angler count is greater than the group count.")
    ))
  } else {
    return(create_results_table(
      pass = TRUE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = "All angler counts are less than or equal to the group count."
    ))
  }
}

# 6, Vehicle Count > Group Count
interview_vehicle.count.group.count <- function(data) {

  interview <- data$interview
  error_count <- sum(interview$vehicle_count > interview$total_group_count, na.rm = TRUE)

  if (error_count > 0) {
    return(create_results_table(
      pass = FALSE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = glue::glue("There are {error_count} interviews where the vehicle count is greater than the group count.")
    ))
  } else {
    return(create_results_table(
      pass = TRUE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = "All vehicle counts are less than or equal to the group count."
    ))
  }
}

# 7, NA in previously_interviewed column
interview_na.previously.interviewed <- function(data) {

  interview <- data$interview
  error_count <- sum(is.na(interview$previously_interviewed))

  if (error_count > 0) {
    return(create_results_table(
      pass = FALSE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = glue::glue("There are {error_count} missing values in the previously_interviewed column.")
    ))
  } else {
    return(create_results_table(
      pass = TRUE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = "All previously_interviewed values are present."
    ))
  }
}

# 8, NA fishing_location column
interview_na.fishing.location <- function(data) {

  interview <- data$interview
  error_count <- sum(is.na(interview$fishing_location))

  if (error_count > 0) {
    return(create_results_table(
      pass = FALSE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = glue::glue("There are {error_count} missing values in the fishing_location column.")
    ))
  } else {
    return(create_results_table(
      pass = TRUE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = "All fishing_location values are present."
    ))
  }
}

# 9, Incomplete trip status with a fishing end time
interview_incomplete.trip.fishing.end.time <- function(data) {

  interview <- data$interview
  error_count <- sum(interview$trip_status == "Incomplete" & !is.na(interview$fishing_end_time))

  if (error_count > 0) {
    return(create_results_table(
      pass = FALSE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = glue::glue("There are {error_count} interviews with an incomplete trip status and a fishing end time.")
    ))
  } else {
    return(create_results_table(
      pass = TRUE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = "All interviews with an incomplete trip status do not have a fishing end time."
    ))
  }
}

# 10, Fishing end time before fishing start time
interview_end.time.before.start.time <- function(data) {

  interview <- data$interview
  error_count <- sum(interview$fishing_end_time < interview$fishing_start_time, na.rm = TRUE)

  if (error_count > 0) {
    return(create_results_table(
      pass = FALSE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = glue::glue("There are {error_count} interviews where the fishing end time is before the fishing start time.")
    ))
  } else {
    return(create_results_table(
      pass = TRUE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = "All fishing end times are after the fishing start time."
    ))
  }
}

# 11, Interview time before fishing start time
interview_interview.before.fishing.start.time <- function(data) {

  interview <- data$interview
  error_count <- sum(interview$interview_time < interview$fishing_start_time, na.rm = TRUE)

  if (error_count > 0) {
    return(create_results_table(
      pass = FALSE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = glue::glue("There are {error_count} interviews where the interview time is before the fishing start time.")
    ))
  } else {
    return(create_results_table(
      pass = TRUE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = "All interview times are after the fishing start time."
    ))
  }
}

# 12, Complete trip status with a missing fishing end time
interview_complete.trip.missing.end.time <- function(data) {

  interview <- data$interview
  error_count <- sum(interview$trip_status == "Complete" & is.na(interview$fishing_end_time))

  if (error_count > 0) {
    return(create_results_table(
      pass = FALSE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = glue::glue("There are {error_count} interviews with a complete trip status and a missing fishing end time.")
    ))
  } else {
    return(create_results_table(
      pass = TRUE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = "All interviews with a complete trip status have a fishing end time."
    ))
  }
}

# 13, NA target_species
interview_na.target.species <- function(data) {

  interview <- data$interview
  error_count <- sum(is.na(interview$target_species))

  if (error_count > 0) {
    return(create_results_table(
      pass = FALSE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = glue::glue("There are {error_count} missing values in the target_species column.")
    ))
  } else {
    return(create_results_table(
      pass = TRUE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = "All target_species values are present."
    ))
  }
}

# 14, NA boat_used
interview_na.boat.used <- function(data) {

  interview <- data$interview
  error_count <- sum(is.na(interview$boat_used))

  if (error_count > 0) {
    return(create_results_table(
      pass = FALSE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = glue::glue("There are {error_count} missing values in the boat_used column.")
    ))
  } else {
    return(create_results_table(
      pass = TRUE,
      critical = FALSE,
      qaqc_check_type = "interview",
      error_count = error_count,
      message = "All boat_used values are present."
    ))
  }
}
