interview_missing.trip.status <- function(data) {

  interview <- data$interview
  missing_count <- sum(is.na(interview$trip_status))

  if (missing_count > 0) {
    return(tibble::tibble(
      pass = FALSE,
      message = glue::glue("There are {missing_count} missing trip statuses in the interviews."),
      critical = FALSE
    ))
  } else {
    return(tibble::tibble(
      pass = TRUE,
      message = "All trip statuses are present in the interviews.",
      critical = FALSE
    ))
  }
}
