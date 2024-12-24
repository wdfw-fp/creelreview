#' interview_missing.trip.status
#'
#' @param data creel data set via CreelEstimateR::fetch_dwg(params$fishery_name)
#'
#' @returns PASS/FAIL with message and indication whether field is critical to estimation
#' @export
interview_missing.trip.status <- function(data) {
  interview <- data$interview

  if (any(is.na(interview$trip_status))) {
    cli::cli_alert_danger("Interview missing trip_status")
    return(list(
      pass = FALSE,
      message = paste("At least one interview is missing a 'trip_status'."),
      critical = FALSE
    ))
  } else {
    cli::cli_alert_success("There are no interviews with missing trip_status")
    return(list(
      pass = TRUE,
      message = paste("There are no interviews with missing 'trip_status'."),
      critical = FALSE
    ))
  }
}
