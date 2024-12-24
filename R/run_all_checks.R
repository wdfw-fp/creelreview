#' run_all_checks
#'
#' @param data creel data
#'
#' @returns results table
#' @export
run_all_checks <- function(data) {
  # List of check functions
  checks <- list(
    #interviews
    interview_missing.trip.status = CreelDataQAQC::interview_missing.trip.status(data)
  )

  # Create tibble to store results
  results <- tibble::tibble(
    check_name = character(),
    pass = logical(),
    critical = logical(),
    message = character(),
    status_color = character()
  )

  # Loop through checks and fill the tibble
  for (check_name in names(checks)) {
    check_result <- checks[[check_name]]
    results <- dplyr::add_row(results,
                       check_name = check_name,
                       pass = check_result$pass,
                       message = check_result$message,
                       critical = check_result$critical)

    # Print status to console
    if (check_result$pass) {
      cli::cli_alert_success(glue::glue("{check_name}: Pass"))
    } else {
      cli::cli_alert_danger(glue::glue("{check_name}: Fail - {check_result$message}"))
    }
   }

  # Add stoplight status colors
  results <- results %>%
    mutate(
      status_color = case_when(
        pass == TRUE & critical == TRUE ~ "green", #Critical pass
        Pass == TRUE & critical == FALSE ~ "green", #Non-critical pass
        pass == FALSE & critical == FALSE ~ "yellow",  # Non-critical failure
        pass == FALSE & critical == TRUE ~ "red"  #Critical failure
      )
  )

  return(results)
}
