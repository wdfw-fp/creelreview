#' run_all_checks
#'
#' @param data creel data
#'
#' @returns results table
#' @export
run_all_checks <- function(data) {
  # List of check functions
  checks <- list(
    interview_missing.trip.status = CreelDataQAQC::interview_missing.trip.status(data)
  )

  # Create tibble to store results
  results <-tibble::tibble(
    check_name = character(),
    pass = logical(),
    message = character(),
    critical = logical(),
    status_color = character()
  )

  # Loop through checks and fill the tibble
  for (check_name in names(checks)) {
    check_result <- checks[[check_name]]
    results <- add_row(results,
                       check_name = check_name,
                       pass = check_result$pass,
                       message = check_result$message,
                       critical = check_result$critical)

    # Print status to console
    if (check_result$pass) {
      cli::cli_alert_success(glue("{check_name}: Pass"))
    } else {
      cli::cli_alert_danger(glue("{check_name}: Fail - {check_result$message}"))
    }
  }

  # Add stoplight status colors
  results <- results %>%
    mutate(
      status_color = case_when(
        pass == TRUE & critical == TRUE ~ "red",  # Critical failure is red
        pass == TRUE ~ "green",  # Normal pass is green
        pass == FALSE & critical == TRUE ~ "orange",  # Critical check fail is orange
        pass == FALSE & critical == FALSE ~ "yellow"  # Non-critical fail is yellow
      )
    )

  return(results)
}
