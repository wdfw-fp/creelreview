#' run_all_checks
#'
#' @param data creel data
#' @importFrom dplyr add_row mutate case_when
#' @returns results table
#' @export
run_all_checks <- function(data) {
  # List of check functions
  checks <- list(
    interview_missing.trip.status = interview_missing.trip.status(data)
  )

  # Loop through checks and fill the results tibble
  for (check_name in names(checks)) {
    check_result <- checks[[check_name]]
    results <- add_row(results,
                       check_name = check_name,
                       pass = check_result$pass,
                       message = check_result$message,
                       critical = check_result$critical)
  }

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
