#' get_grading
#'
#' @param results output of run_all_checks()
#' @importFrom glue glue
#' @importFrom cli cli_alert_info cli_alert_success
#' @returns list of grading
#' @export
get_grading <- function(results) {
  # Count the number of passed checks
  passed_critical <- sum(results$pass & results$critical)
  total_critical <- sum(results$critical)

  passed_non_critical <- sum(results$pass & !results$critical)
  total_non_critical <- sum(!results$critical)

  # Calculate percentages
  critical_pass_rate <- ifelse(total_critical > 0, passed_critical / total_critical, NA)
  non_critical_pass_rate <- ifelse(total_non_critical > 0, passed_non_critical / total_non_critical, NA)

  # Overall grading
  overall_grade <- ifelse(critical_pass_rate == 1, "A",
                          ifelse(critical_pass_rate >= 0.8, "B", "C"))

  # Print grading to console
  cli_alert_info(glue("Critical pass rate: {critical_pass_rate * 100}%"))
  cli_alert_info(glue("Non-critical pass rate: {non_critical_pass_rate * 100}%"))
  cli_alert_success(glue("Overall grade: {overall_grade}"))

  list(
    critical_pass_rate = critical_pass_rate,
    non_critical_pass_rate = non_critical_pass_rate,
    overall_grade = overall_grade
  )
}
