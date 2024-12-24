#' Generate QAQC Reports
#'
#' This function generates HTML QAQC reports for specified freshwater creel fisheries
#' and saves them to a designated directory. By default, reports are saved to
#' a folder named "CreelDataQAQC_reports" in the current working directory.
#'
#' @param fishery_names A character vector of freshwater creel fishery names.
#' @param output_dir An optional custom output directory for the reports. If not provided,
#' the reports are saved to the default directory.
#'
#' @return Generates and saves HTML QAQC reports. No value is returned.
#' @examples
#' \dontrun{
#' # Default usage: saves reports in the current working directory
#' generate_qaqc_report(c("Fishery A", "Fishery B"))
#'
#' # Custom output directory
#' custom_dir <- file.path("myfilepath/")
#' generate_qaqc_report(c("Fishery A", "Fishery B"), output_dir = custom_dir)
#' }
#' @export
generate_qaqc_report <- function(fishery_names, output_dir = NULL) {

  #default output location
  if (is.null(output_dir)) { #also defined in _quarto.yml which seems to take priority?
    output_dir <- normalizePath(file.path(getwd(), "CreelDataQAQC_reports"), mustWork = FALSE)
  } else {
    output_dir <- normalizePath(output_dir, mustWork = FALSE)
  }

  #ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  #access report.qmd from the inst/ folder
  quarto_file <- system.file("rmarkdown", "report.qmd", package = "CreelDataQAQC")

  #check that the file is found
  if (quarto_file == "") {
    stop("report.qmd not found in inst/rmarkdown/. Please confirm proper installation.")
  }
  #Quarto --output and/or --output-dir cannot write to a relative or absolute path?
  #create a temporary directory to reports, then move and rename files after rendering
  temp_dir <- file.path(tempdir(), "qaqc_temp")
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE)
  }

  #Loop through each fishery and render report
  for (fishery_name in fishery_names) {

    cli::cli_alert_info(glue::glue("Starting report generation for {fishery_name}..."))

    #Define output file path
    clean_name <- gsub(" ", "-", fishery_name)
    temp_file <- paste0("qaqc_report_", clean_name, "_", Sys.Date(), ".html")

    # Render Quarto document
    command <- paste(
      "quarto render", shQuote(quarto_file),
      "--to html",
      paste0("--output ", shQuote(temp_file)),
      paste0("-P fishery_name=", shQuote(fishery_name))
    )

    result <- system(command, intern = FALSE, ignore.stderr = TRUE)

    # Check if file exists
    output_check <- file.path(paste0(getwd(), "/CreelDataQAQC_reports"), temp_file)

    if (file.exists(output_check) && result == 0) {
      cli::cli_alert_success(glue::glue("Report for {fishery_name} generated successfully."))

      # # Move the report to the final output directory
      # final_path <- file.path(output_dir, temp_file)
      # file.rename(temp_path, final_path)
      # cli::cli_alert_success(glue::glue("Report for {fishery_name} saved to {final_path}."))

    } else if (!file.exists(output_check) || result != 0) {
      cli::cli_alert_danger(glue::glue("Report generation for {fishery_name} failed. Check Quarto log for details."))
    }
  }

  # Cleanup temporary directory
  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE)
  }
}
