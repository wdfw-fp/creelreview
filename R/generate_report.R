#' Generate QAQC report
#'
#' This function generates HTML QAQC reports for specificed freshwater creel fisheries
#' and saves them to a specified directory. By default, the reports are saved to a
#' directory called "CreelDataQAQC_reports" in the current working directory.
#'
#' @param fishery_names A character vector of freshwater creel fishery names.
#' @param output_dir A character string specifying the directory to save the reports.
#' @importFrom glue glue
#' @importFrom cli cli_alert_info
#' @importFrom quarto quarto_render
#' @returns HTML QAQC report for each fishery
#' @examples
#' \dontrun{
#' # Default useage: saves report to current working directory
#' generate_report(c("Fishery A", "Fishery B"))
#'
#' # Custom output directory
#' generate_report(c("Fishery A", "Fishery B"), output_dir = "path/to/output")
#' }
#' @export
generate_report <- function(fishery_names, output_dir = NULL) {

#This file is the top level script used to produce QAQC reports.
#Define the fisheries you want to generate reports for
#then run this script to call Quarto and produce the HTML outputs.

  #default output location
  if (is.null(output_dir)) {
    output_dir <- normalizePath(file.path(getwd(), "CreelDataQAQC_reports"), mustWork = FALSE)
  } else {
    output_dir <- normalizePath(output_dir, mustWork = FALSE)
  }

  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  #paths to quarto config and qaqc report file in inst/
  quarto_config <- system.file("_quarto.yml", package = "CreelDataQAQC")
  quarto_file <- system.file("rmarkdown", "report.qmd", package = "CreelDataQAQC")

  # Check if the file is found
  if (quarto_file == "") {
    stop("report.qmd not found in inst/rmarkdown/.")
  }

  #Loop through each fishery and render report
  for (fishery_name in fishery_names) {

    cli_alert_info(glue::glue("Starting report generation for {fishery_name}...\n"))

    #Define output file name
    clean_name <- gsub(" ", "-", fishery_name)
    output_file <- paste0("qaqc_report_", clean_name, "_", Sys.Date(), ".html")

    #Render Quarto document
    # system(paste("quarto render", shQuote(quarto_file),
    #              "--to html",
    #              paste0("--output ", shQuote(temp_file)),
    #              paste0("-P fishery_name=", shQuote(fishery_name))
    # ))

    quarto_render(
      input = quarto_file,
      output_format = "html",
      output_file = output_file,
      execute_params = list(fishery_name = fishery_name)
    )

    #define final file location, rename, and remove original copy from working directory
    final_output_file <- file.path(output_dir, basename(output_file))

    file.rename(output_file, final_output_file)

    if (file.exists(output_file)) {
      file.remove(output_file)
    }

    #print message
    cat("Rendered report for", fishery_name, "\n")
  }
}
