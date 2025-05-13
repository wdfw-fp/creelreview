#' Generate QAQC report
#'
#' This function generates HTML QAQC reports for specified freshwater creel fisheries
#' and saves them to a specified directory. By default, the reports are saved to a
#' directory called "CreelDataQAQC_reports" in the current working directory.
#'
#' @param fishery_names A character vector of freshwater creel fishery names.
#' @param output_dir A character string specifying the directory to save the reports.
#' @importFrom glue glue
#' @importFrom cli cli_alert_info
#' @importFrom quarto quarto_render
#' @importFrom here here
#' @returns HTML QAQC report for each fishery
#' @examples
#' \dontrun{
#' # Default usage: saves report to current working directory
#' generate_report(c("Fishery A", "Fishery B"))
#'
#' # Custom output directory
#' generate_report(c("Fishery A", "Fishery B"), output_dir = "path/to/output")
#' }
#' @export
generate_report <- function(fishery_names, output_dir = NULL) {

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

  #path to QAQC report file in inst/rmarkdown/
  quarto_file <- here("qaqc_script.qmd")

  # Check if the file is found
  if (quarto_file == "") {
    stop(paste(quarto_file, "not found in inst/rmarkdown/."))
  }

  #Loop through each fishery and render report
  for (fishery_name in fishery_names) {


    tryCatch({
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

      #call helper function to remove figures from inst/figures between each loop
      cleanup_figures()

      #print message
      cli_alert_success(glue::glue("Rendered report for {fishery_name} to:\n{final_output_file}\n\n\n"))

    }, error = function(e) {
      #print fail message
      cli::cli_alert_danger(glue::glue("Failed to generate report for {fishery_name}. Error: {e$message}\n\n\n"))
    })
  }
}

#' cleanup_figures
#'
#' This is a helper function that removes all files from the inst/figures directory.
#' As the html reports have embeded resources, this is useful for cleaning up the
#' figures directory between each sequential report iteration.
#' @keywords internal
cleanup_figures <- function() {
  figures_path <- paste0(here("inst", "figures"), "/")

  figure_files <- list.files(figures_path, full.names = TRUE)

  if (length(figure_files) > 0) {
    file.remove(figure_files)

  }
}
