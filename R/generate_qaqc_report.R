#' generate_qaqc_report
#'
#' @param fishery_names input vector of freshwater creel fishery_names
#'
#' @returns HTML QAQC report for each fishery
#' @export
generate_qaqc_report <- function(fishery_names, output_dir = NULL) {

#This file is the top level script used to produce QAQC reports.
#Define the fisheries you want to generate reports for
#then run this script to call Quarto and produce the HTML outputs.

  # Default output directory
  if (is.null(output_dir)) {
    # Create a folder in the user's Documents (you can change this path)
    output_dir <- file.path(Sys.getenv("HOME"), "Documents", "CreelDataQAQC_reports")
  }

  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Access report.qmd from the inst/ folder
  quarto_file <- system.file("rmarkdown", "report.qmd", package = "CreelDataQAQC")

  # Check if the file is found
  if (quarto_file == "") {
    stop("report.qmd not found in package")
  }

  #Loop through each fishery and render report
  for (fishery_name in fishery_names) {

    cli::cli_alert_info(glue::glue("Starting report generation for {fishery_name}...\n"))

    #Define output file name
    safe_name <- gsub(" ", "-", fishery_name)
    output_file <- paste0("qaqc_report_", safe_name, "_", Sys.Date(), ".html")

    #Render Quarto document
    system(paste("quarto render", quarto_file,
                 "--to html",
                 paste0("--output ", shQuote(output_file)),
                 paste0("--output-dir ", shQuote(output_dir)),
                 paste0("-P fishery_name=", shQuote(fishery_name))
                 )
           )

    #print message
    cat("Rendered report for", fishery_name, "->", file.path(output_dir, output_file), "\n")
  }
}
