#' generate_qaqc_report
#'
#' @param fishery_names input vector of freshwater creel fishery_names
#'
#' @returns HTML QAQC report for each fishery
#' @export
generate_qaqc_report <- function(fishery_names) {

#This file is the top level script used to produce QAQC reports.
#Define the fisheries you want to generate reports for
#then run this script to call Quarto and produce the HTML outputs.

  #Crate outputs folder
  output_dir <- "reports"
  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  }else{
    print("reports folder exists")
  }

  #Loop through each fishery and render report
  for (fishery_name in fishery_names) {

    cli::cli_alert_info(glue::glue("Starting report generation for {fishery_name}...\n"))

    #Define output file name
    safe_name <- gsub(" ", "-", fishery_name)
    output_file <- paste0("qaqc_report_", safe_name, "_", Sys.Date(), ".html")

    #Render Quarto document
    system(paste("quarto render report.qmd",
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
