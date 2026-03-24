#' Generate QAQC report
#'
#' Renders a HTML report on data quality assurance/quality control (QAQC) for one or more freshwater creel fisheries
#' and saves them to a specified directory.
#'
#' **Note - VPN Required:** By default reports are written to a mapped network "T:/" drive. You must be either directly connected to the internal network or through VPN for reports to save in this default location.
#'
#' @param fishery_names A character vector of freshwater creel fishery names.
#' @param output_dir Path to the directory where rendered HTML reports will be
#'   saved. Defaults to the standard WDFW Teams QAQC Reports folder. On Windows,
#'   mapped drives are supported (e.g., T:/path/).
#'
#' @return Invisibly returns a summary data frame of render results.
#' @importFrom glue glue
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger cli_rule
#' @importFrom quarto quarto_render
#' @importFrom withr with_dir
#' @examples
#' \dontrun{
#' # Single fishery, default output location
#' generate_report("Hoh winter steelhead 2025-26")
#'
#' # Multiple fisheries, custom output location
#' generate_report(
#'   fishery_names = c("Skagit fall salmon 2025", "Chehalis winter steelhead 2025-26"),
#'   output_dir = "<path>"
#' )
#' }
#' @export
generate_report <- function(
    fishery_names,
    output_dir = "T:/DFW-Team FP FW Creel Monitoring Program - General/Project_Support_Files/Data Quality Assurance/QAQC Reports"
) {

  # Locate template bundled with the package
  template <- system.file("scripts/qaqc_script.qmd", package = "creelreview")
  if (!nzchar(template)) {
    stop("QAQC template not found. Try reinstalling creelreview.")
  }

  # Validate output directory
  output_dir <- normalizePath(output_dir, mustWork = FALSE)
  if (!dir.exists(output_dir)) {
    stop("output_dir does not exist: ", output_dir)
  }

  results <- list()

  for (fishery_name in fishery_names) {

    clean_name  <- gsub(" ", "-", fishery_name)
    output_file <- paste0("qaqc_report_", clean_name, "_", Sys.Date(), ".html")

    cli_alert_info(glue("Rendering: {fishery_name}"))

    results[[fishery_name]] <- tryCatch({

      # Copy template to tempdir , copy output to output_dir
      temp_qmd <- file.path(tempdir(), "qaqc_script.qmd")
      file.copy(template, temp_qmd, overwrite = TRUE)

      # Set wd() temporarily to tempdir() using withr, render quarto doc
      with_dir(tempdir(), {
        quarto_render(
          input          = temp_qmd,
          output_format  = "html",
          output_file    = output_file,
          execute_params = list(fishery_name = fishery_name),
          quiet          = TRUE
        )
      })

      # Copy rendered file from tempdir to output_dir
      file.copy(
        from      = file.path(tempdir(), output_file),
        to        = file.path(output_dir, output_file),
        overwrite = TRUE
      )

      cli_alert_success(glue("Saved: {file.path(output_dir, output_file)}"))
      list(status = "SUCCESS", message = "Saved to output directory", error = NA)

    }, error = function(e) {
      cli_alert_danger(glue("Failed: {fishery_name} - {e$message}"))
      list(status = "FAILED", message = NA, error = e$message)
    })
  }

  # Console summary
  summary_df <- data.frame(
    fishery = names(results),
    status  = sapply(results, `[[`, "status"),
    message  = sapply(results, `[[`, "message"),
    error   = sapply(results, `[[`, "error"),
    row.names = NULL
  )

  cli_rule("Render Summary")
  print(summary_df)

  invisible(summary_df)
}
