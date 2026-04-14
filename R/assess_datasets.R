#' Assess multiple fishery datasets
#'
#' @description
#' Runs all registered checks across multiple fisheries and returns a combined
#' results table.
#'
#' @param fishery_names Character vector of fishery names to assess.
#' @param conn Database connection from [creelutils::connect_creel_db()].
#' @param output `"table"` (default) returns raw tibble. `"gt"` returns a
#'   formatted stoplight table.
#' @return Tibble (or gt table) with one row per check per fishery.
#' @export
assess_datasets <- function(
    fishery_names,
    conn,
    output = c("table", "gt")
) {
  output <- match.arg(output)

  results <- purrr::map(fishery_names, \(fn) {

    cli::cli_alert_info("Assessing {.val {fn}}...")

    dwg <- tryCatch(
      creelutils::fetch_dwg(fn),
      error = function(e) {
        cli::cli_alert_danger(
          "Failed to fetch data for {.val {fn}}: {e$message}"
        )
        return(NULL)
      }
    )

    if (is.null(dwg)) {
      return(tibble::tibble(
        fishery_name   = fn,
        pass           = FALSE,
        critical       = TRUE,
        check_category = "data_fetch",
        check_type     = "fetch",
        error_count    = NA_integer_,
        message        = "Failed to fetch data from data.wa.gov",
        status_color   = "red"
      ))
    }

    all <- run_all_checks(dwg, conn)
    all$summary |>
      dplyr::mutate(fishery_name = fn, .before = 1)

  }) |>
    dplyr::bind_rows()

  cli::cli_alert_success(
    "Assessed {length(fishery_names)} fishery dataset{?s}."
  )

  if (output == "gt") return(format_assessment_gt(results))

  results
}


#' Format multi-fishery assessment as a gt stoplight table
#'
#' @param results Tibble from [assess_datasets()].
#' @return A `gt` table object.as
#' @export
format_assessment_gt <- function(results) {

  results |>
    dplyr::mutate(
      status = dplyr::case_when(
        .data$status_color == "green"  ~ "\u2705",
        .data$status_color == "yellow" ~ "\u26a0\ufe0f",
        .data$status_color == "red"    ~ "\u274c",
        TRUE                           ~ "\u2753"
      )
    ) |>
    dplyr::select(
      "fishery_name", "check_type", "status",
      "error_count", "message"
    ) |>
    gt::gt(groupname_col = "fishery_name") |>
    gt::tab_header(
      title = gt::md("**Multi-Fishery Dataset Assessment**"),
      subtitle = paste("Generated", Sys.Date())
    ) |>
    gt::cols_label(
      check_type  = "Category",
      status      = "Status",
      error_count = "Issues",
      message     = "Detail"
    ) |>
    gt::cols_align(align = "center", columns = c("status", "error_count")) |>
    gt::cols_width(
      "check_type"  ~ gt::px(180),
      "status"      ~ gt::px(50),
      "error_count" ~ gt::px(60),
      "message"     ~ gt::px(400)
    ) |>
    gt::tab_style(
      style = list(gt::cell_fill("#f0f0f0"), gt::cell_text(weight = "bold")),
      locations = gt::cells_row_groups()
    ) |>
    gt::tab_options(
      data_row.padding = gt::px(4),
      row_group.padding = gt::px(8),
      table.font.size = gt::px(13)
    ) |>
    gt::sub_missing(columns = "error_count", missing_text = "\u2014")
}
