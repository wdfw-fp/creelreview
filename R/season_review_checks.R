#used in companion with "Post-season data review_TEMPLATE.docx" file

data <- CreelEstimateR::fetch_dwg("Skagit fall salmon 2024")

# Review data composition across strata ####
# Is there sufficient data, any gaps of concern, or issues with completeness

# "Do all the primary data components have values where expected?"

#create summary table to show amount of data downloaded
summary_table <- tibble::tibble(
  `Data Component` = names(data),
  Records = purrr::map_int(data, nrow)
)

summary_table

## Interviews ####

# "Summarize counts of angler types by section. Are both "boat" and "bank" types represented where expected?

data$interview |>
  dplyr::group_by(section_num, boat_used) |>
  dplyr::summarise(freq = dplyr::n())

# "Summarize the number of interviews per section. Are there sections lacking interview data?"

#table
data$interview |>
  dplyr::group_by(section_num) |>
  dplyr::summarise(n_interviews = dplyr::n())

#bar plot
data$interview |>
  dplyr::group_by(section_num) |>
  ggplot2::ggplot(ggplot2::aes(section_num)) +
  ggplot2::geom_bar()

# "Summarize the number of interviews per section per sampling day. Are there sampling gaps that cause concern?"

data$interview |>
  # dplyr::group_by(event_date, section_num) |>
  ggplot2::ggplot(ggplot2::aes(event_date)) +
  ggplot2::geom_bar() +
  ggplot2::facet_wrap(data$interview$section_num)

## Catch ####

# "Per catch group, summarise the total number of encounters"

#total
data$catch |>
  dplyr::group_by(species) |>
  dplyr::summarise(n = dplyr::n()) |>
  dplyr::arrange(dplyr::desc(n))

#by section
join <- data$interview |> dplyr::select(interview_id, section_num, total_group_count)

data$catch |>
  dplyr::left_join(join, by = "interview_id") |>
  dplyr::group_by(section_num, species) |>
  dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(section_num, dplyr::desc(n)) |>
  print(n = Inf)

# Summarize frequency of reported catch by catch group and section. Are sections lacking data for a given catch group?"

#by catch group
data$catch |>
  dplyr::left_join(join, by = "interview_id") |>
  dplyr::group_by(species, life_stage, fin_mark, fate) |>
  dplyr::summarise(n = dplyr::n()) |>
  dplyr::arrange(dplyr::desc(n)) |>
  print(n = Inf)

#by catch group and section
data$catch |>
  dplyr::left_join(join, by = "interview_id") |>
  dplyr::group_by(section_num, species, life_stage, fin_mark, fate) |>
  dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(section_num, dplyr::desc(n)) |>
  print(n = Inf)

### !!! "Calculate the mark rate of reported catch across fate and section. ####
# Does the AD to UM mark rate align with the expected fishery composition?"

# "Look for potential outliers in catch per interview / group size"

data$catch |>
  dplyr::left_join(join, by = "interview_id") |>
  dplyr::group_by(interview_id, species, total_group_count) |>
  dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
  dplyr::mutate(fish_per_angler = n / total_group_count) |>
  dplyr::arrange(dplyr::desc(fish_per_angler)) |>
  print(n = 50)

# Census effort ####

# !!! "Do all sections have at least one "tie-in" count?" ####

#number of effort counts per section
data$effort |>
  dplyr::filter(
    location_type == "Section",
    survey_type == "Census") |>
  dplyr::distinct() |>
  dplyr::group_by(section_num, count_type) |>
  dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(section_num) |>
  gt::gt(groupname_col = "section_num")

#produce a table of census effort count dates
census_dates <- data$effort |>
  dplyr::filter(tie_in_indicator == 1) |>
  dplyr::select(event_date, section_num, effort_start_time, effort_end_time) |>
  dplyr::distinct()

if (nrow(census_dates) > 0) {
  census_dates |>
    dplyr::mutate(duration = as.numeric(effort_end_time - effort_start_time) / 60) |>
    dplyr::arrange(event_date, effort_start_time) |>
    dplyr::group_by(event_date) |>
    gt::gt(row_group_as_column = TRUE) |>
    gt::tab_stubhead(label = "Survey Date") |>
    gt::tab_header(title = "Census Effort Counts") |>
    gt::tab_style(
      style = list(gt::cell_text(align = "center")),
      locations = gt::cells_body()
    ) |>
    gt::tab_style(
      style = gt::cell_fill(color = "yellow"),
      locations = gt::cells_body(
        columns = c("duration"),
        rows = duration > 60
      )
    ) |>
    gt::cols_label(
      effort_start_time = "Start Time",
      effort_end_time = "End Time",
      section_num = "Section Number",
      duration = "Duration"
    )
}
