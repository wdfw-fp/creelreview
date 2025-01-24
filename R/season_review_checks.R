#used in companion with "Post-season data review_TEMPLATE.docx" file

library("tidyverse")

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

# pull reference data for study design derived expectations of the data
# some elements could be informed by a future stand alone study design lut
# these tables should provide a basis for evaluating completeness or important things missing in observed data

ref_data <- list(
  sections = data$fishery_manager |> distinct(section_num) |> arrange(section_num), # pull from sections that should have data, not sections with observed data
  boat_used = data$interview |> distinct(boat_used), # pull distinct levels of boat_used from interview data; source from study design lut in future?
  sample_days = data$effort |> distinct(event_date) # effort data should be most reliable observed data for whether sampling occurred
)

ref_data$sections
ref_data$boat_used
ref_data$sample_days

## Interviews ####

# "Summarize counts of angler types by section. Are both "boat" and "bank" types represented where expected?

### EB 1/17/2025 if no data is collected for a level of boat_used, it will be absent from the output. It will be helpful to infer
# 0's from NA's if a desired insight is identifying gaps/anomalies in sampling by various strata.

# expand grid table for all potential levels of section_num and boat_used
section_boat_grid <- expand_grid(ref_data$sections, ref_data$boat_used)

section_boat_grid

data$interview |>
  dplyr::group_by(section_num, boat_used) |>
  dplyr::summarise(freq = dplyr::n()) |>
  right_join(section_boat_grid) |> # right join to attach levels with no observations, value is NA
  mutate(freq = if_else(is.na(freq), 0, freq)) # mutate NA's to inferred 0's

###

# "Summarize the number of interviews per section. Are there sections lacking interview data?"

### EB 1/17/2025 same pattern here - mutate NA's to 0's for sections with no data

# toy example - let's say no data was collected in section 5
data$interview <- data$interview |> filter(!section_num == 5)

#table
int_sec_summ <- data$interview |>
  dplyr::group_by(section_num) |>
  dplyr::summarise(n_interviews = dplyr::n()) |>
  right_join(ref_data$sections) |>
  mutate(n_interviews = if_else(is.na(n_interviews), 0, n_interviews)) |>
  arrange(section_num)

#bar plot
int_sec_summ |>
  ggplot2::ggplot(ggplot2::aes(section_num, n_interviews)) +
  ggplot2::geom_bar(stat = "identity") +
  scale_x_continuous(breaks = round(seq(min(int_sec_summ$section_num), max(int_sec_summ$section_num), by = 1),1))

### that makes it a little easier to spot what's missing


# "Summarize the number of interviews per section per sampling day. Are there sampling gaps that cause concern?"

### think about the schedule, the actual days sampled, and how those data can help convey "gaps" in sampling

data$interview |>
  # dplyr::group_by(event_date, section_num) |>
  ggplot2::ggplot(ggplot2::aes(event_date)) +
  ggplot2::geom_bar() +
  ggplot2::facet_wrap(data$interview$section_num)

## Catch ####

# "Per catch group, summarise the total number of encounters"

### this is species, likely most useful at the catch group

#total
data$catch |>
  dplyr::group_by(species) |>
  dplyr::summarise(n = dplyr::n()) |>
  dplyr::arrange(dplyr::desc(n))

#by section
join <- data$interview |> dplyr::select(interview_id, section_num, total_group_count)


### could use catch_group params to set up reference data in similar pattern to above, explicitly showing where there were no observations
### for catch groups of interest

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
