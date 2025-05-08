#used in companion with "Post-season data review_TEMPLATE.docx" file

library("tidyverse")
library("gt")
#loads Skagit winter steelhead 2021 fishery dataset
# data("example_dataset")
# data <- example_dataset

data <- CreelEstimateR::fetch_dwg("Satsop winter steelhead 2024-25")

# data$effort <- data$effort |>
#   filter(section_num == 1)

#catch lacks 'section_num' column, so borrow from interview and join by interview_id
join <- data$interview |>
  dplyr::select(interview_id, total_group_count, section_num)

data$catch <- data$catch |>
  dplyr::left_join(join, by = "interview_id")

#now that catch is done, filter sections from interviews
# data$interview <- data$interview |>
#   filter(section_num == 1)


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
  sections = data$effort |> distinct(section_num) |> arrange(section_num), # pull from sections that should have data, not sections with observed data
  boat_used = data$interview |> distinct(boat_used), # pull distinct levels of boat_used from interview data; source from study design lut in future?
  sample_days = data$effort |> distinct(event_date) # effort data should be most reliable observed data for whether sampling occurred
)

ref_data$sections

ref_data$boat_used
ref_data$sample_days

A## Interviews ####

# "Summarize counts of angler types by section. Are both "boat" and "bank" types represented where expected?

### EB 1/17/2025 if no data is collected for a level of boat_used, it will be absent from the output. It will be helpful to infer
# 0's from NA's if a desired insight is identifying gaps/anomalies in sampling by various strata.

# expand grid table for all potential levels of section_num and boat_used
section_boat_grid <- expand_grid(ref_data$sections, ref_data$boat_used)

section_boat_grid

data$interview |>
  dplyr::group_by(section_num, boat_used) |>
  dplyr::summarise(freq = dplyr::n(), .groups = "keep") |>
  right_join(section_boat_grid, by = c("section_num", "boat_used")) |> # right join to attach levels with no observations, value is NA
  mutate(freq = if_else(is.na(freq), 0, freq)) # mutate NA's to inferred 0's

###

# "Summarize the number of interviews per section. Are there sections lacking interview data?"

### EB 1/17/2025 same pattern here - mutate NA's to 0's for sections with no data

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
  scale_x_continuous(breaks = round(seq(min(int_sec_summ$section_num), max(int_sec_summ$section_num), by = 1),1)) +
  theme_classic()

# "Summarize the number of interviews per section per sampling day. Are there sampling gaps that cause concern?"

### think about the schedule, the actual days sampled, and how those data can help convey "gaps" in sampling

data$interview |>
  ggplot2::ggplot(ggplot2::aes(event_date)) +
  ggplot2::geom_bar() +
  ggplot2::facet_wrap(data$interview$section_num)

## Catch ####

# "Per catch group, summarise the total number of encounters"

#frequency
data$catch |>
  dplyr::group_by(catch_group) |>
  dplyr::summarise(freq = dplyr::n()) |>
  dplyr::arrange(dplyr::desc(freq)) |>
  print(n = 100)


### could use catch_group params to set up reference data in similar pattern to above, explicitly showing where there were no observations
### for catch groups of interest

# abundance by catch group
catch_abundance <- data$catch |>
  dplyr::group_by(section_num, catch_group) |>
  mutate(total = sum(fish_count)) |>
  distinct(total) |>
  dplyr::arrange(dplyr::desc(total))

catch_abundance <- catch_abundance |>
  mutate(species = stringr::str_extract(catch_group, "^[A-Za-z]+(?: [A-Za-z]+)?(?: - Unspecified)?"))

species_of_interest <- c("Chinook", "Coho", "Steelhead")

catch_abundance <- catch_abundance |>
  filter(species %in% species_of_interest)

catch_abundance |>
  ggplot(aes(x = catch_group, y = total)) +
  geom_col() +
  # facet_wrap(~ section_num, labeller = labeller(section_num = function(x) paste0("Section: ", x))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "Abundance")
  # scale_y_continuous(limits = c(0,300), n.breaks =6 ,expand = c(0,0))

# Summarize frequency of reported catch by catch group and section. Are sections lacking data for a given catch group?"

#by catch group and section
catch_freq <- data$catch |>
  dplyr::group_by(interview_id, section_num, catch_group) |>
  dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(section_num, dplyr::desc(n)) |>
  relocate(interview_id, .after = last_col())

catch_freq <- catch_freq |>
  mutate(species = stringr::str_extract(catch_group, "^[A-Za-z]+(?: [A-Za-z]+)?(?: - Unspecified)?"))

species_of_interest <- c("Chinook", "Coho")

catch_freq <- catch_freq |>
  filter(species %in% species_of_interest)

catch_freq |>
  ggplot(aes(x = catch_group, y = n)) +
  geom_col() +
  facet_wrap(~ section_num, labeller = labeller(section_num = function(x) paste0("Section: ", x))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "Frequency")


### !!! "Calculate the mark rate of reported catch across fate and section. ####
# Does the AD to UM mark rate align with the expected fishery composition?"

# "Look for potential outliers in catch per interview / group size"

data$catch |>
  dplyr::group_by(interview_id, catch_group, total_group_count) |>
  dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
  dplyr::mutate(fish_per_angler = n / total_group_count) |>
  dplyr::arrange(dplyr::desc(fish_per_angler))

data$catch |>
  group_by(interview_id, catch_group) |>
  summarise(max = max(fish_count)) |>
  relocate(interview_id, .after = max) |>
  arrange(desc(max)) |>
  print(n = 20)


# Census effort ####

#count types recorded
census_count_types <- data$effort |>
  filter(survey_type == "Census") |>
  distinct(count_type) |>
  arrange(count_type)

census_count_types

# "Do all sections have at least one "tie-in" count?" ####
data$effort |>
  filter(survey_type == "Census") |>
  group_by(section_num, location, count_type) |>
  summarise(n_census_count = n()) |>
  print(n = Inf)

#number of effort counts per section
data$effort |>
  dplyr::filter(
    location_type == "Section",
    survey_type == "Census") |>
  dplyr::distinct() |>
  dplyr::group_by(section_num, count_type) |>
  dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(section_num)

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

## Index effort ####
 n_count_sequence <- data$effort |>
  select(count_sequence) |>
  distinct() |>
  nrow()

n_count_sequence

# Considering ...

data$effort |>
  distinct(section_num, location, event_date, tie_in_indicator, count_sequence) |>
  count(section_num, location, tie_in_indicator) |>
  mutate(tie_in_indicator = case_when(
    tie_in_indicator == 1 ~ "census",
    tie_in_indicator == 0 ~ "index"
  )) |>
  arrange(section_num, tie_in_indicator, location) |>
  gt(groupname_col = "section_num", rowname_col = "location") |>
  tab_style(
    style = list(cell_fill("grey70"), cell_text(weight = "bold")),
    locations = cells_body(rows = tie_in_indicator == "census")
  )

# Total number of census effort surveys by water_body, event_date, and section_num;
## NOTE: NAs in the "effort_type.y" column indicate that a census survey was conducted without the paired index count; if this occurs, the census data without a paired index count needs to be filter out for BSS model to run
left_join(
  data$effort |>
    filter(tie_in_indicator == 1) |>
    mutate(effort_type = "Census") |>
    select(water_body, event_date, section_num, effort_type) |>
    distinct() |>
    arrange(event_date, section_num),
  data$effort |>
    filter(tie_in_indicator == 0) |>
    mutate(effort_type = "Index") |>
    select(water_body, event_date, section_num, effort_type) |>
    distinct() |>
    arrange(event_date, section_num),
  by = c("water_body", "event_date", "section_num")
) |> print(n= Inf)

### Are there any unexpected count sequences?

data$effort %>%
  filter(survey_type == "Index") %>%
  ggplot(aes(x = count_sequence)) +
  geom_histogram() +
  labs(y = "Number of index effort counts", x = "Index effort counts per day") +
  scale_x_continuous(breaks = (seq(1, n_count_sequence, by = 1))) +
  theme_bw()

### Were there any index sites missed systematically during the data quality assurance process that should be noted here?

missing_index_counts <- function() {
  #sites visited
  surveyed_index_sites <- data$effort |>
    filter(location_type == "Site") |>
    select(event_date, section_num, location) |>
    distinct()

  #study design
  expected_index_sites <- data$fishery_manager |>
    filter(location_type == "Site") |>
    group_by(section_num, location_code) |>
    summarize()

  #expand expected list by surveyed dates
  expected_index_sites_dates <- surveyed_index_sites |>
    distinct(event_date) |>
    cross_join(expected_index_sites) |>
    select(section_num, location_code, event_date)

  # Perform anti-join to identify missing index sites
  missing_index_sites <- anti_join(expected_index_sites_dates, surveyed_index_sites, by = c("location_code" = "location", "section_num", "event_date"))

  # If there are missing sites, create a summary table
  if (nrow(missing_index_sites) > 0) {
    missing_index_sites_summary <- missing_index_sites |>
      group_by(section_num, location_code) |>
      summarize(min_date = min(event_date),
                max_date = max(event_date),
                num_dates_missed = n())  |>
      ungroup()

    # Plot summary table
    missing_index_sites_summary |>
      gt() |>
      tab_header("Summary of index sites missing from effort count data") |>
      tab_spanner(label = "Survey dates missed",
                  columns = c("min_date", "max_date", "num_dates_missed")) |>
      cols_label(
        section_num = "Section number",
        location_code = "Site name",
        min_date = "First",
        max_date = "Most recent",
        num_dates_missed = "Number"
      ) |>
      cols_align(
        align = "center",
        columns = everything()
      )
  } else {
    cat("There are no index sites missing from effort count data.<br>","*This requires correct setup of Fishery Manager Table on FishApps with project study design.*")
  }
}

missing_index_counts()
