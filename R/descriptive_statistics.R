#### initial commit of code for descriptive statistics and supporting data visualization

library(CreelEstimateR)
library(tidyverse)
library(gt)

# create vector of fisheries containing "Nisqually"

data_check <- CreelEstimateR::search_fishery_name("Skagit")

data_2024 <- data_check |>
  purrr::map(~ purrr::keep(.x, names(.x) |> stringr::str_detect("2024")))

# supply this vector to get_fishery_data function to download the data from data.wa.gov

all_data <- CreelEstimateR::get_fishery_data(fishery_names = data_check)


#number of interviews per year + section and grand total
all_data$interview |>
  group_by(year) |>
  # group_by(trip_status, year) |>
  # group_by(fishing_location, year) |>
  count(section_num) |>
  # arrange(fishing_location, year, section_num) |>
  arrange(year, section_num) |>
  gt() |>
  gt::cols_label(
    year ~ "Year",
    section_num ~ "Section Number",
    n ~ "Interview Count"
  ) |>
  grand_summary_rows(
    columns = n,
    fns = "sum"
  )


# what is a useful set of summary statistics to help describe the interview/sampling effort?
# mean daily # of interviews? Rolling 7 day average of interviews to smooth?

all_data$interview |>
  group_by(year, month, event_date) |>
  summarise(
    daily_count = n()
  ) |>
  group_by(year, month) |>
  summarise(
    max = max(daily_count),
    min = min(daily_count),
    mean = mean(daily_count),
    sd = sd(daily_count)
  )


#raw catch data summarize per catch group
# filter to just Chinook catches and summarize by year
all_data$interview |>
  dplyr::left_join(all_data$catch, by = "interview_id") |>
  dplyr::filter(!is.na(fish_count)) |>
  dplyr::filter(str_detect(catch_group, "Chinook")) |>
  # dplyr::select(catch_group, fish_count) |>
  dplyr::group_by(year, catch_group) |>
  dplyr::summarise(count = sum(fish_count)) |>
  pivot_wider(names_from = catch_group, values_from = count, values_fill = NA) |>
  gt()


all_data$effort |>
  filter(tie_in_indicator == 1) |>
  select(year, event_date, location, count_type, count_quantity) |>
  ggplot(aes(event_date, count_quantity, fill = count_type)) +
  # geom_point() +
  geom_bar(stat = "identity") +
  facet_wrap(.~year, scales = "free_x") +
  theme_bw() +
  labs(title = "Census effort counts by angler count type",
       x = "Date",
       y = "Count")


# these are mis leading, should be mean of counts within day and section, fix before using!
all_data$effort |>
  filter(tie_in_indicator == 0) |>
  select(year, event_date, location, section_num, count_sequence, count_type, count_quantity) |>
  dplyr::group_by(year, section_num, event_date, count_sequence, count_type) |> #
  dplyr::summarise(count_index = sum(count_quantity), .groups = "drop") |>
  ggplot(aes(event_date, count_index, fill = count_type)) +
  # geom_point() +
  geom_bar(stat = "identity") +
  facet_wrap(.~year, scales = "free_x") +
  theme_bw() +
  labs(title = "Index effort counts by count type (vehicle/trailer)",
       x = "Date",
       y = "Count")
