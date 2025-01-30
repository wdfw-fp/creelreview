#### initial commit of code for descriptive statistics and supporting data visualization

library(CreelEstimateR)
library(tidyverse)
library(gt)
library(zoo)

# define fisheries we want data for
fisheries <- c("Nisqually salmon 2021", "Nisqually salmon 2022", "Nisqually salmon 2023")

# fisheries <- c("Skagit fall salmon 2021", "Skagit fall salmon 2022", "Skagit fall salmon 2023", "Skagit fall salmon 2024")

# download the data from data.wa.gov
all_data <- set_names(fisheries) |>
  map(~fetch_dwg(fishery_name = .x))

# select and bind the interview data
# functionalize this


interview <- all_data |>
  map(~keep(.x, names(.x) |>  str_detect("interview"))) |>  # Filter for "interview" named objects
  # map(~map(.x, ~mutate(.x, zip_code = as.numeric(zip_code)))) |> # issue binding zip code due to data mismatch, likely when zipcode is.na across an entire fishery dataset
  map_dfr(bind_rows) |>
  mutate(
    month = lubridate::month(event_date),
    year = lubridate::year(event_date),
    week = lubridate::week(event_date),
    fishing_location = if_else(is.na(fishing_location), interview_location, fishing_location)
  )


# interview <- all_data |>
#   map(~keep(.x, names(.x) |>  str_detect("interview"))) |>
#   map_dfr(bind_rows) |>
#   mutate(
#     month = lubridate::month(event_date),
#     year = lubridate::year(event_date),
#     week = lubridate::week(event_date),
#     fishing_location = if_else(is.na(fishing_location), interview_location, fishing_location))
# )



# select and bind the catch data
# functionalize this
catch <- all_data |>
  map(~keep(.x, names(.x) |>  str_detect("catch"))) |>  # Filter for "catch" named objects
  map_dfr(bind_rows)

# select and bind the effort data
# functionalize this
effort <- all_data |>
  map(~keep(.x, names(.x) |>  str_detect("effort"))) |>  # Filter for "catch" named objects
  map_dfr(bind_rows) |>
  mutate(
    month = lubridate::month(event_date),
    year = lubridate::year(event_date),
    week = lubridate::week(event_date))



#number of interviews per year + section and grand total
interview |>
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

interview |>
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
interview |>
  dplyr::left_join(catch, by = "interview_id") |>
  dplyr::filter(!is.na(fish_count)) |>
  dplyr::filter(str_detect(catch_group, "Chinook")) |>
  # dplyr::select(catch_group, fish_count) |>
  dplyr::group_by(year, catch_group) |>
  dplyr::summarise(count = sum(fish_count)) |>
  pivot_wider(names_from = catch_group, values_from = count, values_fill = NA) |>
  gt()


effort |>
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
effort |>
  filter(tie_in_indicator == 0) |>
  select(year, event_date, location, count_type, count_quantity) |>
  ggplot(aes(event_date, count_quantity, fill = count_type)) +
  # geom_point() +
  geom_bar(stat = "identity") +
  facet_wrap(.~year, scales = "free_x") +
  theme_bw() +
  labs(title = "Index effort counts by count type (vehicle/trailer)",
       x = "Date",
       y = "Count")



#dates table
# creel_estimates$stratum |>
#   dplyr::filter(estimate_category == "effort") |>
#   summarise(`Start` = min(min_event_date),
#             `End` = max(max_event_date)) |>
#   gt::gt() |>
#   gt::fmt_date(date_style = "yMd")

