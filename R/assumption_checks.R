# 1 - Interviews that occur between an index count start and stop time
# violation of sampling assumption
# effort_flag_progressive_counts <- function(data) {
#
#   interview <- data$interview |> dplyr::group_by(event_date)
#   effort <- data$effort |> dplyr::group_by(event_date, count_sequence)
#
#   # Join interview and effort data based on event_date, and check for overlap in time
#   interviews_between_index_counts <- interview |>
#     dplyr::inner_join(effort, by = "event_date") |>
#     dplyr::filter(interview_time >= effort$effort_start_time & interview_time <= effort$effort_end_time)
#
#   return(interviews_between_index_counts)
# }
