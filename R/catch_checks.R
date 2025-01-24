
# effort_na.count.type <- function(data) {
#
#   effort <- data$effort
#   error_count <- sum(is.na(effort$count_type) | effort$count_type == "", na.rm = TRUE)
#
#   if (error_count > 0) {
#     return(create_results_table(
#       pass = FALSE,
#       critical = TRUE,
#       qaqc_check_type = "",
#       error_count = error_count,
#       message = glue::glue("There are {error_count} rows where the count type is NULL or NA.")
#     ))
#   } else {
#     return(create_results_table(
#       pass = TRUE,
#       critical = TRUE,
#       qaqc_check_type = "",
#       error_count = error_count,
#       message = "The 'count_type' field has no missing values."
#     ))
#   }
# }

# Chinook jack length limit
# catch_Chinook.jack <- function(data) {
#
#   Chinook <- data$catch |>
#     dplyr::filter(species == "Chinook", life_stage == "Jack", !is.na(life_stage))
# }



# Coho jack length limit
