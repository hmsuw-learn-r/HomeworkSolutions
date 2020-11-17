
# Meta --------------------------------------------------------------------

## HW 04, problem 1
#
#  Description:
#  Build function to summarize grouped data


# Load libraries ----------------------------------------------------------

library(dplyr)


# Define function ---------------------------------------------------------

summarise_group_data <- function(data, group_id, obs, fun, ...) {

  data %>%
    group_by(across(all_of(group_id))) %>%
    summarise(across(all_of(obs), fun, ...))

}
