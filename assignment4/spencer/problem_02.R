
# Meta --------------------------------------------------------------------

## HW 04, problem 2
#
#  Description:
#  Build function to generate linear models over grouped data


# Load libraries ----------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)


# Define function ---------------------------------------------------------

regress_group_data <- function(data,
                               group_id,
                               obs,
                               covs,
                               include_intercept = TRUE,
                               ...) {

  # Formula: obs ~ 0/1 + cov_1 + cov_2 + ... + cov_n
  lm_formula <- formula(paste(
    obs,
    "~",
    as.integer(include_intercept),
    "+",
    paste0(covs, collapse = " + "))
  )

  # Generate models varying only in input data (useful for mapping)
  model_generator <- function(data) lm(lm_formula, data = data, ...)

  data %>%
    group_by(across(all_of(group_id))) %>%
    tidyr::nest() %>%
    mutate(
      model = purrr::map(data, model_generator),
      coef = purrr::map(model, coef)
    ) %>%
    tidyr::unnest_wider(coef) %>%
    select(-data, -model)

}
