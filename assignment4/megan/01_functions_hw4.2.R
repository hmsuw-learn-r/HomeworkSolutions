#############################################################
## Name: Megan Knight                                      ##
## Purpose: HMS520 Learning Activity 4 -- test function    ##
## for problem 2                                           ##
#############################################################
## clear memory 
rm(list = ls())

## load packages 
pacman::p_load(dplyr, tidyr, ggplot2, data.table, broom)

## Problem 2
## create regress_group_data function with the following arguments
##    data = data frame
##    group_id = character vector with one or more variables to group data by
##    obs = character vector with one or more variables to summarize 
##    covs = 
##    include_intercept = 
##    ... = extra arguments for summarize_at function
regress_group_data <- function(data, group_id, obs, covs, include_intercept = TRUE, ...){
  ## produce formula for linear regression
  covariates <- paste0(covs, collapse = " + ")
  formula <- paste0(obs, ' ~ ', covariates)
  if (include_intercept == FALSE){
    formula <- paste0(formula, ' -1')
  }
  ## grouped linear regression
  df <- data %>% 
    group_by_at(.vars = group_id) %>% 
    do(tidy(lm(formula, data = ., ...))) %>%
    select(c(group_id, 'term', 'estimate')) 
  
  ## transform data to produce data frame
  df <- pivot_wider(df, id_cols = group_id, names_from = 'term', values_from = 'estimate')
  names(df)[names(df) == "(Intercept)"] <- "intercept"
  return(as.data.frame(df))
}
