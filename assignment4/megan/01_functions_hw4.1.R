#############################################################
## Name: Megan Knight                                      ##
## Purpose: HMS520 Learning Activity 4 -- function for     ##
## problem 1                                               ##
#############################################################
## clear memory 
rm(list = ls())

## load packages 
pacman::p_load(dplyr, tidyr, ggplot2, data.table, broom)

## Problem 1 
## Create summarize_group_data function with the following arguments
##    data = data frame
##    group_id = character vector with one or more variables to group data by
##    obs = character vector with one or more variables to summarize 
##    fun = summary function
##    ... = extra arguments for summarize_at function
summarize_group_data <- function(data, group_id, obs, fun, ...){
  grouped_data <- data %>%
    group_by_at(.vars = group_id) %>%
    summarize_at(.vars = obs,
                 .funs = fun, ...)
  return(as.data.frame(grouped_data))
}


