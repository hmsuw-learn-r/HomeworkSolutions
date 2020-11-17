#############################################################
## Name: Megan Knight                                      ##
## Purpose: HMS520 Learning Activity 4  -- test function   ##
## for problem 2                                           ##
#############################################################
# Load libraries / functions 
source("/ihme/homes/mknight4/repos/class/HMS520A_Autumn2020_Megan_Knight/homework/01_functions_hw4.2.R")

## problem 2 
regress_group_data(data = mtcars, 
                   group_id = 'carb',
                   obs = 'mpg', 
                   covs = c('wt', 'disp', 'drat'), 
                   include_intercept = FALSE)