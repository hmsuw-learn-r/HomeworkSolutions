#############################################################
## Name: Megan Knight                                      ##
## Purpose: HMS520 Learning Activity 4 -- test function    ##
## for problem 1                                           ##
#############################################################
# Load libraries / functions 
source("/ihme/homes/mknight4/repos/class/HMS520A_Autumn2020_Megan_Knight/homework/01_functions_hw4.1.R")

## problem 2 
## prep test data by adding a second grouping variable (Color) and NA values 
dt <- data.table(iris)
dt[1:25, Color := 'red']
dt[1:15, Sepal.Length := NA]
dt[26:75, Color := 'white']
dt[76:125, Color := 'purple']
dt[76:85, Sepal.Length := NA]
dt[126:150, Color := 'yellow']

## test summarize_group_data functions
summarize_group_data(dt, obs=c('Sepal.Length', 'Sepal.Width'), group_id = c('Species', 'Color'), fun=mean)
summarize_group_data(dt, obs=c('Sepal.Length', 'Sepal.Width'), group_id = c('Species', 'Color'), fun=mean, na.rm=TRUE)

summarize_group_data(dt, obs=c('Sepal.Length', 'Sepal.Width'), group_id = c('Species', 'Color'), fun=sum)
summarize_group_data(dt, obs=c('Sepal.Length', 'Sepal.Width'), group_id = c('Species', 'Color'), fun=sum, na.rm=TRUE)
