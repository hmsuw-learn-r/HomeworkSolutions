# -------------------------------------------------------------------------
# Author: Sam Farmer
# Date 10/10/2020
# Purpose: This assignment explores how to create vectors and lists using 
# built in R functions

# -------------------------------------------------------------------------
#Setup
rm(list = ls())

# problem 1 ---------------------------------------------------------------
# Create various vectors
# 1
vector1 <- c(1:100)
# 2
vector2 <- rep(10, 100)
# 3
vector3 <- rep(c(1, 2, 3, 4, 5), 10)
# 4
vector4 <- rep(c(1, 2, 3, 4, 5), each=10)
# 5
vector5 <- seq.int(0.00, 1.00, 0.01)


# problem 2 ---------------------------------------------------------------
# Rivers dataset practice
# 1
help(rivers)
paste0("type of rivers data set is an atomic vector of: ", typeof(rivers))

# 2
# Create an atomic vector of Rivers that contains the length, sum,
# mean, median, variance, standard deviation, minimum and maximum of 
# log-transformed rivers
log_statistics <- function(data){
  log_data <- log(data)
  data_len <- length(log_data)
  data_sum <- sum(log_data)
  data_mean <- mean(log_data)
  data_median <- median(log_data)
  data_var <- var(log_data)
  data_stddev <- sd(log_data)
  data_min <- min(log_data)
  data_max <- max(log_data)
  data_stats <- c("length"= data_len, "sum"= data_sum,
                  "mean"= data_mean, "median"= data_median,
                  "variance"= data_var, "stddev"= data_stddev,
                  "min"= data_min, "max"= data_max)
  return(data_stats)
}

log_river_stats <- log_statistics(rivers)
# 3
# Remove the 10 least and 10 greatest values in Rivers then create descriptive
# stats vector from it
log_trimriver_stats <- log_statistics(sort(rivers)[11:(length(rivers)-10)])


# Problem 3 ---------------------------------------------------------------
# Create a list and modify its contents
# 1 
# create a list 'u' with two items x = c(5, 6, 7, 8) and y = c("a", "b", "c", "d")
u <- list(x = c(5, 6, 7, 8), y = c("a", "b", "c", "d"))

# 2
# Modify y in u such that it has numerical values c(1, 2, 3, 4).
u$y <- c(1, 2, 3, 4)

# 3
# What is the best way to compute the mean of all elements in x and y?
mean_u <- lapply(u, mean) # compute mean for each vector within list
mean_u_allvals <- mean(unlist(u)) # compute mean for all values in list

# 4 
# Add x2 = x^2 and log_x = log(x) into the list.
u$"x^2" <- u$x^2
u$"log_x" <- log(u$x)

# 5
# Remove log_x from the list.
u$log_x <- NULL


# Problem 4 ---------------------------------------------------------------
# 1
# Create 3 vectors that will go into matrix
x <- c(1, 2, 3)
y <- c(4, 5, 6)
z <- c(7, 8, 9)

# 2
# Create row matrix from 3 vectors
row_matrix <- rbind(x, y, z)

# 3
# Create a col matrix from 3 vectors
col_matrix <- cbind(x, y, z)

# 4
# Reshape a vector into a 4x3 matrix by col
a <- c(1:12)
matrix_a_col <- matrix(a, ncol = 3, nrow = 4)

# 5
# Reshape a vector into a 4x3 matrix by rows
matrix_a_row <- matrix(a, ncol = 3, nrow = 4, byrow = TRUE)


# Problem 5 ---------------------------------------------------------------
# Matrix operations
# 1
# Create 3 matrices A, B, C
A <- matrix(c(1:12), nrow = 3)
B <- matrix(c(1:16), nrow = 4)
C <- matrix(c(16:1), nrow = 4)

# 2
# Multiplication. What does B * C do?
B_by_C <- B*C

# B_by_C
# [,1] [,2] [,3] [,4]
# [1,]   16   60   72   52
# [2,]   30   66   70   42
# [3,]   42   70   66   30
# [4,]   52   72   60   16
# 
# Resulting matrix is multiplication by column so:
# B[1,1] <- 1 multiplied by  C[1,1] <- 16 results in 16
# B[1, 2] <- 2 multiplied by C[1, 2] <- 15 results in 30

# 3
# Matrix Multiplication. What does A %*% B do?
A_by_B <- A %*% B

# A_by_B
# [,1] [,2] [,3] [,4]
# [1,]   70  158  246  334
# [2,]   80  184  288  392
# [3,]   90  210  330  450
#
# Resulting matrix is actual matrix multiplication so:
# A columns [1, 2, 3, 4] * B rows[1, 2, 3, 4] -> 
# A vals(1, 4, 7, 10) * B vals(1, 2, 3, 4)
# 1*1 + 4*2 + 7*3 + 10*4 = 70

# 4
# Compute the sum of diagonal elements in B
sum_diag_B <- sum(diag(B))
