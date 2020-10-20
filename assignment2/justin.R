##### Homework 2 ----
# Author: Justin Lo
# Date: 10/05/2020

rm(list = ls())
library(dplyr)

##### Problem 1 ----

# Question 1
vec1 <- seq(1, 100)

# Question 2
vec2 <- rep(10, 100)

# Question 3
vec3 <- rep(seq(1, 5), 10)

# Question 4
vec4 <- sort(rep(seq(1, 5), 10))

# Question 5
vec5 <- seq(0, 1, length.out = 101)

##### Problem 2 ----

# Question 1 ----
rivers

typeof(rivers)

# Rivers is an atomic vector that is a double type.

# Question 2 ----
rivers_summary <- c("length" = length(rivers),
                    "sum" = sum(rivers),
                    "mean" = mean(rivers),
                    "median" = median(rivers),
                    "var" = var(rivers),
                    "sd" = sd(rivers),
                    "min" = min(rivers),
                    "max" = max(rivers))
rivers_summary

# Question 3 ----

# Sort by value
rivers_trimmed <- sort(rivers)

# Drop first 10 and last 10 elements
rivers_trimmed <- rivers_trimmed[-c(1:10, (length(rivers) - 9):length(rivers))]

trimmed_summary <- c("length" = length(rivers_trimmed),
                     "sum" = sum(rivers_trimmed),
                     "mean" = mean(rivers_trimmed),
                     "median" = median(rivers_trimmed),
                     "var" = var(rivers_trimmed),
                     "sd" = sd(rivers_trimmed),
                     "min" = min(rivers_trimmed),
                     "max" = max(rivers_trimmed))

trimmed_summary

##### Problem 3 ----

# Question 1 ----
u <- list(x = 5:8,
          y = c("a", "b", "c", "d"))

# Question 2 ----
u$y <- 1:4

# Question 3 ----
# We can use the unlist function to extract all elements of the list u.
mean(unlist(u))

# Question 4 ----
u$x2 <- (u$x)^2
u$log_x <- log(u$x)

# Question 5 ----
u$log_x <- NULL

##### Problem 4 ----

# Question 1 ----
x <- seq(1, 3)
y <- seq(4, 6)
z <- seq(7, 9)

mat1 <- matrix(c(x, y, z), byrow = TRUE, nrow = 3)

# Question 2 ----
mat2 <- matrix(c(x, y, z), byrow = FALSE, nrow = 3)

# Question 3 ----
a <- seq(1, 12)
mat3 <- matrix(a, byrow = FALSE, nrow = 4)

# Question 4 ----
mat4 <- matrix(a, byrow = TRUE, nrow = 4)

##### Problem 5 ----

# Question 1 ----
A <- matrix(c(seq(1, 12)), nrow = 3)
B <- matrix(c(seq(1, 16)), nrow = 4)
C <- matrix(c(rev(seq(1, 16))), nrow = 4)

# Question 2 ----
prod_B_C <- B*C

# This operation multiplies each element in matrix B by its respective element in C with the same position.

# Question 3 ----
dot_A_B <- A %*% B

# This operation computes the dot product of the two matrices A and B.

# Question 4 ----
sum(diag(B))
