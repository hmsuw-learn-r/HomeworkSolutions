
# Meta --------------------------------------------------------------------

## HW 03, Problem 1 functions
#
#  Description:
#  Recreated "my_*" functions


# Function definitions ----------------------------------------------------

my_sum <- function(x, na.rm = TRUE) {

  if (!is.numeric(x)) {
    stop("`x` is not an atomic vector of integers or doubles.")
  }

  if (na.rm) { x <- x[!is.na(x)] }

  x_sum <- 0L

  if (length(x) == 0) {
    warning(paste0("Length of `x` when `na.rm = ", na.rm, "` is 0."))
  } else {
    for (i in x) {
      x_sum <- x_sum + i
    }
  }

  x_sum

}

my_mean <- function(x, na.rm = TRUE) {

   x_sum <- my_sum(x, na.rm)

   if (na.rm) {
     x_length <- sum(!is.na(x))
   } else {
     x_length <- length(x)
   }

   x_sum / x_length

}

my_var <- function(x, na.rm = TRUE) {

  # formula: variance = (1 / N-1) * sum((x - x_mean)^2)

  x_mean <- my_mean(x, na.rm)

  if (na.rm) { x <- x[!is.na(x)] }

  x_length <- length(x)

  if (x_length == 0) {
    warning(paste0("Length of `x` when `na.rm = ", na.rm, "` is 0."))
    return(NA_real_)
  }

  (1 / (x_length - 1)) * my_sum((x - x_mean)^2, na.rm)

}
