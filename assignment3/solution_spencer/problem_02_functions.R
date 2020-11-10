
# Meta --------------------------------------------------------------------

## HW 03, Problem 2 functions
#
#  Description:
#  Fibonacci sequence functions


# Function definitions ----------------------------------------------------

fib <- function(n, start_from = c(0L, 1L)) {

  # Validate inputs ---------------------------------------------------------

  if (!is.integer(n) || length(n) != 1 || n < 0) {
    stop("`n` must be a single, non-negative integer")
  }

  if (!is.numeric(start_from) || length(start_from) != 2) {
    stop("`start_from` must be a length 2 numeric vector")
  }


  # Handle base case --------------------------------------------------------

  if (n < 2) { return(start_from[1:(n + 1)]) }


  # Handle other cases ------------------------------------------------------

  fib_seq <- integer(n + 1)
  fib_seq[1:2] <- start_from

  i <- 3L

  while (i <= n + 1) {
    fib_seq[i] <- fib_seq[i - 1] + fib_seq[i - 2]
    i <- i + 1L
  }

  fib_seq

}
