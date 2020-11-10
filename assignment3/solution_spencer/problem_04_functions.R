
# Meta --------------------------------------------------------------------

## HW 03, Problem 4 functions
#
#  Description:
#  Binomial distribution estimation function


# Function definitions ----------------------------------------------------

binomial_fun <- function(s, n) {

  if (!is.numeric(s)) { stop("`s` must be a numeric atomic") }
  if (!is.numeric(n)) { stop("`n` must be a numeric atomic") }

  if (length(n) != length(s) && length(n) != 1) {
    stop("`n` must be either the same length of `s` or length 1")
  }

  p_hat <- s / n
  v_hat <- p_hat * (1 - p_hat) / n

  return(list("p_hat" = p_hat, "std_dev" = sqrt(v_hat)))

}
