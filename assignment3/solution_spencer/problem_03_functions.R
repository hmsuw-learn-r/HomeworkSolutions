
# Meta --------------------------------------------------------------------

## HW 03, Problem 3 functions
#
#  Description:
#  Count and unique related functions


# Function definitions ----------------------------------------------------

count <- function(vec, x) {

  # Validate inputs ---------------------------------------------------------

  if (!is.atomic(vec)) { stop("`vec` must be an atomic vector") }

  if (!is.atomic(x)) { stop("`x` must be an atomic vector") }

  if (typeof(vec) != typeof(x)) { stop("`vec` and `x` must be the same type") }


  # Get counts --------------------------------------------------------------

  vec_x_counts <- vapply(x, function(y) sum(vec == y), FUN.VALUE = integer(1))
  names(vec_x_counts) <- x

  vec_x_counts

}

my_unique <- function(vec, return_counts = FALSE) {

  unique_vec <- unique(vec)

  if (return_counts) {
    return(count(vec, unique_vec))
  } else {
    return(unique_vec)
  }

}
