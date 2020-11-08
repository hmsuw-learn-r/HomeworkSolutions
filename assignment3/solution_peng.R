# Assignment 3 Solution
# ==============================================================================
rm(list = ls())

# Assert functions
# ------------------------------------------------------------------------------
assert_length <- function(x, len) {
  if (length(x) != len) stop(paste0("Assertion Error: length must be ",
                                    as.character(len), "."))
}

assert_atomic <- function(x, len = NA_integer_) {
  if (!is.atomic(x)) stop("Assertion Error: object must be a atomic vector.")
  if (!is.na(len)) assert_length(x, len)
}

assert_vector <- function(x, len = NA_integer_) {
  if (!is.vector(x)) stop("Assertion Error: object must be a vector.")
  if (!is.na(len)) assert_length(x, len)
}

assert_numeric <- function(x, len = NA_integer_) {
  if (!is.numeric(x)) stop("Assertion Error: type must be numeric.")
  if (!is.na(len)) assert_length(x, len)
}

assert_integer <- function(x, len = NA_integer_) {
  if (!is.integer(x)) stop("Assertion Error: type must be integer.")
  if (!is.na(len)) assert_length(x, len)
}

assert_double <- function(x, len = NA_integer_) {
  if (!is.double(x)) stop("Assertion Error: type must be double.")
  if (!is.na(len)) assert_length(x, len)
}

# Problem 1
# ------------------------------------------------------------------------------
my_length <- function(x, na.rm = TRUE) {
  ifelse(na.rm, sum(!is.na(x)), length(x))
}

my_sum <- function(x, na.rm = TRUE) {
  assert_numeric(x)
  sum_x <- vector(typeof(x), 1)
  if (length(x) == 0) {
    warning("Input zero length vector.")
    return(sum_x)
  }
  if (na.rm) x <- x[!is.na(x)]
  for (xi in x) sum_x <- sum_x + xi
  sum_x
}

my_mean <- function(x, na.rm = TRUE) {
  my_sum(x, na.rm)/my_length(x, na.rm)
}

my_var <- function(x, na.rm = TRUE, bessel_correction = TRUE) {
  mu <- my_mean(x, na.rm)
  n <- my_length(x, na,rm)
  if (bessel_correction) n <- n - 1L
  my_sum((x - mu)^2, na.rm) / n
}

# Problem 2
# ------------------------------------------------------------------------------
fib <- function(n, start_from = c(0L, 1L)) {
  assert_numeric(n)
  assert_numeric(start_from, len = 2L)
  if (n < 0) stop("n must be non-negative.")
  n <- as.integer(n)
  
  len <- n + 1L
  if (n <= 1L) {
    return(start_from[1:len])
  } else {
    v <- vector(typeof(start_from), len)
    v[1:2] <- start_from
    i <- 3
    while (i <= len) {
      v[i] <- v[i - 1L] + v[i - 2L]
      i <- i + 1L
    }
    return(v)
  }
}

# Problem 3
# ------------------------------------------------------------------------------
count_single <- function(vec, x) {
  assert_atomic(vec)
  assert_length(x, 1L)
  sum(vec == x)
}

count <- function(vec, x) {
  n <- sapply(x, function(t) count_single(vec, t))
  names(n) <- x
  n
}

my_unique <- function(vec, return_counts = FALSE) {
  unique_vals <- unique(vec)
  if (!return_counts) return(unique_vals)
  count(vec, unique_vals)
}

# Problem 4
# ------------------------------------------------------------------------------
binomial_fun <- function(s, n) {
  assert_numeric(s)
  assert_numeric(n)
  s <- as.integer(s)
  n <- as.integer(n)
  
  if (length(s) != length(n) & !(length(s) == 1 | length(n) == 1)) {
    stop("Length of s and n must match.")
  }
  if (!all(s <= n)) stop("Numbers of events must be smaller than sample sizes.")
  bin_p <- s/n
  bin_s <- sqrt(bin_p*(1 - bin_p)/n)
  list(mean = bin_p, sd = bin_s)
}

# Problem 5
# ------------------------------------------------------------------------------
lin_fit <- function(y, x) {
  assert_vector(y)
  assert_numeric(y)
  assert_numeric(x)
  as.vector(lm(y ~ x)$coefficients)
}

poly_mat <- function(x, degree = 1L, include_intercept = TRUE) {
  assert_numeric(x)
  assert_numeric(degree, 1L)
  degree <- as.integer(degree)
  
  mat <- poly(x, degree, raw = TRUE)
  if (include_intercept) mat <- cbind(1, mat)
  mat
}

poly_fit <- function(y, x, degree = 1L) {
  mat <- poly_mat(x, degree = degree, include_intercept = FALSE)
  lin_fit(y, mat)
}

poly_pred <- function(x, coefs) {
  assert_numeric(x)
  assert_numeric(coefs)
  mat <- poly_mat(x, length(coefs) - 1L)
  as.vector(mat %*% coefs)
}
