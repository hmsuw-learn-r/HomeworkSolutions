
# Meta --------------------------------------------------------------------

## HW 03, Problem 1 checks
#
#  Description:
#  Check R built-in functions against "my_*" function variants


# Load libraries / functions ----------------------------------------------

library(testthat)
source("problem_01_functions.R")


# Set up test environment -------------------------------------------------

# Basic atomic vectors
v_int <- 1:5
v_dbl <- 1:5 + .1
v_chr <- letters[1:5]
v_lgl <- c(TRUE, FALSE, TRUE, FALSE)

# Non-atomic objects
l_int <- list(1, 2, 3, 4, 5)

# Atomic numerics with missingness
v_int_na <- c(1:4, NA_integer_, 6:8, NA_integer_, NA_integer_)
v_dbl_na <- c(1:4 + .1, NA, 6:8 + .1, NA, NA)

# Empty atomics
empty_int <- integer(0)
empty_dbl <- double(0)


# Check custom functions --------------------------------------------------

test_that("Assertions are met", {

  expect_error(my_sum(v_chr))
  expect_error(my_sum(v_lgl))
  expect_error(my_sum(l_int))

  expect_error(my_mean(v_chr))
  expect_error(my_mean(v_lgl))
  expect_error(my_mean(l_int))

  expect_error(my_var(v_chr))
  expect_error(my_var(v_lgl))
  expect_error(my_var(l_int))


})

test_that("Functions mimic basic inputs", {

  # Basic inputs
  expect_equal(my_sum(v_int), sum(v_int))
  expect_equal(my_sum(v_dbl), sum(v_dbl))

  expect_equal(my_mean(v_int), mean(v_int))
  expect_equal(my_mean(v_dbl), mean(v_dbl))

  expect_equal(my_var(v_int), var(v_int))
  expect_equal(my_var(v_dbl), var(v_dbl))


})

test_that("Functions mimic NA handling", {

  for (rm_type in c(TRUE, FALSE)) {

    expect_equal(my_sum(v_int_na, na.rm = rm_type), sum(v_int_na, na.rm = rm_type))
    expect_equal(my_sum(v_dbl_na, na.rm = rm_type), sum(v_dbl_na, na.rm = rm_type))

    expect_equal(my_mean(v_int_na, na.rm = rm_type), mean(v_int_na, na.rm = rm_type))
    expect_equal(my_mean(v_dbl_na, na.rm = rm_type), mean(v_dbl_na, na.rm = rm_type))

    expect_equal(my_var(v_int_na, na.rm = rm_type), var(v_int_na, na.rm = rm_type))
    expect_equal(my_var(v_dbl_na, na.rm = rm_type), var(v_dbl_na, na.rm = rm_type))

  }

})

test_that("Functions mimic empty inputs", {

  expect_equal(my_sum(empty_int), sum(empty_int))
  expect_equal(my_sum(empty_dbl), sum(empty_dbl))

  expect_equal(my_mean(empty_int), mean(empty_int))
  expect_equal(my_mean(empty_dbl), mean(empty_dbl))

  expect_equal(my_var(empty_int), var(empty_int))
  expect_equal(my_var(empty_dbl), var(empty_dbl))

})
