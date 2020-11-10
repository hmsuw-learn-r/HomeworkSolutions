
# Meta --------------------------------------------------------------------

## HW 03, Problem 5 functions
#
#  Description:
#  Polynomial fitting functions


# Function definitions ----------------------------------------------------

poly_fit <- function(x, y, degree = 1) {

  if (!is.numeric(degree) || length(degree) != 1) {
    stop("`degree` must be a length 1 integer-ish value")
  }

  degree <- as.integer(degree)

  data <- data.frame(
    y = y,
    x = x
  )

  if (degree > 1) {

    for (d in 2:degree) {
      data[paste0("x", d)] <- x ^ d
    }

  }

  lm_formula <- paste0("y ~ ", paste0(tail(colnames(data), -1), collapse = " + "))

  lm_model <- lm(as.formula(lm_formula), data = data)

  lm_model$coefficients

}

lin_fit <- function(x, y) {
  poly_fit(x, y, degree = 1)
}


poly_pred <- function(x, coef) {

  sapply(x, function(xx) {
    sum(sapply(1:length(coef), function(i) coef[i] * xx^(i - 1)), na.rm = TRUE)
  })

}
