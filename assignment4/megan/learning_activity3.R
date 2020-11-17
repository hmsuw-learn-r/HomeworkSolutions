#############################################################
## Name: Megan Knight                                      ##
## Purpose: HMS520: Learning Activity 3                    ##
#############################################################
## clear memory 
rm(list = ls())

#############################################################
######################### problem 1 #########################
#############################################################
## test vector for problem 1.1 - 1.3
z <- c(1,2,3, NA) 

## 1.1 create my sum function 
## arguments: x = vector of integers or doubles, na.rm = logical argument for removing NA values
my_sum <- function(x, na.rm = TRUE){
  ## body for acceptable data types
  if (typeof(x) %in% c('double', 'integer')){
    ## body for removing missings 
    if (na.rm == TRUE) {
      ## subset to non-null values
      x <- x[!is.na(x)]
      ## body for empty vectors post-subseting 
      if (length(x) == 0){
        print('ERROR: x is empty after removing null values')
        sum <- 0
        for(i in seq_along(x)){
          sum <- sum + x[i]
        }
        return(sum)
      ## body for non-empty vectors post-subseting
      } else {
        sum <- 0
        for(i in seq_along(x)){
          sum <- sum + x[i]
        }
        return(sum)
      }
    ## body for not removing missings 
    } else {
      ## body for empty vector that have not been subsetted
      if (length(x) == 0){
        print('ERROR: x is empty')
        sum <- 0
        for(i in seq_along(x)){
          sum <- sum + x[i]
        }
        return(sum)
      ## body for non-empty vectors that have not been subsetted to calculate sum (NOTE: may contain NA)
      } else { 
        sum <- 0
        for(i in seq_along(x)){
          sum <- sum + x[i]
        }
        return(sum)
      }
    }
  ## body for unacceptable data types  
  } else {
    print('ERROR: x argument is not a double or integer atomic vector')
  }
}

## check 1.1 
my_sum(z, na.rm = TRUE)
sum(z, na.rm = TRUE)
my_sum(z, na.rm = FALSE)
sum(z, na.rm = FALSE)

## 1.2 create my_mean function to calculate mean 
## arguments: x = vector of integers or doubles, na.rm = logical argument for removing NA values
my_mean <- function(x, na.rm = TRUE){
  if (na.rm == TRUE){
    do.call(my_sum, list(x, na.rm))/length(x[!is.na(x)])
  } else {
    do.call(my_sum, list(x, na.rm))/length(x)
  }
}

## check 1.2 
my_mean(z, na.rm = TRUE)
mean(z, na.rm = TRUE)
my_mean(z, na.rm = FALSE)
mean(z, na.rm = FALSE)

## 1.3 create my_var function to calculate variance
## arguments: x = vector of integers or doubles, na.rm = logical argument for removing NA values
my_var <- function(x, na.rm = TRUE){
  if (na.rm == TRUE){
    do.call(my_sum, list((x - do.call(my_mean, list(x, T)))^2, na.rm))/(length(x[!is.na(x)])-1)
    
  } else {
    do.call(my_sum, list((x - do.call(my_mean, list(x, T)))^2, na.rm))/(length(x)-1)
  }
}

## check 1.3 
my_var(z, na.rm = TRUE)
var(z, na.rm = TRUE)
my_var(z, na.rm = FALSE)
var(z, na.rm = FALSE)

#############################################################
######################### problem 2 #########################
#############################################################
## test vector for 2.1 - 2.2
y <- 5L
x <- c(1,3)

## 2.1 create Fibonacci sequence function 
## arguments: n = non-negative integer to specify length of Fibonacci sequence
fib <- function(n){
  ## body for acceptable data types and values
  if (typeof(n) == 'integer' & n >= 0){
    ## fib sequence for 0
    if (n == 0){
      return(0)
    ## fib sequence for 1
    } else if (n== 1){
      return(c(0,1))
    } 
    ## fib sequence for positive integers greater than 1
    fib_seq <- rep(0, n + 1)
    fib_seq[2] <- 1
    i <- 3
    while (i <= n + 1){
      fib_seq[i] <- fib_seq[i - 1] + fib_seq[i - 2]
      i <- i + 1
    }
    return(fib_seq)
  ## body for unacceptable data types and values
  } else{
    print('ERROR: n must be a non-negative integer')
  }
}

## check 2.1
fib(y)

## 2.2 extend Fibonacci sequence function with flexible start values 
## arguments: n = non-negative integer to specify length of Fibonacci sequence, start_values = double vector to specify first two values of Fibonacci sequence
fib <- function(n, start_values = c(a0, a1)){
  ## body for acceptable data types and values
  if (typeof(n) == 'integer' & n >= 0){
    ## fib sequence for a0 (user input)
    if (n == 0){
      return(start_values[1])
    ## fib sequence for a0 and a1 (user input)
    } else if (n == 1){
      return(start_values)
    } 
    ## fib sequence for positive integers greater than 1 (user input)
    fib_seq <- rep(start_values[1], n + 1)
    fib_seq[2] <- start_values[2]
    i <- 3
    while (i <= n +1){
      fib_seq[i] <- fib_seq[i - 1]+ fib_seq[i - 2]
      i <- i + 1
    }
    return(fib_seq)
  ## body for unacceptable data types and values
  } else{
    print('ERROR: n must be a non-negative integer')
  }
}

## check 2.2
fib(5L, x)

#############################################################
######################### problem 3 #########################
#############################################################
## test vectors for problems 3.1 - 3.3
a <- c(1,2,3,3,3,4)
b <- c(200,200,30,39,30,2)

## 3.1 create count function to count number of values in a vector 
## arguments: vec = atomic vector with values to be counted, x = value in vec to be counted
count <- function(vec, x){
  if (is.atomic(vec) == TRUE){
    length(vec[vec == x])
  } else {
    print('ERROR: vec not an atomic vector')
  }
}

## check 3.1
count(a, 3)
count(b, 39)

## 3.2 extend count function to count multiple values in a vector
## arguments: vec = atomic vector with values to be counted, x = value in vec to be counted
count <- function(vec, x){
  if (is.atomic(vec) == TRUE){
    count <- rep(0, length(x))
    for (i in seq_along(x)){
      count[i] <- length(vec[vec == x[i]])
    }
    return(count)
  } else {
    print('ERROR: vec argument is not an atomic vector')
  }
}

## check 3.2 
count(a, c(3, 4))
count(b, c(39, 30, 200))

## 3.3 cretae my_unique function to pull unique values and selectively count those values 
## arguments: vec = atomic vector with values to be counted, return_counts = logical argument to return counts of unique values
my_unique <- function(vec, return_counts = FALSE){
  pull_unique <- unique(vec)
  if (return_counts == TRUE){
    return(list(unique = pull_unique, 
                counts = do.call(count, args = list(vec, pull_unique))))
  } else{
    return(pull_unique)
  }
}

## check 3.3 
my_unique(a, return_counts = TRUE)
my_unique(b, return_counts = FALSE)

#############################################################
######################### problem 4 #########################
#############################################################
## test vectors for problems 4.1 - 4.2
events <- rep(100, 3)
sucesses <- c(50, 25, 15)

## 4.1 binomial function 
## bin_fun arguments (n = number of samples, s = sum of the events)
bin_fun <- function(n, s){
  p_hat <- ((1/n)*s)
  stdev <- sqrt((p_hat*(1-p_hat))/n)
  return(list(p_estimate = p_hat, 
              standard_deviation = stdev))
}

## check 4.2 
bin_fun(n = events, s = sucesses)

## 4.2 extending the binomial function 
# my binomial function already works to input sample size of events as a single scalar or a vector
bin_fun(n = 100, s = sucesses)

#############################################################
######################### problem 5 #########################
#############################################################
## test vectors for problem 5 
w <- c(4, 8, 20, 40)
v <- c(400, 800, 2000, 4000)

## 5.1 linear fit model function to produce coefficients
## arguments: x = indepdent variable, y = dependent variable
lin_fit <- function(x, y){
  df <- data.frame(x,y)
  ## run linear model 
  lin_results <- lm(formula = y ~ x, data = df)
  ## pull coefficients from linear model 
  return(coefficients = lin_results$coefficients)
}
lin_fit(w, v)

## 5.2 create polynomial model function to produce coefficients 
## arguments: x = independent variable, y = depdendent variable, degree = polynomial degree 
poly_fit <- function(x, y, degree = 1){
  df <- data.frame(x,y)
  ## run polynomial model 
  poly_results <- lm(formula = y ~ poly(x, degree, raw = TRUE), data = df)
  ## return coefficients from polynomial model 
  return(poly_results$coefficients)
} 
poly_fit(x = w, y = v, degree = 3)

## 5.3 create poly_predict function 
## arguments: x = indepdent values, coef = coeffiencents from polynomial model 
poly_pred <- function(x, coef){
  y_pred <- 0
  ## calculate depdent variables from coefficients and independent variables 
  for (i in seq_along(coef)){
    y_pred <- y_pred + coef[i] * x^(i - 1)
  }
  return(y_pred)
}
poly_pred(x = w, coef = do.call(poly_fit, args = list(x = w, y = v, degree = 3)))

