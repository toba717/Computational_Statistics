###########################################################################################


get_sqrt <- function(a, x0, tol = 1e-8, iter_max = 1e4, verbose = FALSE){
  func1 <- function(x) x0^2 - a
  fprime <- function(x) 2*x0
  it <- 0
  
  while((abs(func1(x)) > tol) & (it <= iter_max)){
    x0 <- x0 - func1(x0) / fprime(x0)
    it <- it + 1
    if (verbose == TRUE) {
      cat("Approximated square root = ", x0, ", ", "iteration = ", it, "\n", sep = "")
    }
  }
  cat("Square root = ", x0, ", ", "iteration = ", it, "\n", sep="")
}

###########################################################################################

get_abroot <- function(a, root, x0, tol = 1e-8, iter_max = 1e4, verbose = FALSE){
  func1 <- function(x) x0^root - a
  fprime <- function(x) root*x0^(root - 1)
  it <- 0
  
  while((abs(func1(x)) > tol) & (it <= iter_max)){
    x0 <- x0 - func1(x0) / fprime(x0)
    it <- it + 1
    if (verbose == TRUE) {
      cat("Approximated square root = ", x0, ", ", "iteration = ", it, "\n", sep = "")
    }
  }
  cat("Square root = ", x0, ", ", "iteration = ", it, "\n", sep="")
}

###########################################################################################

get_min <- function (f, x0, ... ){
  
  if (is.expression(f) == FALSE) {
    stop("Input of f must be in the form of an expression!")
  }
  
  
  tol <- 1e-6
  i <- 0
  max <- 1e4
  withdot <- list(x0, ...)
  alpha <- withdot$alpha
  n <- withdot$n
  x <- x0
  fp <- D(f, name = "x")
  fpp <- D(fp, name = "x")
  out <- x0
  
  
  while((abs(eval(fp)) > tol) & (i < max)){
    x <- x - eval(fp) / eval(fpp)
    i <- i + 1
    out <- c(out, x)
  }
  
  out
}

###########################################################################################


