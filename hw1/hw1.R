gcd <- function(x, y) {
  # This function finds the greatest common divisor between two integer that the user imputs
  # Args:
  # x: integer of size 1 
  # y: integer of size 1
  # Return:
  # The greatest common divisor of the integers x and y, size 1
  
  if ((x %% 1 != 0) | (y %% 1 != 0)) {
    warning("One or more of your input is not an integer!")
  }else if (is.character(x) | is.character(y)){
    warning("One or more of your input is a character! Greatest common divisor cannot be calculated!")
  }else if (is.logical(x) | is.logical(y)){
    warning("One or more of your input is logical! Greatest common divisor cannot be calculated!")
  }else{
    out <- integer(0)
    
    if (x == 0) {
      out <- y
      
    }else if (y == 0) {
      out <- x
      
    }else{
      
      mid <- max(abs(x), abs(y))
      y <- min(abs(x), abs(y))
      x <- mid
      remainder <- x %% y
      
      if (remainder == 0) {
        
        out <- y
        
      }else{
        
        while (remainder != 0) {
          x <- y
          y <- remainder
          remainder <- x %% y
        }
        out <- y
      }
    }
    out
  }
}







lcm <- function(a) {
  # This function finds the least common multiple between two to a hundred integers that the user inputs
  # Args:
  # a: integer of size 2 through a 100
  # Return:
  # The least common multiple of the inputted integers, size 1
  
  if (length(a) > 100) {
    warning("The input has over 100 values!")
  }else if (length(a) < 2) {
    warning("The input does not have enough values!")
  }else if (sum(is.na(a)) > 0){
    warning("There is one or more NAs in your input!")
  }else if (is.character(a)){
    warning("The input is a character vector!")
  }else if (is.logical(a)){
    warning("The input is a logical vector!")
  }else{
    
    for (i in (seq_len(length(a) - 1))) {
      out <- (a[i] * a[i + 1]) / gcd(a[i], a[i + 1])
      a[i + 1] <- out
    }
    
    out
  }
}





is_prime <- function(b) {
  # This function inputs a numerical vector and outputs a logical vector where TRUE represents that the corresponding value is a prime number and FALSE represents that the corresponding value is not a prime number 
  # Args:
  # b: integer of varying size
  # Return:
  # Logical Vector of the same size as the input that correspondingly tells whether the number in the vector is a prime number or not
  

  if (is.character(b)){
    warning("This is a character vector!")
  }else if (is.logical(b)){
    warning("This is a logical vector!")
  }else if (sum(is.na(b)) > 0){
    warning("There are NAs in data!")
  }else if (sum(b) %% 1 != 0) {
    warning("Not all of the inputted values are integers!")
  }else{
    out <- logical(length(b))
    tally <- integer(length(b))
    
    for (i in seq_len(length(b))) {
      
      for (j in seq_len(b[i])) {
        if (b[i] %% j ==0) {
          tally[i] <- tally[i] + 1
        }
      }
      
      if (tally[i] == 2) {
        out[i] <- TRUE
      }
      
    }
    out
  }
}




get_factors <- function(c) {
  # This function inputs a single integer and returns a list object with a vector of unique prime factors of the input and the corresponding exponents 
  # Args:
  # c: integer of size 1
  # Return:
  # List object with size 2, The first component of the list is the unique prime factors and the second component of the list is the exponents of the corresponding exponent to the prime factor.
  
  if(is.character(c)){
    warning("The input is a character!")
  }else if (is.logical(c)){
    warning("The input is logical!")
  }else if (c == 1 | c == 0 | c == -1) {
    warning("The value doesn't have a prime factor!")
  }else{
    
    prime <- integer(0)
    exponents <- integer(0)
    tally <- 0
    inter_variable <- c
    
    for (i in seq_len(c)) {
      if ((c %% i == 0) & is_prime(i)) {
        prime <- c(prime, i) 
        
        while (inter_variable %% i == 0){
          inter_variable <- inter_variable %/% i
          tally <- tally + 1
        }
        
        exponents <- c(exponents, tally)
        tally <- 0
      }
    }
    
    list("primes" = prime, "exponents" = exponents)
  }
  
}

