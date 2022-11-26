####################################################################################
pqnumber <- function(sign, p, q, nums){
  #This function is the constructor function which creates the pqnumber utilizing the sign, p, q, and the nums that the user inputs
  #Args:
  #sign: integer of either -1 or 1, size of 1
  #p: integer between 0 and 9 including 0 and 9, size of 1
  #q: integer between 0 and 9 including 0 and 9, size of 1
  #nums: numeric vector, size of p+q+1
  #Return:
  #out: pqnumber object size of 4
  
  if (sign != 1 & sign != -1) {
    stop("Invalid input for sign argument!")
  }else if (!is.numeric(p)) {
    stop("The input value for p must be numeric!")
  }else if (!is.numeric(q)){
    stop("The input value for q must be numeric!")
  }else if ((p < 0) | (q < 0)) {
    stop("The input values for p and q must be positve!")
  }else if ((p%%1 + q%%1) > 0) {
    stop("The input values for p and q must be integers!")
  }else if (length(nums) != (p + q + 1)) {
    stop("The input vector of nums must have the length of p + q + 1!")
  }else if ((sum(nums > 0) != length(nums)) & (sum(nums < 9) != length(nums))) {
    stop("Each component of the input vector of nums needs to be between 0 and 9")
  }else{
    
    
    
    out <- structure((list("sign" = sign, "p" = p, "q" = q, "nums" = nums)), class = "pqnumber")
    
    out
  }
}  


###################################################################################

is_pqnumber <- function(x){
  # A function that returns true if input is a pqnumber, false if elsewise
  #Arg:
  #x: object, size = 1
  #Return:
  #TRUE or FALSE, size = 1
  
  if((inherits(x, "pqnumber"))){
    TRUE
  }else{
    FALSE
  }
}


####################################################################################

print <- function(x, DEC = FALSE){
  #A function that is used to print the given input
  #Args:
  #x: input variable, size of 1
  #DEC: can be either TRUE or FALSE, size of 1

  UseMethod("print")
}

####################################################################################

print.pqnumber <- function(x, DEC = FALSE){
  #A function that is used to print the given input
  #Args:
  #x: input variable, size of 1
  #DEC: can be either TRUE or FALSE, size of 1
  #Return:
  # Returns components of x and also phrases such as "sign = " or "p = "
  
  print(c(noquote("sign = "), x$sign))
  print(c(noquote("p = "), x$p))
  print(c(noquote("q = "), x$q))
  print(c(noquote("nums = "), x$nums))
  
  
  if (DEC == TRUE) {
    out <- integer(x$p + x$q + 2)
    
    for (i in seq_len(x$p + x$q + 2)) {
      out[i] <- x$nums[i] * (10^(-x$p + i - 1))
      if(is.na(x$nums[i])) {
        out[i] <- 0
      }
    }
    
    
    if (x$sign == 1){
      print(c(noquote("Decimal = "), sum(out)))
    }else if (x$sign == -1){
      print(c(noquote("Decimal = "), -sum(out)))
    }
  }
  
  
  
}


####################################################################################

as_pqnumber <- function(x, p, q){
  #Converts the user's input of x, p, q into a pqnumber
  #Args: 
  #x: An integer, size = 1
  #p: An integer between 0 and 9, size = 1
  #q: An integer between 0 and 9, size = 1
  #Return:
  #out: a pqnumber object
  
  
  if (!is.numeric(p)) {
    stop("The input value for p must be numeric!")
  }else if (!is.numeric(q)){
    stop("The input value for q must be numeric!")
  }else if ((p < 0) | (q < 0)) {
    stop("The input values for p and q must be positve!")
  }else if ((p%%1 + q%%1) > 0) {
    stop("The input values for p and q must be integers!")
  }else{
    intervar = rep(0, p + q + 1)
    
    exponents = -p:q
    
    if (x < 0) {
      sign <- -1
    }else if (x > 0) {
      sign <- 1
    }else {
      sign <- 0
    }
    
    
    
    for(i in seq_len(length(exponents))) {
      intervar[i] = abs(x) %% 10 ^ (exponents[i] + 1)
      if((intervar[i] * 10 ^ (-exponents[i])) >= 9.5) {
        intervar[i] = 0
        print((intervar[i] * 10 ^(-exponents[i])))
        
      }else {
        x = abs(x) - intervar[i]
        intervar[i] = intervar[i] * 10 ^ (-exponents[i])
      }
      
    }
    
    
    out <- structure((list("sign" = sign, "p" = p, "q" = q, "nums" = round(intervar))), class = "pqnumber")
    
    out
  }
  
}

####################################################################################


as_numeric <- function(x) {
  #Converts pqnumber into numeric
  #Arg:
  #x: pqnumber object
  #Return:
  #out: integer
  out <- integer(0)
  
  for (i in seq_len(x$p + x$q + 1)) {
    out[i] <- x$nums[i] * (10^(-x$p + i - 1))
  }
  
  
  if (x$sign == 1){
    sum(out)
  }else if (x$sign == -1){
    -sum(out)
  }
}

####################################################################################

carry_over <- function(x1){
  #A carry over function that will be utilized in the add function
  #Arg:
  #x1: integer, size = 1
  #Return:
  #x1 %% 10
  x1 %% 10
}

####################################################################################

add <- function(x, y){
  #A function that adds two pqnumbers
  #Args:
  #x: pqnumber
  #y: pqnumber
  #Return:
  #out: also a pqnumber with the pqnumbers x and y added
  
  if (x$p != y$p) {
    var1 <- max(x$p, y$p) - min(x$p, y$p)
    
    if (x$p < y$p){
      x$nums <- c(rep(0, var1), x$nums)
    }else if (x$p > y$p) {
      y$nums <- c(rep(0, var1), y$nums)
    }
    
  }
  
  if(x$q != y$q) {
    var2 <- max(x$q, y$q) - min(x$q, y$q)
    
    if (x$q < y$q) {
      x$nums <- c(x$nums, rep(0, var2))
    }else if (x$q > y$q) {
      y$nums <- c(y$nums, rep(0, var2))
    }
    
  }
  
  if(x$sign == -1){
    x$nums <- x$nums*-1
  }
  if(y$sign == -1){
    y$nums <- y$nums*-1
  }
  
  out <- structure((list("sign" = 1, "p" = max(x$p, y$p), "q" = max(x$q, y$q), "nums" = integer(max(x$p, y$p) + max(x$q, y$q) + 2))), class = "pqnumber")
  
  for(i in seq_len(out$p + out$q + 1)){
    out$nums[i] <- out$nums[i] + x$nums[i] + y$nums[i]
    
    
    if ((out$nums[i]) > 9.5) {
      out$nums[i] <- carry_over(out$nums[i])
      out$nums[i + 1] <- out$nums[i+1] + 1
    }
    if (out$nums[i] < -9.5) {
      out$nums[i] <- carry_over(out$nums[i])
      out$nums[i + 1] <- out$nums[i+1] -1
    }
  }
  
  if(out$nums[out$p + out$q + 2] == 1){
    out$q <- out$q + 1
  }
  
  if(sum(out$nums) < 0){
    out$sign <- -1
    out$nums <- abs(out$nums)
  }
  
  out

}

####################################################################################

burrowing <- function(x1, y1){
  # A function that will be utilized in the subtract function
  #Args:
  #x1: pqnumber
  #x2: pqnumber
  #return:
  #abs(x1 + 10 - y1)
  abs(x1 + 10 - y1)
}

####################################################################################

subtract <- function(x,y){
  #A function used to subtract two pqnumbers
  #Args:
  #x: a pqnumber
  #y: a pqnumber
  #Return:
  #out: a pqnumber where y is subtracted from x
  
  if (x$p != y$p) {
    var1 <- max(x$p, y$p) - min(x$p, y$p)
    
    if (x$p < y$p){
      x$nums <- c(rep(0, var1), x$nums)
    }else if (x$p > y$p) {
      y$nums <- c(rep(0, var1), y$nums)
    }
    
  }
  
  if(x$q != y$q) {
    var2 <- max(x$q, y$q) - min(x$q, y$q)
    
    if (x$q < y$q) {
      x$nums <- c(x$nums, rep(0, var2))
    }else if (x$q > y$q) {
      y$nums <- c(y$nums, rep(0, var2))
    }
    
  }
  
  
  
  out <- structure((list("sign" = 1, "p" = max(x$p, y$p), "q" = max(x$q, y$q), "nums" = integer(max(x$p, y$p) + max(x$q, y$q) + 3))), class = "pqnumber")
  
  if ((x$sign == 1 & y$sign == -1) | (x$sign == -1 & y$sign ==1)) {
    if(x$sign == -1 & y$sign ==1){
      out$sign <- -1
    } 
    copyx = x; copyx$sign = 1
    copyy = y; copyy$sign = 1
    out$nums <- (add(copyy,copyx))$nums
  }
  
  
  for (i in seq_len(out$p + out$q + 1)) {
    
    if (((x$sign == 1) & (y$sign == 1)) | (x$sign == -1 & y$sign == -1)) {
      if (abs(as_numeric(x)) > abs(as_numeric(y))) {
        if((x$sign == -1 & y$sign == -1)){
          out$sign <- -1
        }
        
        if (x$nums[i] < y$nums[i]) {
          out$nums[i] <- out$nums[i] + burrowing(x$nums[i], y$nums[i])
          out$nums[i + 1] <- -1
          
        }else{
          out$nums[i] <- out$nums[i] + x$nums[i] - y$nums[i]
        }
        
        if(out$nums[i] == -1){
          out$nums[i] <- 9
          out$nums[i + 1] <- -1
        }
        
      } else if (abs(as_numeric(y)) > abs(as_numeric(x))) {
        
        if((x$sign == 1) & (y$sign == 1)){
          out$sign <- -1
        }
        
        if(y$nums[i] < x$nums[i]) {
          out$nums[i] <- out$nums[i] + burrowing(y$nums[i], x$nums[i])
          out$nums[i + 1] <- -1
        }else{
          out$nums[i] <- out$nums[i] + y$nums[i] - x$nums[i]
        }
        if(out$nums[i] == -1){
          out$nums[i] <- 9
          out$nums[i + 1] <- -1
        }
      }
    } 
  }

  out
  
}

####################################################################################

multiply <- function(x,y){
  #A function that multiplies two pqnumbers that utilizes the add() function to do so
  # Args:
  #x: a pqnumber
  #y: a pqnumber
  # Return:
  #out: a pqnumber that multiplied x and y
  
  if (x$p != y$p) {
    var1 <- max(x$p, y$p) - min(x$p, y$p)
    
    if (x$p < y$p){
      x$nums <- c(rep(0, var1), x$nums)
    }else if (x$p > y$p) {
      y$nums <- c(rep(0, var1), y$nums)
    }
    
  }
  
  if(x$q != y$q) {
    var2 <- max(x$q, y$q) - min(x$q, y$q)
    
    if (x$q < y$q) {
      x$nums <- c(x$nums, rep(0, var2))
    }else if (x$q > y$q) {
      y$nums <- c(y$nums, rep(0, var2))
    }
    
  }
  
  out <- structure((list("sign" = 1, "p" = x$p + y$p, "q" = x$q + y$q, "nums" = integer(x$p + y$p + x$q + y$q + 1))), class = "pqnumber")
  
  if (!(x$sign == y$sign)) {
    out$sign <- -1
  }
  
  for(i in seq_len(length(y$nums))){
    
    if(y$nums[i] == 1){
      out$nums[i:(i + length(x$nums) - 1)] <- x$nums
    }else if(y$nums[i] == 2){
      out$nums[i:(i + length(add(x, x)$nums) - 1)] <-out$nums[i:(i + length(add(x, x)$nums) -1)] + add(x, x)$nums
    }else if(y$nums[i] == 3){
      out$nums[i:(i + length(add(add(x, x),x)$nums) - 1)] <-out$nums[i:(i + length(add(add(x, x),x)$nums) -1)] + add(add(x, x),x)$nums
    }else if(y$nums[i] == 4){
      out$nums[i:(i + length(add(add(add(x, x),x),x)$nums) - 1)] <-out$nums[i:(i + length(add(add(add(x, x),x),x)$nums) -1)] + add(add(add(x, x),x),x)$nums
    }else if(y$nums[i] == 5){
      out$nums[i:(i + length(add(add(add(add(x, x),x),x),x)$nums) - 1)] <-out$nums[i:(i + length(add(add(add(add(x, x),x),x),x)$nums) -1)] + add(add(add(add(x, x),x),x),x)$nums
    }else if(y$nums[i] == 6){
      out$nums[i:(i + length(add(add(add(add(add(x, x),x),x),x),x)$nums) - 1)] <-out$nums[i:(i + length(add(add(add(add(add(x, x),x),x),x),x)$nums) -1)] + add(add(add(add(add(x, x),x),x),x),x)$nums
    }else if(y$nums[i] == 7){
      out$nums[i:(i + length(add(add(add(add(add(add(x, x),x),x),x),x),x)$nums) - 1)] <-out$nums[i:(i + length(add(add(add(add(add(add(x, x),x),x),x),x),x)$nums) -1)] + add(add(add(add(add(add(x, x),x),x),x),x),x)$nums
    }else if(y$nums[i] == 8){
      out$nums[i:(i + length(add(add(add(add(add(add(add(x, x),x),x),x),x),x),x)$nums) - 1)] <-out$nums[i:(i + length(add(add(add(add(add(add(add(x, x),x),x),x),x),x),x)$nums) -1)] + add(add(add(add(add(add(add(x, x),x),x),x),x),x),x)$nums
    }else if(y$nums[i] == 9){
      out$nums[i:(i + length(add(add(add(add(add(add(add(add(x, x),x),x),x),x),x),x),x)$nums) - 1)] <-out$nums[i:(i + length(add(add(add(add(add(add(add(add(x, x),x),x),x),x),x),x),x)$nums) -1)] + add(add(add(add(add(add(add(add(x, x),x),x),x),x),x),x),x)$nums
    }
    
  }
  
  for(i in seq_len(length(out$nums))){
    if(out$nums[i] > 9.5){
      
      out$nums[i+1] <- out$nums[i+1] + (out$nums[i] %/% 10)
      out$nums[i] <- out$nums[i] %% 10
      
    }
  }
  
  out
  
}

####################################################################################


