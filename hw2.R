## (a)
n <- 100
p <- 10 + 1
set.seed(205615894)
gradebook <- data.frame(rep(0, n))
gradebook[, 1] <- 205615894:(205615894+n-1)
for (i in 2:p){
  gradebook[, i] <- sample(0:100, n, replace = TRUE)
}
colnames(gradebook) <- c("UID", "hw1", "hw2", "hw3", "hw4", "hw5",
                         "quiz1", "quiz2", "quiz3", "quiz4", "quiz5")
gradebook

#############################################

## (c)

messy_impute <- function(df, center, margin, ...){
  # Function description
  # Args:
  # df: This is the data frame input that contains the gradebook as shown in the example. 
  # center: A character object that specifies what the imputed value will be defined by. This will be a character object and should either be "mean" or "median"
  # margin: The margin argument should be inputted as an integer, either the value 1 or 2 which corresponds to 1 as the row/student and 2 as the column/assignment in which the imputed value will be calculated by. 
  # ...: This is the optional argument which may or may not be used such as "trim"
  # Return:
  # The imputed data frame
  out <- df
  
    for(i in seq_len(dim(df)[1])){
      for(j in seq_len(dim(df)[2])){
        
        if(is.na(df[i,j])){
          if(margin == 1){
            
            if(center == "mean"){
              out[i,j] <- mean(as.numeric(df[i,-1]), na.rm = TRUE)
            }else if(center == "median"){
              out[i,j] <- median(as.numeric(df[i,-1]), na.rm = TRUE)
            }
          }else if(margin == 2){
            if(center == "mean"){
              out[i,j] <- mean(df[ ,j], na.rm = TRUE)
            }else if(center == "median"){
              out[i,j] <- median(df[ ,j], na.rm = TRUE)
            }
          }
        }
      }
    }

  
  out
}


################


## (f)

tidy_impute <- function (df, center, margin, ...){
  # Function description
  # Args:
  # df: This is the data frame input that contains the gradebook_tidy as shown in the example. 
  # center: A character object that specifies what the imputed value will be defined by. This will be a character object and should either be "mean" or "median"
  # margin: The margin argument should be inputted as an integer, either the value 1 or 2 which corresponds to 1 to be grouped with UID and 2 to be grouped by the specific assignment/quiz in which the imputed value will be calculated by. 
  # ...: This is the optional argument which may or may not be used
  # Return:
  # The imputed data frame for gradebook_tidy
  
  for (i in seq_len((dim(df))[1])) {
    if (is.na(df[i, 3])){
      if (margin == 1) {
        
        if (center == "mean") {
          df[i, 3] <- as.integer((df %>% group_by(UID) %>% summarise(center = mean(Grade, na.rm = T)))[i %/% 10, 2]) # the data frame on the right side of this line has a shrunken dimension, dimension of this df is 100 by 2
        }else if (center == "median") {
          df[i, 3] <- as.integer((df %>% group_by(UID) %>% summarise(center = median(Grade, na.rm = T)))[i %/% 10, 2]) # the data frame on the right side of this line has a shrunken dimension, dimension of this df is 100 by 2
        }
        
      }else if (margin == 2) {
        
        if (center == "mean") {
          df[i, 3] <- as.integer((df %>% group_by(Item) %>% summarise(center = mean(Grade, na.rm = T)))[i %/% 100, 2]) # the data frame on the right side of this line has a shrunken dimension, dimension of this df is 10 by 2
        }else if (center == "median") {
          df[i, 3] <- as.integer((df %>% group_by(Item) %>% summarise(center = median(Grade, na.rm = T)))[i %/% 100, 2]) # the data frame on the right side of this line has a shrunken dimension, dimension of this df is 10 by 2
        }
        
      }
    } 
  }
  
  df
  
}


