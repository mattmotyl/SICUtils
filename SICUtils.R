#######################
#### SIC Lab Utils ####
#######################

#### Data Cleaning #####

  ## Retaining the rational zero

    ratzero <- function(x,y){
      # (x is the column you're trying to center. y is the rational zero.)
      initcen <- scale(x, center = TRUE, scale = TRUE) # Centering on Zero
      adj <- (y - mean(x, na.rm = T)) / sd(x, na.rm = T)
      initcen - adj # The value of a moderate person
    }  


#### Data Analysis ####
    
    # Cleaning data. You'll still need to make sure that they're coded in a way that makes sense after running this. 
      # (Coding varies, especially across different publicly available sources, so we can't automate making sure that the numbers make sense.)
    
numbify<-function(colsn){
  # "x" is the column (or columns) of the variables that you'd like to clean.
  # Problematic responses are coded as negative values; replace those as NA
  colsn.r <- lapply(colsn, function(x) replace(x, grep("[-]", x), NA))
  # Responses for respondents not yet eligible to complete certain questions are stored as "Not met"
  # Replace those as NA
  colsn.r <- lapply(colsn, function(x) replace(x, grep(c("Not met","Prefer not to say"), x), NA))
  colsn.d <- as.data.frame(lapply(colsn.r, function(x) factor(x)))
  colnum <- ncol(colsn)
  colsn.d[,1:colnum] <- sapply(colsn.d[,1:colnum], as.numeric)
  # Adding an indication that these are numeric. 
  colnames(colsn.d) <- paste(colnames(colsn.d), "n", sep = ".")
  colsn.d
}


#### Plotting ####

