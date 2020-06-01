# Part 1 of Wk2

pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of lenght 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the 
  ## mean; either 'sulfate' or 'nitrate'.
  
  ## 'id' is an integer vector indicating teh monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round teh results!
  
  
  # loop over ids
  #   format file name
  #   read file from directory : read.csv()
  #   calcuate 
  
  holder <- vector()
  for (i in id) {
    #print(i)
    # format file name to handle variable "zeros" in name
    if (i < 10) {
        file <- paste0("00", i)
    } else if (i < 100) {
        file <- paste0("0", i)
    } else {      
        file <- toString(i)
    }
    filename <- paste(file, ".csv", sep="")
    print(filename)
    # works!!
    
    #open file and drop NA's
    df <- read.csv(paste(directory, "/", filename, sep=""))
    t <- is.na(df[pollutant])
    
    #calc mean and store
    holder[i] <- mean(df[pollutant][!t], na.rm=TRUE)
    
    #print(holder[i])
  }
  #print(holder)
  return (mean(holder, na.rm=TRUE))
}