# Part 2

complete <- function(directory, id = 1:332){
  
  fileId <- vector()
  numb <- vector() 
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
    df <- read.csv(paste(directory, "/", filename, sep=""))
    
    fileId <- append(fileId, i)
    #numb[i] <- sum(complete.cases(df))
    numb <- append(numb, sum(complete.cases(df)))
  }
  
  return (data.frame(id=fileId, nobs=numb))
}