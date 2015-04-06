pollutantmean <- function(directory, pollutant, id = 1:332) {
  files <- list.files(directory)
  filenames <- as.numeric(sub("\\.csv$","",files))
  temp <- c();
  for(i in id) {
    file = files[match(i,filenames)]
    curfile <- read.csv(paste(directory,"/", file, sep=""))
    temp <- c(temp, curfile[,pollutant])
  }
  result <- mean(temp, na.rm = TRUE)
  return(round(result, 3)) 
}