corr <- function(directory, threshold = 0) {
  files <- complete(directory)
  ids <- files[files["nobs"] > threshold, ]$id
  tempcor <- c()
  for(i in ids){
    curfile <- read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), ".csv", sep = ""))
    corr_ <- curfile[complete.cases(curfile), ]
    tempcor <- c(tempcor, cor(corr_$sulfate, corr_$nitrate))
  }
  return(tempcor)
}