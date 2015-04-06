complete <- function(directory, id = 1:332) {
  nobs <- c()
  for(i in id){
    ccases <- complete.cases(read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), ".csv", sep = "")))
    nobs <- c(nobs, sum(ccases))
  }
  return(data.frame(id, nobs)) 
}