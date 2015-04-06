## PART 1
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

#pollutantmean("specdata", "sulfate", 1:10) == 4.064
#pollutantmean("specdata", "nitrate", 70:72) == 1.706
#pollutantmean("specdata", "nitrate", 23) == 1.281

# PART 2
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases
complete <- function(directory, id = 1:332) {
  nobs <- c()
  for(i in id){
    ccases <- complete.cases(read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), ".csv", sep = "")))
    nobs <- c(nobs, sum(ccases))
  }
  return(data.frame(id, nobs)) 
}
#complete("specdata", c(2, 4, 8, 10, 12))
#complete("specdata", 30:25)
#complete("specdata", 3)

## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations

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
cr <- corr("specdata", 150)
head(cr)
summary(cr)



