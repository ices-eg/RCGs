loadInputData <- function(fileName){
  message("Loading input data file ...")
  x <- data.table(read.csv(fileName,stringsAsFactors = F, na.strings = ""))
  return(x)
}