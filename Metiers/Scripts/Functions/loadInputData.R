loadInputData <- function(fileName){
  print("Loading input data file ...")
  x <- data.table(read.csv(fileName,stringsAsFactors = F, na.strings = ""))
  return(x)
}