loadInputData <- function(fileName){
  message("Loading input data file ...")
  x <- fread(fileName, stringsAsFactors = F, na.strings = "")
  return(x)
}