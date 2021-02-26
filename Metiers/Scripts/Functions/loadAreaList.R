loadAreaList <- function(url){
  print("Loading area list ...")
  x <- data.table(read.csv(url, sep = ",", stringsAsFactors = F))
  setnames(x, old = c("Code","AreaCode"), new = c("RCG","area"))
  x <- x[,map(.SD,trimws)]
  # area 21.0.A is present twice for NA and NSEA. In the next step we merge by area so we must get rid of duplicated areas. Question is how to handle 21.0.A.
  if(any(duplicated(x$area))){
    x<-x[!duplicated(x$area)]
    print("Warning! Duplicated area codes were found and were removed.")
  }
  return(x)
}