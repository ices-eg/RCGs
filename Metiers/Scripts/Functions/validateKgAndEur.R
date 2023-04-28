validateKgAndEur <- function(input.data){
  input.data$KG_num <-suppressWarnings(as.numeric(input.data$KG))
  input.data$EUR_num <- suppressWarnings(as.numeric(input.data$EUR))
  err.msg <- ""
  temp<-input.data[(is.na(KG_num) & !is.na(KG))]$KG
  if(length(temp)>0){
    err.msg <- paste0(err.msg,"There are values in the KG column that are not in numeric format! ",
                     "Few examples: ",paste(head(temp),collapse = ", "),"\n")
  }
  temp<-input.data[(is.na(EUR_num) & !is.na(EUR))]$EUR
  if(length(temp)>0){
    err.msg <- paste(err.msg,"There are values in the EUR column that are not in numeric format!",
                     "Few examples: ",paste(head(temp),collapse = ", "),"\n")
  }
  if(nchar(err.msg)>0) stop(err.msg)
  return(TRUE)
}
