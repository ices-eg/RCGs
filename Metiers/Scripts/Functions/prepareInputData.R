prepareInputData <- function(x){
  print("Preparing input data ...")
  x[,EUR:=as.numeric(EUR)]
  x[,KG:=as.numeric(KG)]
  x[,c("selection_type","selection_mesh"):=data.table(str_split_fixed(selection,"_",2))]
  x[,selection_type:=ifelse(selection_type=="",NA,selection_type)]
  x[,selection_mesh:=ifelse(selection_mesh=="",NA,selection_mesh)]
  
  
  if(!"selection_mesh" %in% colnames(x)){
    x[,selection_mesh:=NA]
  }
  x <- merge(x, area.list, all.x = T, by = "area")
  x[is.na(RCG) & substr(area,1,2) %in% c("31","34","41","47","51","57","58","87"),RCG:="LD"]
  x[is.na(RCG) & substr(area,1,2) == "37",RCG:="MED"]
  
  # Assign species category to the input data
  x <- merge(x, species.list, all.x = T, by = "FAO_species")
  return(x)
}
