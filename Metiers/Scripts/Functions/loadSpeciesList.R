loadSpeciesList <- function(url){
  message("Loading species list ...")
  x <- data.table(read.xlsx(url, sheet = "Species Reference List", startRow = 1, check.names = T))
  x <- unique(x[,.(FAOcode, Grouping.2..TO.BE.USED., Grouping.3.DWS.Reg..DWS)])
  setnames(x, old = c("FAOcode","Grouping.2..TO.BE.USED.","Grouping.3.DWS.Reg..DWS"), new = c("FAO_species", "species_group","dws_group"))
  if(any(duplicated(x$FAO_species))){
    x<-x[!duplicated(x$FAO_species)]
    print("Warning! Duplicated species codes were found and were removed.")
  }
  return(x[,.(FAO_species,species_group,dws_group)])
}