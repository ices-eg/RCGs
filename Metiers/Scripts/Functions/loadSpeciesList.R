loadSpeciesList <- function(url){
  message("Loading species list ...")
  x <- data.table(read.xlsx(url, sheet = "Species Reference List", startRow = 2, check.names = T))
  x <- unique(x[,.(FAOcode, Grouping.2.1, Grouping.3.DWS.Reg..DWS.1)])
  setnames(x, old = c("FAOcode","Grouping.2.1","Grouping.3.DWS.Reg..DWS.1"), new = c("FAO_species", "species_group","dws_group"))
  if(any(duplicated(x$FAO_species))){
    x<-x[!duplicated(x$FAO_species)]
    print("Warning! Duplicated species codes were found and were removed.")
  }
  return(x[,.(FAO_species,species_group,dws_group)])
}