loadSpeciesList <- function(url){
  print("Loading species list ...")
  x <- data.table(read.xlsx(url, sheet = "Species Reference List", startRow = 2, check.names = T))
  x <- unique(x[,.(FAOcode, Grouping.2.1, Grouping.3.DWS.Reg..DWS.1)])
  setnames(x, old = c("FAOcode","Grouping.2.1","Grouping.3.DWS.Reg..DWS.1"), new = c("FAO_species", "species_group","dws_group"))
  return(x[,.(FAO_species,species_group,dws_group)])
}