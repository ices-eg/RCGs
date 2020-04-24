loadSpeciesList <- function(url){
  print("Loading species list ...")
  x <- data.table(read.xlsx(url, sheet = "Species Reference List"))
  x <- unique(x[,.(FAOcode, Grouping.2, `Grouping.3.DWS.Reg./DWS`)])
  setnames(x, old = c("FAOcode","Grouping.2","Grouping.3.DWS.Reg./DWS"), new = c("FAO_species", "species_group","dws_group"))
  return(x[,.(FAO_species,species_group)])
}