loadSpeciesList <- function(url){
  print("Loading species list ...")
  x <- data.table(read.xlsx(url, sheet = "Species Reference List"))
  x <- unique(x[,.(FAOcode, Grouping.2)])
  setnames(x, old = c("FAOcode","Grouping.2"), new = c("FAO_species", "species_group"))
  return(x)
}