validateInputDataCodes <- function(input.data, gear.list, area.list, species.list){
  message("Validation of input data codes...")
  assemblage.list <- unique(c(species.list$species_group, species.list$dws_group))
  assemblage.list <- assemblage.list[!is.na(assemblage.list)]
  assemblage.list <- c(assemblage.list,c("MCD","FIF"))
  
  invalid.area<-setdiff(unique(input.data$area), area.list$area)
  invalid.gear<-setdiff(unique(input.data$gear), gear.list$gear_code)
  gearFR<-unique(input.data$gear_FR)
  gearFR<-gearFR[!is.na(gearFR)]
  invalid.gearFR<-setdiff(gearFR, gear.list$gear_code)
  selection<-unique(input.data$selection)
  selection<-selection[!is.na(selection)]
  invalid.selection<-selection[grep("[01234]_\\d{1,3}",selection,invert=TRUE)]
  reg.tar.assemblage<-unique(input.data$registered_target_assemblage)
  reg.tar.assemblage<-reg.tar.assemblage[!is.na(reg.tar.assemblage)]
  invalid.reg.target.assemblage<-setdiff(reg.tar.assemblage,assemblage.list)
  invalid.species<-setdiff(unique(input.data$FAO_species), species.list$FAO_species)
  invalid.measure<-setdiff(unique(input.data$measure),c("weight","value"))
  if(length(invalid.area)>0) 
    warning(paste0("Invalid area codes found: ",invalid.area))
  if(length(invalid.gear)>0) 
    warning(paste0("Invalid gear codes found: ",invalid.gear))
  if(length(invalid.gearFR)>0) 
    warning(paste0("Invalid gear_FR codes found: ",invalid.gearFR))
  if(length(invalid.selection)>0) 
    warning(paste0("Invalid selection device coding found: ",invalid.selection))
  if(length(invalid.reg.target.assemblage)>0) 
    warning(paste0("Invalid reg target assemblage codes found: ",invalid.reg.target.assemblage))
  if(length(invalid.species)>0) 
    warning(paste0("Invalid species codes found: ",invalid.species))
  if(length(invalid.measure)>0) 
    warning(paste0("Invalid measure codes found: ",invalid.measure))
  
  return(TRUE)
}