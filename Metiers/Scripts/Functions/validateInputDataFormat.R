validateInputDataFormat <- function(input.data){
  message("Validation of input data format...")
  valid.colnames<-c("Country","year","vessel_id","vessel_length","trip_id",
                    "haul_id","fishing_day","area","ices_rectangle","gear",
                    "gear_FR","mesh","selection","registered_target_assemblage",
                    "FAO_species","metier_level_6","measure","KG","EUR")
  valid.coltypes<-c("character","integer","character","double","character",
                    "character","character","character","character","character",
                    "character","integer","character","character",
                    "character","character","character","double","double")
  validation<-data.table(column_name=valid.colnames,column_type=valid.coltypes)
  for(i in 1:nrow(validation)){
    column.name<-validation[i,]$column_name
    if(column.name %in% colnames(input.data)){
      if(is.factor(input.data[,unlist(.SD),.SDcols=column.name]))
        stop(paste0("Column ",column.name," is a factor."))
      column.type<-validation[i,]$column_type
      if(column.type==typeof(input.data[,unlist(.SD),.SDcols=column.name]))
        print(paste0("Column ",column.name,": validation passed."))
      else 
        warning(paste0("Column ",column.name," should be of type ",column.type,"."))
    }
    else stop(paste0("The ",validation[i,]$column_name," column is missing."))
  }
  if(ncol(input.data) > nrow(validation)) 
    warning(paste0("Input data has additional columns that are not recognized by the script."))
  return(TRUE)
}


