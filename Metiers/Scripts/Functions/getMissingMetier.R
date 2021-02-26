getMissingMetier<-function(vessel_id,month,area,seq_dom_group,
                           quarter,year,vessel_length_group,
                           gear_FR){
  input.data.row <- data.table(vessel_id,month,area,seq_dom_group,
                               quarter,year,vessel_length_group,
                               gear_FR)
  search.levels <- list(c("vessel_id","month","area","seq_dom_group"),
                        c("vessel_id","quarter","area","seq_dom_group"),
                        c("vessel_id","year","area","seq_dom_group"),
                        c("vessel_length_group","gear_FR","month","area","seq_dom_group"),
                        c("gear_FR","month","area","seq_dom_group"))
  result<-NA
  for(l in search.levels){
    result<-merge(input.data.row, input.data.sequances, by=l)
    result<-result[,.(n=.N), by="metier_level_6"]
    result<-as.character(result[which.max(n),.(metier_level_6)][1])
    if(!is.na(result)){
      break
    }
  }
  
  if(is.na(result)){
    return("MIS_MIS_0_0_0*")
  }
  else{
    return(paste0(result,"*"))
  }
}
