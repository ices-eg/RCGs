
getMetierLvl5FromPattern<-function(p.vessel_id, p.year, p.gear, p.gear_group, p.reg_target, p.dom_group){
  p.target <- ifelse(is.na(p.reg_target),p.dom_group,p.reg_target)
  
  metiers<-pattern[year==p.year & vessel_id == p.vessel_id & target_assemblage == p.target]
  
  result<-NA
  if(nrow(metiers)==1){
    result<-as.character(metiers[,metier_level_5])
  }
  else if(nrow(metiers)>1){
    result<-as.character(metiers[gear_group == p.gear_group, metier_level_5[which.max(seq_no_lvl5)]])
  }
  else{
    metiers<-pattern[year==p.year & vessel_id == p.vessel_id & gear_group == p.gear_group]
    result<-as.character(metiers[, metier_level_5[which.max(seq_no_lvl5)]])
  }

  if(length(result)>0){
    return(paste("pattern",result,sep="_"))
  }
  else{
    if(p.gear %in% gear.list$gear_code & p.target %in% assemblage.list){
      return(paste("rare",p.gear,p.target,sep="_"))
    }
    else{
      return(NA)
    }
  }
}