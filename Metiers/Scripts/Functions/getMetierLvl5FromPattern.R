
getMetierLvl5FromPattern<-function(p.vessel_id, p.year, p.gear, p.gear_group, p.reg_target, p.dom_group){
  p.target <- ifelse(is.na(p.reg_target),p.dom_group,p.reg_target)
  
  # Search for metier lvl5 in the pattern having the same year, vessel id and target assemblage.
  metiers<-pattern[year==p.year & vessel_id == p.vessel_id & target_assemblage == p.target]
  result<-NA
  # if exactly one metier lvl5 was found then save it in the result for the sequence. 
  if(nrow(metiers)==1){
    result<-as.character(metiers[,metier_level_5])
  }
  # if more than one metier lvl5 were found then take the one with the highest 
  # number of sequences in the same gear group.
  else if(nrow(metiers)>1){
    result<-as.character(metiers[gear_group == p.gear_group, metier_level_5[which.max(seq_no_lvl5)]])
  }
  # if none of the above returned any metier lvl5 then search for a metier lvl5 
  # in the pattern having the same year, vessel id and gear group. If more than one
  # were found, take the one with the highest number of sequences
  else{
    metiers<-pattern[year==p.year & vessel_id == p.vessel_id & gear_group == p.gear_group]
    result<-as.character(metiers[, metier_level_5[which.max(seq_no_lvl5)]])
  }
  # if metier lvl 5 was found in the pattern then return it with a prefix "pattern"
  if(length(result)>0){
    return(paste("pattern",result,sep="_"))
  }
  else{
    # if metier lvl5 was not found in the pattern but gear code and target assemblage
    # have valid values then return them with a prefix "rare"
    if(p.gear %in% gear.list$gear_code & p.target %in% assemblage.list){
      return(paste("rare",p.gear,p.target,sep="_"))
    }
    # if metier lvl5 was not found in the pattern and gear code and target assemblage
    # do not have valid values then return NA
    else{
      return(as.character(NA))
    }
  }
}