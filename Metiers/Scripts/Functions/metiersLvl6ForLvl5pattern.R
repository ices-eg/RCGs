metiersLvl6ForLvl5pattern <-function(input.data,level,sequence.def){
  if(!"metier_level_6_pattern" %in% colnames(input.data)) input.data[,metier_level_6_pattern:=NA]
  input.data.to.process<-input.data[metier_level_5_status=="rare" & 
                                      !is.na(metier_level_5_pattern) &
                                      is.na(metier_level_6_pattern)]
  input.data.processed<-input.data[metier_level_5_status=="rare" & !is.na(metier_level_6_pattern)]
  input.data.not.considered<-input.data[metier_level_5=="MIS_MIS" | (metier_level_5_status=="rare" & 
                                                                       is.na(metier_level_5_pattern))]
  input.data<-input.data[metier_level_5_status=="common"]
  data.with.metiers<-input.data[,.(ves_pat_met6_number_of_seq=uniqueN(.SD)),
                                by=c(level,"metier_level_6"),
                                .SDcols=sequence.def][,.SD[which.max(ves_pat_met6_number_of_seq)],
                                                      by=level,
                                                      .SDcols=c("metier_level_6",
                                                                "ves_pat_met6_number_of_seq")][,ves_pat_met6_level:=paste(level,collapse = "|")]
  setnames(data.with.metiers,old="metier_level_6",new="metier_level_6_pattern")
  cols.to.remove<-c("metier_level_6_pattern","ves_pat_met6_number_of_seq","ves_pat_met6_level")
  for(c in cols.to.remove){
    if(c %in% colnames(input.data.to.process)){
      input.data.to.process[,c(c):=NULL]
    }
  }
  level.to.process<-replace(level,level=="metier_level_5","metier_level_5_pattern")
  input.data.to.process<-merge(input.data.to.process,data.with.metiers,all.x=T,
                               by.x=level.to.process, by.y = level)
  return(rbind(input.data, input.data.to.process,input.data.processed, 
               input.data.not.considered, fill=T))
}
