detailedMetiersLvl6ForLvl5 <-function(input.data,level,sequence.def){
  input.data.to.process<-input.data[is.na(detailed_metier_level_6)]
  input.data.processed<-input.data[!is.na(detailed_metier_level_6)]

  data.with.metiers<-input.data.processed[,.(det_met6_number_of_seq=uniqueN(.SD)),
                                by=c(level,"detailed_metier_level_6"),
                                .SDcols=sequence.def][,.SD[which.max(det_met6_number_of_seq)],
                                                      by=level,
                                                      .SDcols=c("detailed_metier_level_6",
                                                                "det_met6_number_of_seq")][,det_met6_level:=paste(level,collapse = "|")]
  cols.to.remove<-c("detailed_metier_level_6","det_met6_number_of_seq","det_met6_level")
  for(c in cols.to.remove){
    if(c %in% colnames(input.data.to.process)){
      input.data.to.process[,c(c):=NULL]
    }
  }
  input.data.to.process<-merge(input.data.to.process,data.with.metiers,all.x=T,by=level)
  return(rbind(input.data.to.process,input.data.processed,fill=T))
}
