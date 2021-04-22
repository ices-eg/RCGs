vesselPatternsByLevel <- function(input.data,level,sequence.def){
  input.data.rare.to.process<-input.data[metier_level_5_status=="rare" & is.na(metier_level_5_pattern)]
  input.data.rare.processed<-input.data[metier_level_5_status=="rare" & !is.na(metier_level_5_pattern)]
  input.data.no.metier<-input.data[metier_level_5=="MIS_MIS"]
  input.data<-input.data[metier_level_5_status=="common"]
  data.with.metiers<-input.data[,.(ves_pat_number_of_seq=uniqueN(.SD)),
                                by=c(level,"metier_level_5"),
                                .SDcols=sequence.def][,.SD[which.max(ves_pat_number_of_seq)],
                                                      by=level,
                                                      .SDcols=c("metier_level_5",
                                                                "ves_pat_number_of_seq")][,ves_pat_level:=paste(level,collapse = "|")]
  setnames(data.with.metiers,old="metier_level_5",new="metier_level_5_pattern")
  cols.to.remove<-c("metier_level_5_pattern","ves_pat_number_of_seq","ves_pat_level")
  for(c in cols.to.remove){
    if(c %in% colnames(input.data.rare.to.process)){
      input.data.rare.to.process[,c(c):=NULL]
    }
  }
  input.data.rare.to.process<-merge(input.data.rare.to.process,data.with.metiers,all.x=T,by=level)
  return(rbind(input.data, input.data.rare.to.process,input.data.rare.processed, 
               input.data.no.metier, fill=T))
}
