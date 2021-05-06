missingMetiersByLevel<-function(input.data, level, sequence.def){
  input.data.metiers.missing<-input.data[substr(metier_level_6,1,3)=="MIS"]
  input.data.metiers.missing[,":="(metier_level_6_backup=metier_level_6,
                                   metier_level_5_backup=metier_level_5)]
  n.rows.initial<-nrow(input.data)
  data.with.metiers<-input.data[substr(metier_level_6,1,3)!="MIS",
                                .(mis_met_number_of_seq=uniqueN(.SD)),
                                by=c(level,"metier_level_6","metier_level_5"),
                                .SDcols=sequence.def][,.SD[which.max(mis_met_number_of_seq)],
                                                      by=level,
                                                      .SDcols=c("metier_level_6",
                                                                "metier_level_5",
                                                                "mis_met_number_of_seq")][,mis_met_level:=paste(level,collapse = "|")]
  cols.to.remove<-c("metier_level_6","metier_level_5","mis_met_number_of_seq","mis_met_level")
  for(c in cols.to.remove){
    if(c %in% colnames(input.data.metiers.missing)){
      input.data.metiers.missing[,c(c):=NULL]
    }
  }
  input.data.metiers.missing<-merge(input.data.metiers.missing,
                                    data.with.metiers,all.x=T,by=level)
  input.data.metiers.missing[is.na(metier_level_6),
                             ":="(metier_level_6=metier_level_6_backup, metier_level_5=metier_level_5_backup)]
  input.data.metiers.missing[,c("metier_level_6_backup","metier_level_5_backup"):=NULL]

  input.data<-input.data[substr(metier_level_6,1,3)!="MIS"]
  input.data<-rbind(input.data, input.data.metiers.missing, fill=T)
  if(nrow(input.data)!=n.rows.initial){ 
    stop("Processed dataset has more rows than input dataset.
         Check your input data and search levels.")
  }
  return(input.data)
}
