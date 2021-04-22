vesselPatterns<-function(input.data,sequence.def,rare.threshold,gear.list){
  pattern <- input.data[,unique(.SD),.SDcols=c(sequence.def,"metier_level_5")]
  pattern<-pattern[metier_level_5!="MIS_MIS"]
  pattern <- pattern[,.(seq_no_lvl5 = .N), by=.(year, vessel_id, metier_level_5)]
  pattern[,seq_perc_lvl5:=seq_no_lvl5/sum(seq_no_lvl5,na.rm = T)*100, by=.(year, vessel_id)]
  input.data <- merge(input.data, pattern,all.x = T ,
                      by=c("year", "vessel_id", "metier_level_5"))
  input.data[,metier_level_5_status:=ifelse(seq_perc_lvl5<rare.threshold,"rare","common")]
  input.data.rare<-input.data[seq_perc_lvl5<rare.threshold]
  input.data.no.metier<-input.data[metier_level_5=="MIS_MIS" & is.na(seq_perc_lvl5)]
  input.data<-input.data[seq_perc_lvl5>=rare.threshold]
  pattern<-pattern[seq_perc_lvl5>=rare.threshold]
  pattern[,c("gear","target_assemblage"):=data.table(str_split_fixed(metier_level_5,"_",2))]
  pattern<-merge(pattern, gear.list[,.(gear_code,gear_group)], 
                 all.x = T, by.x = "gear", by.y = "gear_code")
  
  pattern[,count_year_vessel_target:=.N,by=.(year, vessel_id, target_assemblage)]
  pattern[,rank_year_vessel_target_geargr:=frank(-seq_no_lvl5, ties.method = "first"),
          by=.(year, vessel_id, target_assemblage, gear_group)]
  pattern[,rank_year_vessel_target:=frank(-seq_no_lvl5, ties.method = "first"),
          by=.(year, vessel_id, target_assemblage)]
  pattern[,rank_year_vessel_geargr:=frank(-seq_no_lvl5, ties.method = "first"),
          by=.(year, vessel_id, gear_group)]
  setnames(pattern, old="metier_level_5", new="metier_level_5_pattern")
  
  input.data.rare.merged<-data.table()
  if(nrow(input.data.rare)>0){
    input.data.rare<-merge(input.data.rare, pattern[count_year_vessel_target==1,
                                                    .(year,vessel_id,target_assemblage,
                                                      metier_level_5_pattern)],
                      by.x=c("year","vessel_id","seq_dom_group"),
                      by.y=c("year","vessel_id","target_assemblage"),all.x=T)
    input.data.rare.merged<-rbind(input.data.rare.merged,
                                  input.data.rare[!is.na(metier_level_5_pattern)],fill=T)
    input.data.rare<-input.data.rare[is.na(metier_level_5_pattern)]
  }
  if("metier_level_5_pattern" %in% colnames(input.data.rare)) input.data.rare[,metier_level_5_pattern:=NULL]
  
  if(nrow(input.data.rare)>0){
    input.data.rare<-merge(input.data.rare, 
                           pattern[count_year_vessel_target>1 & rank_year_vessel_target_geargr==1,
                                   .(year,vessel_id,target_assemblage,gear_group,metier_level_5_pattern)],
                           by.x=c("year","vessel_id","seq_dom_group","gear_group"),
                           by.y=c("year","vessel_id","target_assemblage","gear_group"),all.x=T)
    input.data.rare.merged<-rbind(input.data.rare.merged,
                                  input.data.rare[!is.na(metier_level_5_pattern)],fill=T)
    input.data.rare<-input.data.rare[is.na(metier_level_5_pattern)]
  }
  if("metier_level_5_pattern" %in% colnames(input.data.rare)) input.data.rare[,metier_level_5_pattern:=NULL]
  
  if(nrow(input.data.rare)>0){
    input.data.rare<-merge(input.data.rare, 
                           pattern[count_year_vessel_target>1 & rank_year_vessel_target==1,
                                   .(year,vessel_id,target_assemblage,metier_level_5_pattern)],
                           by.x=c("year","vessel_id","seq_dom_group"),
                           by.y=c("year","vessel_id","target_assemblage"),all.x=T)
    input.data.rare.merged<-rbind(input.data.rare.merged,
                                  input.data.rare[!is.na(metier_level_5_pattern)],fill=T)
    input.data.rare<-input.data.rare[is.na(metier_level_5_pattern)]
  }
  if("metier_level_5_pattern" %in% colnames(input.data.rare)) input.data.rare[,metier_level_5_pattern:=NULL]
  
  if(nrow(input.data.rare)>0){
    input.data.rare<-merge(input.data.rare, 
                           pattern[rank_year_vessel_geargr==1,
                                   .(year,vessel_id,gear_group,metier_level_5_pattern)],
                           by.x=c("year","vessel_id","gear_group"),
                           by.y=c("year","vessel_id","gear_group"),all.x=T)
    input.data.rare.merged<-rbind(input.data.rare.merged,
                                  input.data.rare[!is.na(metier_level_5_pattern)],fill=T)
    input.data.rare<-input.data.rare[is.na(metier_level_5_pattern)]
  }
  if("metier_level_5_pattern" %in% colnames(input.data.rare)) input.data.rare[,metier_level_5_pattern:=NULL]
  
  return(rbind(input.data,input.data.no.metier,input.data.rare,input.data.rare.merged,fill=T))
}
