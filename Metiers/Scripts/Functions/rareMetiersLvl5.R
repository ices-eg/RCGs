rareMetiersLvl5 <- function(input.data,sequence.def,rare.threshold){
  pattern <- input.data[,unique(.SD),.SDcols=c(sequence.def,"metier_level_5")]
  pattern<-pattern[metier_level_5!="MIS_MIS"]
  pattern <- pattern[,.(seq_no_lvl5 = .N), by=.(year, vessel_id, metier_level_5)]
  pattern[,seq_perc_lvl5:=seq_no_lvl5/sum(seq_no_lvl5,na.rm = T)*100, by=.(year, vessel_id)]
  input.data <- merge(input.data, pattern,all.x = T ,
                      by=c("year", "vessel_id", "metier_level_5"))
  input.data[,metier_level_5_status:=ifelse(seq_perc_lvl5<rare.threshold,"rare","common")]
  input.data[,":="(seq_no_lvl5=NULL, seq_perc_lvl5=NULL, 
                   metier_level_5_pattern=NA, ves_pat_number_of_seq=NA, ves_pat_level=NA)]
  return(input.data)
}
