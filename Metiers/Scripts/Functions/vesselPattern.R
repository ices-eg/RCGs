# At this moment the function calculates the number of sequences and their percentage by year, vessel 
# and metier_level_5
vesselPattern <- function(x, seq.def){
  x[,metier_level_5:=substr(metier_level_6,1,7)]
  pattern <- unique(x[,.SD,.SDcols=c(seq.def,"metier_level_5")])
  pattern <- pattern[,.(seq_no_lvl5 = .N), by=.(year, vessel_id, metier_level_5)]
  pattern[,seq_perc_lvl5:=seq_no_lvl5/sum(seq_no_lvl5,na.rm = T)*100, by=.(year, vessel_id)]
  x <- merge(x, pattern, by=c("year", "vessel_id", "metier_level_5"))
  
  return(x)
}