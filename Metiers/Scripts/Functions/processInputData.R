processInputData <- function(x, seq.def=c("Country","year","vessel_id",
                                          "vessel_length","trip_id","haul_id",
                                          "fishing_day","area","ices_rectangle",
                                          "gear","mesh","selection",
                                          "registered_target_assemblage")){
  print("Processing input data ...")
  # Calculate group totals for each sequence
  x[,":="(seq_group_KG = sum(KG, na.rm = T),
          seq_group_EUR = sum(EUR, na.rm = T)),
    by=c(seq.def,"species_group")]
  
  # Select a measure to determine the dominant group at a sequence level. If at least one species in a sequence has "value" in a measure column then 
  # all species in that sequence get the same measure.
  x[,":="(seq_measure = getMeasure(measure)),
    by=seq.def]
  
  
  # Determine the dominant group for each sequence
  x[seq_measure == "weight",":="(seq_dom_group = species_group[which.max(seq_group_KG)]),
    by=seq.def]
  x[seq_measure == "value",":="(seq_dom_group = species_group[which.max(seq_group_EUR)]),
    by=seq.def]
  
  return(x)
}