library(stringr)
library(data.table)
library(openxlsx)
library(purrr)
library(lubridate)

rm(list=ls())
gc()

# Import all functions
for(f in list.files(path="./Scripts/Functions", full.names = T)){
  source(f)
}
rm(f)

# Load the input data
data.file <- "data_input_example.csv"
input.data <- loadInputData(data.file)
rm(data.file)

# Validate input data format
validateInputDataFormat(input.data)

# Load reference lists
url <- "https://github.com/ices-eg/RCGs/raw/master/Metiers/Reference_lists/AreaRegionLookup.csv"
area.list <- loadAreaList(url)
url <- "https://github.com/ices-eg/RCGs/raw/master/Metiers/Reference_lists/Metier%20Subgroup%20Species%202020.xlsx"
species.list <- loadSpeciesList(url)
url <- "https://github.com/ices-eg/RCGs/raw/master/Metiers/Reference_lists/RDB_ISSG_Metier_list.csv"
metier.list <- loadMetierList(url)
url <- "https://github.com/ices-eg/RCGs/raw/master/Metiers/Reference_lists/Code-ERSGearType-v1.1.xlsx"
gear.list <- loadGearList(url)
rm(url)

# Validate input data codes
validateInputDataCodes(input.data, gear.list, area.list, species.list)

# Prepare input data
input.data[,EUR:=as.numeric(EUR)]
input.data[,KG:=as.numeric(KG)]
input.data[,c("selection_type","selection_mesh"):=data.table(str_split_fixed(selection,"_",2))]
input.data[,selection_type:=ifelse(selection_type=="",NA,selection_type)]
input.data[,selection_mesh:=ifelse(selection_mesh=="",NA,selection_mesh)]

# Assign RCG names to the input data
input.data <- merge(input.data, area.list, all.x = T, by = "area")

# Assign species category to the input data
input.data <- merge(input.data, species.list, all.x = T, by = "FAO_species")

# Assign gear group and re-coded gear name to the input data
input.data<-merge(input.data, gear.list, all.x = T, by.x = "gear", by.y = "gear_code")

# Process input data
#In the variable called sequence.def please include all columns that will constitute a fishing sequence
#This variable will be used as a key for grouping operations
sequence.def <- c("Country","year","vessel_id","vessel_length","trip_id","haul_id",
                  "fishing_day","area","ices_rectangle","gear_level6","mesh","selection",
                  "registered_target_assemblage")
# Calculate group totals for each sequence
input.data[,":="(seq_group_KG = sum(KG, na.rm = T),
        seq_group_EUR = sum(EUR, na.rm = T)),
  by=c(sequence.def,"species_group")]

# Select a measure to determine the dominant group at a sequence level. If at least one species in a sequence has "value" in a measure column then 
# all species in that sequence get the same measure.
input.data[,":="(seq_measure = getMeasure(measure)),
  by=sequence.def]

# Determine the dominant group for each sequence
input.data[seq_measure == "weight",":="(seq_dom_group = species_group[which.max(seq_group_KG)]),
  by=sequence.def]
input.data[seq_measure == "value",":="(seq_dom_group = species_group[which.max(seq_group_EUR)]),
  by=sequence.def]
input.data[,":="(seq_group_KG=NULL,seq_group_EUR=NULL,seq_measure=NULL)]
# Apply DWS rules
input.data[dws_group=="DWS",seq_DWS_kg:=sum(KG, na.rm = T),
           by=c(sequence.def, "dws_group")]
input.data[,seq_total_kg:=sum(KG, na.rm = T),
           by=sequence.def]
input.data[,seq_DWS_perc:=ifelse(is.na(seq_DWS_kg),0,seq_DWS_kg/seq_total_kg)*100]
input.data[,seq_DWS_perc:=max(seq_DWS_perc),by=sequence.def]
input.data[,DWS_gear_applicable:=grepl(RCG,DWS_for_RCG),by=.(RCG)]
input.data[seq_DWS_perc>8 & DWS_gear_applicable,seq_dom_group:="DWS"]
input.data[,":="(dws_group=NULL,DWS_for_RCG=NULL,seq_DWS_kg=NULL,seq_total_kg=NULL,seq_DWS_perc=NULL,
                 DWS_gear_applicable=NULL)]
# Assign metier level 6
input.data$metier_level_6<-NA
input.data$metier_level_5<-NA
input.data[,c("metier_level_6","metier_level_5"):=pmap_dfr(list(RCG,
                                          year,
                                          gear_level6, 
                                          registered_target_assemblage,
                                          seq_dom_group, 
                                          mesh, 
                                          selection_type,
                                          selection_mesh), getMetier)]
# Missing metier. Step 1: Search levels based on a dominant group of species
input.data[,":="(month=month(dmy(fishing_day)),
                 quarter=quarter(dmy(fishing_day)))]
step.levels<-list(c("vessel_id","month","area","seq_dom_group","gear_group"),
                  c("vessel_id","month","area","seq_dom_group"),
                  c("vessel_id","quarter","area","seq_dom_group","gear_group"),
                  c("vessel_id","quarter","area","seq_dom_group"),
                  c("vessel_id","year","area","seq_dom_group","gear_group"),
                  c("vessel_id","year","area","seq_dom_group"),
                  c("vessel_id","month","seq_dom_group","gear_group"),
                  c("vessel_id","month","seq_dom_group"),
                  c("vessel_id","quarter","seq_dom_group","gear_group"),
                  c("vessel_id","quarter","seq_dom_group"),
                  c("vessel_id","year","seq_dom_group","gear_group"),
                  c("vessel_id","year","seq_dom_group"))
for(level in step.levels){
  if(nrow(input.data[substr(metier_level_6,1,3)=="MIS"])>0){
    input.data <- missingMetiersByLevel(input.data,level,sequence.def)
  } else {break}
}
# Missing metier. Step 2: Search levels based on gear/gear group
step.levels<-list(c("vessel_id","month","area","gear_level6"),
                  c("vessel_id","quarter","area","gear_level6"),
                  c("vessel_id","year","area","gear_level6"),
                  c("vessel_id","month","gear_level6"),
                  c("vessel_id","quarter","gear_level6"),
                  c("vessel_id","year","gear_level6"),
                  c("vessel_id","month","area","gear_group"),
                  c("vessel_id","quarter","area","gear_group"),
                  c("vessel_id","year","area","gear_group"),
                  c("vessel_id","month","gear_group"),
                  c("vessel_id","quarter","gear_group"),
                  c("vessel_id","year","gear_group"))
for(level in step.levels){
  if(nrow(input.data[substr(metier_level_6,1,3)=="MIS"])>0){
    input.data <- missingMetiersByLevel(input.data,level,sequence.def)
  } else {break}
}
# Missing metier. Step 3: Search levels based on fleet register gear, vessel length group
# and species group
input.data[,vessel_length_group:=cut(vessel_length,breaks=c(0,10,12,18,24,40,Inf),right=F)]
step.levels<-list(c("month","vessel_length_group","gear_FR","area","seq_dom_group"),
                  c("month","gear_FR","area","seq_dom_group"),
                  c("quarter","vessel_length_group","gear_FR","area","seq_dom_group"),
                  c("quarter","gear_FR","area","seq_dom_group"),
                  c("year","vessel_length_group","gear_FR","area","seq_dom_group"),
                  c("year","gear_FR","area","seq_dom_group"),
                  c("month","vessel_length_group","gear_FR","seq_dom_group"),
                  c("month","gear_FR","seq_dom_group"),
                  c("quarter","vessel_length_group","gear_FR","seq_dom_group"),
                  c("quarter","gear_FR","seq_dom_group"),
                  c("year","vessel_length_group","gear_FR","seq_dom_group"),
                  c("year","gear_FR","seq_dom_group"))
for(level in step.levels){
  if(nrow(input.data[substr(metier_level_6,1,3)=="MIS"])>0){
    input.data <- missingMetiersByLevel(input.data,level,sequence.def)
  } else {break}
}
# Missing metier. Step 4: Search levels based on fleet register gear, vessel length group
step.levels<-list(c("month","vessel_length_group","gear_FR","area","gear_level6"),
                  c("month","gear_FR","area","gear_level6"),
                  c("quarter","vessel_length_group","gear_FR","area","gear_level6"),
                  c("quarter","gear_FR","area","gear_level6"),
                  c("year","vessel_length_group","gear_FR","area","gear_level6"),
                  c("year","gear_FR","area","gear_level6"),
                  c("month","vessel_length_group","gear_FR","gear_level6"),
                  c("month","gear_FR","gear_level6"),
                  c("quarter","vessel_length_group","gear_FR","gear_level6"),
                  c("quarter","gear_FR","gear_level6"),
                  c("year","vessel_length_group","gear_FR","gear_level6"),
                  c("year","gear_FR","gear_level6"),
                  c("month","vessel_length_group","gear_FR","area","gear_group"),
                  c("month","gear_FR","area","gear_group"),
                  c("quarter","vessel_length_group","gear_FR","area","gear_group"),
                  c("quarter","gear_FR","area","gear_group"),
                  c("year","vessel_length_group","gear_FR","area","gear_group"),
                  c("year","gear_FR","area","gear_group"),
                  c("month","vessel_length_group","gear_FR","gear_group"),
                  c("month","gear_FR","gear_group"),
                  c("quarter","vessel_length_group","gear_FR","gear_group"),
                  c("quarter","gear_FR","gear_group"),
                  c("year","vessel_length_group","gear_FR","gear_group"),
                  c("year","gear_FR","gear_group"))
for(level in step.levels){
  if(nrow(input.data[substr(metier_level_6,1,3)=="MIS"])>0){
    input.data <- missingMetiersByLevel(input.data,level,sequence.def)
  } else {break}
}

# Analyze vessel patterns
# Specify the percentage threshold of the number of sequences below which 
# a metier will be considered rare
rare.threshold <- 15
# Version 1 of the vessel pattern algorithm
# input.data <- vesselPatterns(input.data,sequence.def,rare.threshold,gear.list)
# Version 2 of the vessel pattern algorithm
input.data<-rareMetiersLvl5(input.data,sequence.def,rare.threshold)
# Vessel patterns. Step 1.
step.levels<-list(c("vessel_id","month","area","seq_dom_group","gear_group"),
                  c("vessel_id","month","area","seq_dom_group"),
                  c("vessel_id","quarter","area","seq_dom_group","gear_group"),
                  c("vessel_id","quarter","area","seq_dom_group"),
                  c("vessel_id","year","area","seq_dom_group","gear_group"),
                  c("vessel_id","year","area","seq_dom_group"),
                  c("vessel_id","month","seq_dom_group","gear_group"),
                  c("vessel_id","month","seq_dom_group"),
                  c("vessel_id","quarter","seq_dom_group","gear_group"),
                  c("vessel_id","quarter","seq_dom_group"),
                  c("vessel_id","year","seq_dom_group","gear_group"),
                  c("vessel_id","year","seq_dom_group"))
for(level in step.levels){
  if(nrow(input.data[metier_level_5_status=="rare" & is.na(metier_level_5_pattern)])>0){
    input.data <- vesselPatternsByLevel(input.data,level,sequence.def)
  } else {break}
}
# Vessel patterns. Step 2.
step.levels<-list(c("vessel_id","month","area","gear_level6"),
                  c("vessel_id","quarter","area","gear_level6"),
                  c("vessel_id","year","area","gear_level6"),
                  c("vessel_id","month","gear_level6"),
                  c("vessel_id","quarter","gear_level6"),
                  c("vessel_id","year","gear_level6"),
                  c("vessel_id","month","area","gear_group"),
                  c("vessel_id","quarter","area","gear_group"),
                  c("vessel_id","year","area","gear_group"),
                  c("vessel_id","month","gear_group"),
                  c("vessel_id","quarter","gear_group"),
                  c("vessel_id","year","gear_group"))
for(level in step.levels){
  if(nrow(input.data[metier_level_5_status=="rare" & is.na(metier_level_5_pattern)])>0){
    input.data <- vesselPatternsByLevel(input.data,level,sequence.def)
  } else {break}
}

# Metier level 6 assignment to metier level 5 which was assigned from pattern.
input.data[,metier_level_6_pattern:=NA]
step.levels<-list(c("vessel_id","month","area","metier_level_5"),
                  c("vessel_id","quarter","area","metier_level_5"),
                  c("vessel_id","year","area","metier_level_5"),
                  c("vessel_id","month","metier_level_5"),
                  c("vessel_id","quarter","metier_level_5"),
                  c("vessel_id","year","metier_level_5"))
for(level in step.levels){
  if(nrow(input.data[metier_level_5_status=="rare" & 
                     !is.na(metier_level_5_pattern) &
                     is.na(metier_level_6_pattern)])>0){
    input.data <- metiersLvl6ForLvl5pattern(input.data,level,sequence.def)
  } else {break}
}

# Create new metier columns where rare metiers are replaced with the ones found in the pattern.
input.data[,":="(metier_level_5_new=ifelse(is.na(metier_level_5_pattern),
                                           metier_level_5,
                                           metier_level_5_pattern),
                 metier_level_6_new=ifelse(is.na(metier_level_6_pattern),
                                           metier_level_6,
                                           metier_level_6_pattern))]

# Detailed metier level 6 assignment to general >0_0_0 cases.
input.data[,detailed_metier_level_6:=ifelse(grepl("_>0_0_0",metier_level_6_new),NA,metier_level_6_new)]
step.levels<-list(c("vessel_id","month","area","metier_level_5_new"),
                  c("vessel_id","quarter","area","metier_level_5_new"),
                  c("vessel_id","year","area","metier_level_5_new"),
                  c("vessel_id","month","metier_level_5_new"),
                  c("vessel_id","quarter","metier_level_5_new"),
                  c("vessel_id","year","metier_level_5_new"))
for(level in step.levels){
  if(nrow(input.data[is.na(detailed_metier_level_6)])>0){
    input.data <- detailedMetiersLvl6ForLvl5(input.data,level,sequence.def)
  } else {break}
}

# Save results
print("Saving results ...")
result<-input.data[order(vessel_id,trip_id,fishing_day,area,ices_rectangle,gear,mesh),
               .(Country,RCG,year,vessel_id,vessel_length,trip_id,haul_id,fishing_day,area,ices_rectangle,gear,gear_FR,mesh,selection,FAO_species,
                 registered_target_assemblage,KG,EUR,metier_level_6,mis_met_level,mis_met_number_of_seq,
                 metier_level_5,metier_level_5_status,
                 metier_level_5_pattern,ves_pat_level,ves_pat_number_of_seq,
                 metier_level_6_pattern,ves_pat_met6_level,ves_pat_met6_number_of_seq,
                 metier_level_5_new,metier_level_6_new,
                 detailed_metier_level_6,det_met6_level,det_met6_number_of_seq)]
write.csv(result,"metier_results.csv", na = "")
write.xlsx(file = "metier_results_summary.xlsx",result[,.(n_count=.N,
                                                          KG_sum=sum(KG, na.rm=T),
                                                          EUR_sum=sum(EUR, na.rm=T)),
                                                       by=.(Country, RCG, metier_level_6_new)][order(Country, RCG, metier_level_6_new)])







