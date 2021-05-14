library(stringr)
library(data.table)
library(openxlsx)
library(purrr)
library(dplyr)
library(lubridate)
library(haven)
library(ggplot2)

rm(list=ls())
gc()

options(scipen=999)

year <- 2020

setwd("Q:\\dfad\\data\\Program\\Programmer til dannelse af DFAD\\DFAD_SQL")

# Import all functions
for(f in list.files(path="./Functions", full.names = T)){
  source(f)
}
rm(f)


########################################################################################################
#1. DNK Prepare input data
DFAD <- readRDS(paste("Q:\\dfad\\data\\Data\\udvidet_data\\dfad_udvidet",year,".rds", sep=""))
DFAD1 <- DFAD
DFAD1$Country <- 'DNK'
DFAD1$vessel_id <- DFAD1$fid
DFAD1$vessel_length <- as.numeric(DFAD1$oal)
DFAD1$trip_id <- DFAD1$match_alle
DFAD1$fishing_day0 <- ifelse(is.na(DFAD1$fngdato),DFAD1$ldato, DFAD1$fngdato)
DFAD1$fishing_day1 <-as.Date(DFAD1$fishing_day0, origin="1970-01-01")
DFAD1$fishing_day <- format(DFAD1$fishing_day1, "%d-%m-%Y")
#Gear corrections
DFAD1$gear <- DFAD1$redskb
DFAD1$gear[DFAD1$gear==""] <- 'NK'
DFAD1$gear[DFAD1$gear %in% c('DRC','DRO','BMS','BRJ','DRH')] <- 'DRB'
DFAD1$gear[DFAD1$gear %in% c('TBN','TB','OTT')] <- 'OTB'
DFAD1$gear[DFAD1$gear %in% c('LX','LLX','LHM')] <- 'LHP'
#DFAD1$gear[DFAD1$gear %in% c('LL','LLD')] <- 'LLS'
DFAD1$gear[DFAD1$gear %in% c('LL')] <- 'LLS'
DFAD1$gear[DFAD1$gear %in% c('TM')] <- 'OTM'
DFAD1$gear[DFAD1$gear %in% c('FIX')] <- 'FPO'
DFAD1$gear[DFAD1$gear %in% c('GN','GTR','GTN')] <- 'GNS'
DFAD1$gear[DFAD1$gear=="TBS" & DFAD1$art=="DVR"] <- "OTB"
DFAD1$gear[DFAD1$gear=="TBS" & DFAD1$art=="HRJ"] <- "TBB"
DFAD1$gear[(is.na(DFAD1$gear) || DFAD1$gear=="MIS") & DFAD1$art %in% c('BLL','GLL') ] <- "FPN"
DFAD1$gear[is.na(DFAD1$match) & DFAD1$oal < 10 & DFAD1$art == "BRS"] <- "GNS" #Avoiding OTM metiers in SSF

#Fleet register gear_FR corrections
DFAD1$gear_FR <- DFAD1$redskba
#Correcting gears in fleet register based on observer data
DFAD1$gear_FR[DFAD1$fid %in% c('H39','HG102','FN19','FN22','FN133','HG290','FN168','RI291','FN91','HM324','SG172','HG198')] <- "OTB"
DFAD1$gear_FR[DFAD1$fid %in% c('AS86','FN6','RS21','ND112','MI212')] <- "FPN"
DFAD1$gear_FR[DFAD1$fid %in% c('HG3','S108','HG85','H264','HG48','FN31','O17','SG98','SG237','KA21','RI383','AS91','A150','H422','A45','H65',
                             'SG24','L140','H41','ND40','R272')] <- "GNS"


#Mesh size corrections
DFAD1$mesh <- as.numeric(DFAD1$maske)
DFAD1$mesh[DFAD1$gear=='DRB' & DFAD1$mesh==999] <- 120
DFAD1$mesh[DFAD1$gear %in% c('LLS','LHP','MIS','UNK','LL','LLD','LTL')] <- NA
DFAD1$mesh[is.na(DFAD1$mesh) & DFAD1$fid %in% c('S43','HV133','O78','O81','SG172','HG27','HM141','HM80','L511')] <- 120
DFAD1$mesh[is.na(DFAD1$mesh) & DFAD1$fid %in% c('ND392')] <- 110
DFAD1$mesh[is.na(DFAD1$mesh) & DFAD1$fid %in% c('HM426') & DFAD1$gear == "OTB"] <- 105
DFAD1$mesh[is.na(DFAD1$mesh) & DFAD1$fid %in% c('O95','RI174')] <- 90


#Selection panel
DFAD1$selection[DFAD1$fao_area=='27.3.a.21' & DFAD1$gear =='OTB' & DFAD1$redskstr=='04'] <- '2_35'
DFAD1$selection[DFAD1$fao_area %in% c('27.3.a.20','27.3.a.21') & DFAD1$gear =='OTB' & DFAD1$redskstr=='01'] <- '1_180'
DFAD1$selection[DFAD1$fao_area %in% c('27.3.a.20','27.3.a.21') & DFAD1$gear =='OTB' & DFAD1$redskstr=='02'] <- '1_270'
DFAD1$selection[DFAD1$fao_area %in% c('27.3.a.20','27.3.a.21') & DFAD1$gear =='OTB' & DFAD1$redskstr=='03'] <- '1_180'
DFAD1$selection[DFAD1$fao_area %in% c('27.3.a.20','27.3.a.21') & DFAD1$gear =='OTB' & DFAD1$redskstr=='05'] <- '1_300'
DFAD1$selection[DFAD1$fao_area %in% c('27.3.a.20','27.3.a.21') & DFAD1$gear =='OTB' & DFAD1$redskstr=='06'] <- '1_270'
DFAD1$selection[DFAD1$fao_area %in% c('27.3.a.20','27.3.a.21') & DFAD1$gear =='OTB' & DFAD1$redskstr=='07'] <- '1_140'
DFAD1$selection[DFAD1$fao_area %in% c('27.3.a.20','27.3.a.21') & DFAD1$gear =='OTB' & DFAD1$redskstr=='08'] <- '1_270'
DFAD1$selection[DFAD1$fao_area %in% c('27.3.a.20','27.3.a.21') & DFAD1$gear =='OTB' & DFAD1$redskstr=='09'] <- '1_140'

DFAD1$metier_level_6 <- ''
DFAD1$metier_level_6_DNK <- DFAD1$metier_level6_ret
DFAD1$ices_rectangle <- DFAD1$square_ret
DFAD1$registered_target_assemblage <- NA
DFAD1$measure <- 'value'
DFAD1$KG <- DFAD1$hel
DFAD1$EUR <- DFAD1$vrd/7.45
DFAD1$FAO_species <- DFAD1$fao_species
DFAD1$area <- DFAD1$fao_area
DFAD1 <- DFAD1[DFAD1$EUR>0,]
DFAD1$logbook <- ''
DFAD1$logbook[DFAD1$match !=''] <- 'Y'

DFAD1$year <- as.integer(DFAD1$year)
DFAD1$fishing_day <- as.character(DFAD1$fishing_day)
DFAD1$mesh <- as.integer(DFAD1$mesh)
DFAD1$registered_target_assemblage <- as.character(DFAD1$registered_target_assemblage)

DFAD_sum <- DFAD1 %>%
  group_by(Country, year, vessel_id, vessel_length, trip_id, haul_id, fishing_day, area, ices_rectangle, gear, gear_FR, mesh, selection, FAO_species, registered_target_assemblage, metier_level_6, metier_level_6_DNK, measure, logbook) %>%
  summarise(KG=sum(KG), EUR=sum(EUR))

#DNK: to start with, only with logbook
#input.data <- data.table(DFAD_sum[DFAD_sum$logbook=='Y',])
input.data <- data.table(DFAD_sum)

# Load the input data
#data.file <- "Metier_data_format_Example_test_input.csv"
#input.data <- loadInputData(data.file)

########################################################################################################
# 2. Assign metiers

# Validate input data
input.data <- validateInputData(input.data)

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
#assemblage.list <- unique(c(species.list$species_group, species.list$dws_group))
#assemblage.list <- assemblage.list[!is.na(assemblage.list)]
rm(url)

#DNK: Count number of level 5, to be able to remove the >0_0_0 when there are other options
metier.list$metier_level_5 <- substr(metier.list$metier_level_6,1,7)
metier.list <- metier.list %>% add_count(RCG, metier_level_5)
metier.list <- metier.list %>% filter(!(n>1 & mesh == ">0"))

#DNK: Remove metiers with end_year smaller than current year
metier.list$End_year[is.na(metier.list$End_year)] <- 2030
metier.list<-metier.list[!(End_year<year)]

# Prepare input data
input.data[,EUR:=as.numeric(EUR)]
input.data[,KG:=as.numeric(KG)]
input.data[,c("selection_type","selection_mesh"):=data.table(str_split_fixed(selection,"_",2))]
input.data[,selection_type:=ifelse(selection_type=="",NA,selection_type)]
input.data[,selection_mesh:=ifelse(selection_mesh=="",NA,selection_mesh)]

# Assign RCG name to the input data
input.data <- merge(input.data, area.list, all.x = T, by = "area")

# Assign species category to the input data
input.data <- input.data[!is.na(input.data$area),]

# Assign species category to the input data
input.data <- merge(input.data, species.list, all.x = T, by = "FAO_species")

#DNK Species corrections
input.data$species_group[input.data$FAO_species=="GUK"] <- "SPF"
input.data$species_group[input.data$species_group %in% c("CEP","DES")] <- "DEF" #No directed cephalopod and benthos fisheries
input.data$species_group[input.data$gear %in% c("SSC")] <- "DEF"
input.data$species_group[input.data$gear %in% c("DRB")] <- "MOL"
input.data$gear[input.data$gear=="LLD" & input.data$species_group=="DEF"] <- "LLS"
input.data$gear[input.data$RCG=="BALT" & input.data$gear=="GND" & input.data$species_group=="DEF"] <- "GNS"

#Define MCD fishery: Sweden and Denmark have agreed on a procedure for area 3.a (Skagerrak and Kattegat): 
#all OTB and OTT with mesh size 90-119 will be set to MCD as it is a mixed fishery for nephrops and demersal fish.
input.data$species_group[input.data$area %in% c("27.3.a.20","27.3.a.21") & input.data$gear %in% c("OTB","PTB","OTT") & (input.data$mesh>=90 & input.data$mesh <= 119)] <- "MCD"

# Assign gear group and re-coded gear name to the input data
input.data<-merge(input.data, gear.list, all.x = T, by.x = "gear", by.y = "gear_code")

# Process input data
#In the variable called sequence.def please include all columns that will constitute a fishing sequence
#This variable will be used as a key for grouping operations
sequence.def <- c("Country","year","vessel_id","vessel_length","trip_id","haul_id")
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

#DNK
MIS <- input.data[input.data$metier_level_6 %like% "MIS",]
print(paste("Nrows MIS step0: ", nrow(MIS)), sep=",")

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

#DNK
MIS <- input.data[input.data$metier_level_6 %like% "MIS",]
print(paste("Nrows MIS step1: ", nrow(MIS)), sep=",")

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

#DNK
MIS <- input.data[input.data$metier_level_6 %like% "MIS",]
print(paste("Nrows MIS step2: ", nrow(MIS)), sep=",")

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

#DNK
MIS <- input.data[input.data$metier_level_6 %like% "MIS",]
print(paste("Nrows MIS step3: ", nrow(MIS)), sep=",")

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

#DNK
MIS <- input.data[input.data$metier_level_6 %like% "MIS",]
print(paste("Nrows MIS step4: ", nrow(MIS)), sep=",")


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

#DNK: Correct some metiers based on expert knowledge
#If pct sandeel>90 assign to sandeel metier OTB_DEF_<16_0_0
input.data$KG_SAN <- ifelse(input.data$FAO_species=="SAN", input.data$KG, 0)
sandeelFishery <- input.data %>%
  group_by(haul_id) %>%
  summarize(KG_SAN_haul=sum(KG_SAN), KG_haul=sum(KG))
sandeelFishery <- sandeelFishery[sandeelFishery$KG_SAN_haul>0 & !is.na(sandeelFishery$KG_SAN_haul),]
sandeelFishery$pct_SAN <- (sandeelFishery$KG_SAN_haul/sandeelFishery$KG_haul)
input.data <- merge(input.data, sandeelFishery,by="haul_id", all = TRUE)
input.data$metier_level_6[input.data$pct_SAN>=0.9] <- "OTB_DEF_<16_0_0"
#Corrections
input.data$metier_level_6[input.data$metier_level_6=="TBB_DEF_16-31_0_0"] <- "TBB_CRU_16-31_0_0"
input.data$metier_level_6[input.data$area=="27.3.c.22" & input.data$gear=="FPO" & input.data$seq_dom_group=="CRU"] <- "FPN_CRU_>0_0_0"
input.data$metier_level_6[input.data$area=="27.3.c.22" & input.data$gear=="FP0" & input.data$seq_dom_group=="DEF"] <- "FPO_DEF_>0_0_0"
input.data$metier_level_6[input.data$area=="27.4.b" & input.data$gear %in% c("FP0","FPN")] <- "FYK_CAT_>0_0_0"


# Save results
print("Saving results ...")
result<-input.data[order(vessel_id,trip_id,fishing_day,area,ices_rectangle,gear,mesh),
                   .(Country,RCG,year,vessel_id,vessel_length,trip_id,haul_id,fishing_day,area,ices_rectangle,gear,gear_FR,mesh,selection,FAO_species,
                     registered_target_assemblage,KG,EUR,metier_level_6,metier_level_6_DNK, mis_met_level,mis_met_number_of_seq,
                     metier_level_5,metier_level_5_status,
                     metier_level_5_pattern,ves_pat_level,ves_pat_met6_number_of_seq,
                     metier_level_5_new,metier_level_6_new,
                     detailed_metier_level_6,det_met6_level,det_met6_number_of_seq)]
write.csv(result,paste("Q:\\dfad\\data\\Program\\Programmer til dannelse af DFAD\\DFAD_SQL\\Metier_results\\metier_results",year,".csv", na = "", sep=","))
write.xlsx(file = paste("Q:\\dfad\\data\\Program\\Programmer til dannelse af DFAD\\DFAD_SQL\\Metier_results\\metier_results_summary",year,".xlsx", sep=","),result[,.(n_count=.N,
                                                          KG_sum=sum(KG, na.rm=T),
                                                          EUR_sum=sum(EUR, na.rm=T)),
                                                       by=.(Country, RCG, metier_level_6)][order(Country, RCG, metier_level_6)])




########################################################################################################


########################################################################################################
