library(stringr)
library(data.table)
library(openxlsx)
library(purrr)
library(dplyr)
library(lubridate)

rm(list=ls())
options(scipen=999)

year <- 2020

setwd("Q:\\dfad\\data\\Program\\Programmer til dannelse af DFAD\\DFAD_SQL")

# Import all functions
for(f in list.files(path="./Functions", full.names = T)){
  source(f)
}

########################################################################################################
#1. DNK Prepare input data
DFAD <- readRDS("Q:\\dfad\\data\\Data\\udvidet_data\\dfad_udvidet2020.rds")
DFAD1 <- DFAD
DFAD1$Country <- 'DNK'
DFAD1$vessel_id <- DFAD1$fid
DFAD1$vessel_length <- as.numeric(DFAD1$oal)
DFAD1$trip_id <- DFAD1$match_alle
DFAD1$fishing_day0 <- ifelse(is.na(DFAD1$fngdato),DFAD1$ldato, DFAD1$fngdato)
DFAD1$fishing_day <-as.Date(DFAD1$fishing_day0, origin="1970-01-01")
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

DFAD1$logbook[DFAD1$match !=''] <- 'Y'

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
# Load reference lists
url <- "https://github.com/ices-eg/RCGs/raw/master/Metiers/Reference_lists/AreaRegionLookup.csv"
area.list <- loadAreaList(url)
url <- "https://github.com/ices-eg/RCGs/raw/master/Metiers/Reference_lists/Metier%20Subgroup%20Species%202020.xlsx"
species.list <- loadSpeciesList(url)
#DNK: In Denmark Starfish is part of the Molluscs fishery
species.list$species_group[species.list$FAO_species=="STH"] <- "MOL"

url <- "https://github.com/ices-eg/RCGs/raw/master/Metiers/Reference_lists/RDB_ISSG_Metier_list.csv"
metier.list <- loadMetierList(url)

#DNK: Count number of level 5, to be able to remove the >0_0_0 when there are other options
metier.list$metier_level_5 <- substr(metier.list$metier_level_6,1,7)
metier.list <- metier.list %>% add_count(RCG, metier_level_5)
metier.list <- metier.list %>% filter(!(n>1 & mesh == ">0"))

#DNK: Remove metiers with end_year smaller than current year
metier.list$End_year[is.na(metier.list$End_year)] <- 2030
metier.list<-metier.list[!(End_year<year)]


#DNK correct: #url <- "./Reference_lists/Code-ERSGearType-v1.1.xlsx"
url <- "https://github.com/ices-eg/RCGs/raw/master/Metiers/Reference_lists/Code-ERSGearType-v1.1.xlsx"
gear.list <- loadGearList(url)
assemblage.list <- unique(c(species.list$species_group, species.list$dws_group))
assemblage.list <- assemblage.list[!is.na(assemblage.list)]

# Prepare input data
input.data[,EUR:=as.numeric(EUR)]
input.data[,KG:=as.numeric(KG)]
input.data[,c("selection_type","selection_mesh"):=data.table(str_split_fixed(selection,"_",2))]
input.data[,selection_type:=ifelse(selection_type=="",NA,selection_type)]
input.data[,selection_mesh:=ifelse(selection_mesh=="",NA,selection_mesh)]

# Assign RCG name to the input data
input.data <- merge(input.data, area.list, all.x = T, by = "area")
input.data[is.na(RCG) & substr(area,1,2) %in% c("31","34","41","47","51","57","58","87"),RCG:="LDF"]
input.data[is.na(RCG) & substr(area,1,2) == "37",RCG:="MED"]

# Assign species category to the input data
#DNK correct: remove NA FAO_species
input.data <- input.data[!is.na(input.data$area),]
input.data <- merge(input.data, species.list, all.x = T, by = "FAO_species")
#DNK Species corrections
input.data$species_group[input.data$FAO_species=="GUK"] <- "SPF"
input.data$species_group[input.data$species_group=="CEP"] <- "DEF" #No directed cephalopod fishery
input.data$gear[input.data$gear=="LLD" & input.data$species_group=="DEF"] <- "LLS"

# Process input data
#In the variable called sequence.def please include all columns that will constitute a fishing sequence
#This variable will be used as a key for grouping operations
sequence.def <- c("Country","year","vessel_id","vessel_length","trip_id","haul_id",
                  "fishing_day","area","ices_rectangle","gear","mesh","selection",
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

# Include DWS rules
input.data[dws_group=="DWS",seq_DWS_kg:=sum(KG, na.rm = T),
           by=c(sequence.def, "dws_group")]
input.data[,seq_total_kg:=sum(KG, na.rm = T),
           by=sequence.def]
input.data[,seq_DWS_perc:=ifelse(is.na(seq_DWS_kg),0,seq_DWS_kg/seq_total_kg)*100]
input.data[,seq_DWS_perc:=max(seq_DWS_perc),by=sequence.def]
input.data[seq_DWS_perc>8,seq_dom_group:="DWS"]

# Assign metier level 6
input.data$metier_level_6<-NA
input.data[,metier_level_6:=as.character(pmap(list(RCG,
                                          year,
                                          gear, 
                                          registered_target_assemblage,
                                          seq_dom_group, 
                                          mesh, 
                                          selection_type,
                                          selection_mesh),
                                     function(r,y,g,t,d,m,st,sm) getMetier(r,y,g,t,d,m,st,sm)))]

# Analyze vessel patterns
input.data[,metier_level_5:=paste(gear,ifelse(is.na(registered_target_assemblage),
                                              seq_dom_group,
                                              registered_target_assemblage),sep="_")]
pattern <- unique(input.data[,.SD,.SDcols=c(sequence.def,"metier_level_5")])
pattern <- pattern[,.(seq_no_lvl5 = .N), by=.(year, vessel_id, metier_level_5)]
pattern[,seq_perc_lvl5:=seq_no_lvl5/sum(seq_no_lvl5,na.rm = T)*100, by=.(year, vessel_id)]
pattern<-pattern[!is.na(metier_level_5)]
input.data <- merge(input.data, pattern,all.x = T , by=c("year", "vessel_id", "metier_level_5"))
# Specify the percentage threshold of the number of sequences below which 
# a metier will be considered rare
rare.threshold <- 13
input.data[seq_perc_lvl5<rare.threshold, metier_level_5:=NA]
pattern<-pattern[seq_perc_lvl5>=rare.threshold]
pattern[,c("gear","target_assemblage"):=data.table(str_split_fixed(metier_level_5,"_",2))]
pattern<-merge(pattern, gear.list, all.x = T, by.x = "gear", by.y = "gear_code")
input.data<-merge(input.data, gear.list, all.x = T, by.x = "gear", by.y = "gear_code")
input.data[is.na(metier_level_5),metier_level_5:=as.character(pmap(list(vessel_id,
                                                                        year,
                                                                        gear,
                                                                        gear_group,
                                                                        registered_target_assemblage,
                                                                        seq_dom_group),
                                              function(v,y,g,gg,rt,d) getMetierLvl5FromPattern(v,y,g,gg,rt,d)))]


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

#DNK: set metier assignment mark
input.data$metier_assign <- ""
input.data$metier_assign[input.data$metier_level_6 != "MIS_MIS_0_0_0"] <- "1. level 6" 

# Save results
print("Saving results ...")
result<-input.data[order(vessel_id,trip_id,fishing_day,area,ices_rectangle,gear,mesh),
               .(Country,RCG,year,vessel_id,vessel_length,trip_id,haul_id,fishing_day,area,ices_rectangle,gear, gear_FR, mesh,selection,FAO_species,
                 registered_target_assemblage,metier_level_6,metier_assign, KG,EUR,species_group,
                 seq_group_KG,seq_group_EUR,seq_dom_group,metier_level_5, seq_no_lvl5, seq_perc_lvl5)]
#write.csv(result,"metier_results.csv", na = "")
#write.xlsx(file = "metier_results_summary.xlsx",result[,.(n_count=.N,
#                                                          KG_sum=sum(KG, na.rm=T),
#                                                          EUR_sum=sum(EUR, na.rm=T)),
#                                                       by=.(Country, RCG, metier_level_6)][order(Country, RCG, metier_level_6)])

########################################################################################################

#3. Assign metiers where missing
#Note: to run this analysis, the first gear from the fleet register need to be included in the input data.
# 3.2 assign metiers to same vessel, month, area, species group 

#result$fishing_day1 <- as.Date(result$fishing_day, origin="1970-01-01")
result$month <- month(result$fishing_day)
result$quarter <- quarter(result$fishing_day)
result$VL_group[result$vessel_length < 8] <- "<8"
result$VL_group[result$vessel_length >= 8 & result$vessel_length < 10] <- "8-10"
result$VL_group[result$vessel_length >= 10 & result$vessel_length < 12] <- "10-12"
result$VL_group[result$vessel_length >= 12] <- ">=12"

metier3.2_haul <- result %>%
  group_by(vessel_id, month, quarter, year, area, gear_FR, VL_group, metier_level_6, species_group, haul_id, metier_assign) %>%
  summarise(EUR_sum=sum(EUR))

metier3.2_month <- metier3.2_haul %>%
  group_by(vessel_id, month, area, metier_level_6, species_group, metier_assign) %>%
  summarise(EUR_sum=sum(EUR_sum)) %>%
  arrange(vessel_id, month, area, metier_level_6, species_group, desc(EUR_sum))

metier3.2_metier <- metier3.2_month[metier3.2_month$metier_level_6 !="MIS_MIS_0_0_0",]
metier3.2_metier$main_metier_lvl6 <- metier3.2_metier$metier_level_6

metier3.2_metier1 <- metier3.2_metier %>%
  select(vessel_id, month, area, species_group, main_metier_lvl6) %>%
  group_by(vessel_id, month, area, species_group) %>%
  slice(1)

metier3.2_metier1 <- select(metier3.2_metier1, -metier_level_6)

metier3.2_haul1 <-merge(metier3.2_haul,metier3.2_metier1, by=c("vessel_id","month","area","species_group"), all=TRUE)
metier3.2_haul1$metier_assign[metier3.2_haul1$metier_level_6=="MIS_MIS_0_0_0" & (metier3.2_haul1$main_metier_lvl6!="MIS_MIS_0_0_0" & !is.na(metier3.2_haul1$main_metier_lvl6))] <- "2. vessel month"
metier3.2_haul1$metier_level_6 <- ifelse(metier3.2_haul1$metier_level_6=="MIS_MIS_0_0_0" & (metier3.2_haul1$main_metier_lvl6!="MIS_MIS_0_0_0" & !is.na(metier3.2_haul1$main_metier_lvl6)), metier3.2_haul1$main_metier_lvl6, metier3.2_haul1$metier_level_6)

# 3.3 assign metiers to same vessel, quarter, area and species group
metier3.3_haul <- metier3.2_haul %>%
  group_by(vessel_id, month, quarter, year, area, gear_FR, VL_group, metier_level_6, species_group, haul_id, metier_assign) %>%
  summarise(EUR_sum=sum(EUR_sum))

metier3.3_quarter <- metier3.3_haul %>%
  group_by(vessel_id, quarter, area, metier_level_6, species_group, metier_assign) %>%
  summarise(EUR_sum=sum(EUR_sum)) %>%
  arrange(vessel_id, quarter, area, metier_level_6, species_group, desc(EUR_sum))

metier3.3_metier <- metier3.3_quarter[metier3.3_quarter$metier_level_6 !="MIS_MIS_0_0_0",]
metier3.3_metier$main_metier_lvl6 <- metier3.3_metier$metier_level_6

metier3.3_metier1 <- metier3.3_metier %>%
  select(vessel_id, quarter, area, species_group, main_metier_lvl6) %>%
  group_by(vessel_id, quarter, area, species_group) %>%
  slice(1)

metier3.3_metier1 <- select(metier3.3_metier1, -metier_level_6)

metier3.3_haul1 <-merge(metier3.3_haul,metier3.3_metier1, by=c("vessel_id","quarter","area","species_group"), all=TRUE)
metier3.3_haul1$metier_assign[metier3.3_haul1$metier_level_6=="MIS_MIS_0_0_0" & (metier3.3_haul1$main_metier_lvl6!="MIS_MIS_0_0_0"  & !is.na(metier3.3_haul1$main_metier_lvl6))] <- "3. vessel quarter"
metier3.3_haul1$metier_level_6 <- ifelse(metier3.3_haul1$metier_level_6=="MIS_MIS_0_0_0" & (metier3.3_haul1$main_metier_lvl6!="MIS_MIS_0_0_0" & !is.na(metier3.3_haul1$main_metier_lvl6)), metier3.3_haul1$main_metier_lvl6, metier3.3_haul1$metier_level_6)

# 3.4 assign metiers to same vessel, year, area and species group 
metier3.4_haul <- metier3.3_haul1 %>%
  group_by(vessel_id, month, quarter, year, area, gear_FR, VL_group, metier_level_6, species_group, haul_id, metier_assign) %>%
  summarise(EUR_sum=sum(EUR_sum))

metier3.4_year <- metier3.4_haul %>%
  group_by(vessel_id, year, area, metier_level_6, species_group, metier_assign) %>%
  summarise(EUR_sum=sum(EUR_sum)) %>%
  arrange(vessel_id, year, area, metier_level_6, species_group, desc(EUR_sum))

metier3.4_metier <- metier3.4_year[metier3.4_year$metier_level_6 !="MIS_MIS_0_0_0",]
metier3.4_metier$main_metier_lvl6 <- metier3.4_metier$metier_level_6

metier3.4_metier1 <- metier3.4_metier %>%
  select(vessel_id, year, area, species_group, main_metier_lvl6) %>%
  group_by(vessel_id, year, area, species_group) %>%
  slice(1)

metier3.4_metier1 <- select(metier3.4_metier1, -metier_level_6)

metier3.4_haul1 <-merge(metier3.4_haul,metier3.4_metier1, by=c("vessel_id","year","area","species_group"), all=TRUE)
metier3.4_haul1$metier_assign[metier3.4_haul1$metier_level_6=="MIS_MIS_0_0_0" & (metier3.4_haul1$main_metier_lvl6!="MIS_MIS_0_0_0" & !is.na(metier3.4_haul1$main_metier_lvl6))] <- "4. vessel year"
metier3.4_haul1$metier_level_6 <- ifelse(metier3.4_haul1$metier_level_6=="MIS_MIS_0_0_0" & (metier3.4_haul1$main_metier_lvl6!="MIS_MIS_0_0_0" & !is.na(metier3.4_haul1$main_metier_lvl6)), metier3.4_haul1$main_metier_lvl6, metier3.4_haul1$metier_level_6)

# 3.5 assign metiers to same month, vessel length group, fleet register gear, area and species group 
metier3.5_haul <- metier3.4_haul1 %>%
  group_by(vessel_id, month, quarter, year, area, gear_FR, VL_group, metier_level_6, species_group, haul_id, metier_assign) %>%
  summarise(EUR_sum=sum(EUR_sum))

metier3.5_month_VL <- metier3.5_haul %>%
  group_by(month, VL_group, gear_FR, area, metier_level_6, species_group, metier_assign) %>%
  summarise(EUR_sum=sum(EUR_sum)) %>%
  arrange(month, VL_group, gear_FR, area, metier_level_6, species_group, desc(EUR_sum))

metier3.5_metier <- metier3.5_month_VL[metier3.5_month_VL$metier_level_6 !="MIS_MIS_0_0_0",]
metier3.5_metier$main_metier_lvl6 <- metier3.5_metier$metier_level_6

metier3.5_metier1 <- metier3.5_metier %>%
  select(month, VL_group, gear_FR, area, species_group, main_metier_lvl6) %>%
  group_by(month, VL_group, gear_FR, area, species_group) %>%
  slice(1)

metier3.5_metier1 <- select(metier3.5_metier1, -metier_level_6)

metier3.5_haul1 <-merge(metier3.5_haul,metier3.5_metier1, by=c("month","VL_group","gear_FR","area","species_group"), all=TRUE)
metier3.5_haul1$metier_assign[metier3.5_haul1$metier_level_6=="MIS_MIS_0_0_0" & metier3.5_haul1$main_metier_lvl6!="MIS_MIS_0_0_0"] <- "5. month, VL, gear_FR"
metier3.5_haul1$metier_level_6 <- ifelse(metier3.5_haul1$metier_level_6=="MIS_MIS_0_0_0" & metier3.5_haul1$main_metier_lvl6!="MIS_MIS_0_0_0", metier3.5_haul1$main_metier_lvl6, metier3.5_haul1$metier_level_6)

# 3.6 assign metiers to same month, fleet register gear, area and species group 
metier3.6_haul <- metier3.5_haul1 %>%
  group_by(vessel_id, month, quarter, year, area, gear_FR, VL_group, metier_level_6, species_group, haul_id, metier_assign) %>%
  summarise(EUR_sum=sum(EUR_sum))

metier3.6_month_FR <- metier3.6_haul %>%
  group_by(month, gear_FR, area, metier_level_6, species_group, metier_assign) %>%
  summarise(EUR_sum=sum(EUR_sum)) %>%
  arrange(month, gear_FR, area, metier_level_6, species_group, desc(EUR_sum))

metier3.6_metier <- metier3.6_month_FR[metier3.6_month_FR$metier_level_6 !="MIS_MIS_0_0_0",]
metier3.6_metier$main_metier_lvl6 <- metier3.6_metier$metier_level_6

metier3.6_metier1 <- metier3.6_metier %>%
  select(month, gear_FR, area, species_group, main_metier_lvl6) %>%
  group_by(month, gear_FR, area, species_group) %>%
  slice(1)

metier3.6_metier1 <- select(metier3.6_metier1, -metier_level_6)

metier3.6_haul1 <-merge(metier3.6_haul,metier3.6_metier1, by=c("month","gear_FR","area","species_group"), all=TRUE)
metier3.6_haul1$metier_assign[metier3.6_haul1$metier_level_6=="MIS_MIS_0_0_0" & metier3.6_haul1$main_metier_lvl6!="MIS_MIS_0_0_0"] <- "6. month, gear_FR"
metier3.6_haul1$metier_level_6 <- ifelse(metier3.6_haul1$metier_level_6=="MIS_MIS_0_0_0" & metier3.6_haul1$main_metier_lvl6!="MIS_MIS_0_0_0", metier3.6_haul1$main_metier_lvl6, metier3.6_haul1$metier_level_6)

metier3.6_haul1$metier_level_6_correct <- metier3.6_haul1$metier_level_6
metier3.6_haul1$metier_assign_correct <- metier3.6_haul1$metier_assign
metier_haul <- metier3.6_haul1%>%
  group_by(haul_id, metier_level_6_correct, metier_assign_correct) %>%
  summarise(EUR_sum=sum(EUR_sum))

input.data1 <- merge(input.data, metier_haul, by="haul_id")
input.data1$metier_assign <- ifelse(input.data1$metier_assign_correct!="1. level 6", input.data1$metier_assign_correct, input.data1$metier_assign)
input.data1$metier_level_6 <- ifelse(input.data1$metier_level_6=="MIS_MIS_0_0_0", input.data1$metier_level_6_correct,input.data1$metier_level_6)

########################################################################################################
#4. DNK Merge back to DFAD data
#DFAD$FAO_species <- DFAD$fao_species
#DFAD2 <- merge(DFAD, result2, all.x = T, all.y = T, by = c("haul_id","FAO_species"))
DFAD2 <- merge(DFAD, metier_haul, all.x = T, all.y = T, by = c("haul_id"))

# Determine the dominant species (by weight), its group and percentage in the total catch for each sequence
# (sequence = trip_id+haul_id(if available) +fishing_day+area+ices_rectangle+gear+mesh+selection)
# Information on dominant species can be used to identify the mesh size in case it is missing.
# Function for mesh size determination will be developed. Should the function use national reference lists of mesh sizes corresponding to target species?
#input.data<-input.data[,":="(seq_dom_species_KG = FAO_species[which.max(KG)],
#                               seq_dom_species_group = species_group[which.max(KG)],
#                               seq_dom_species_perc_KG = round(max(KG)/sum(KG)*100,1)),
#            by=.(Country,year,vessel_id,vessel_length,trip_id,haul_id,fishing_day,area,
#                 ices_rectangle,gear,mesh,selection,registered_target_assemblage)][order(vessel_id,trip_id,fishing_day,area,ices_rectangle,gear,mesh)]

# Identify sequences where the group of the dominant species differs from the dominant group.
# The results of this step do not affect further calculations. It highlights the above-mentioned situations, that may indicate an input data error.
# input.data[,group.mismatch:=ifelse(seq_dom_species_group!=seq_dom_group,1,0)]

# Save results
#input.data<-input.data[order(vessel_id,trip_id,fishing_day,area,ices_rectangle,gear,mesh),
#                       .(Country,year,vessel_id,vessel_length,trip_id,haul_id,fishing_day,area,ices_rectangle,gear,mesh,selection,FAO_species,
#                         registered_target_assemblage,metier_level_6,KG,EUR,species_group,seq_dom_species_KG,seq_dom_species_group,
#                         seq_dom_species_perc_KG,seq_group_KG,seq_group_EUR,seq_dom_group,group.mismatch)]

#table(input.data[,.(metier=ifelse(is.na(metier_level_6),0,1))])

########################################################################################################
#5. DNK test

table(input.data1[,.(metier=ifelse(is.na(metier_level_6),0,1))])

input.data1$check[input.data1$metier_level_6 == input.data1$metier_level_6_DNK] <- "equal"
input.data1$check[input.data1$metier_level_6 != input.data1$metier_level_6_DNK] <- "not equal"
input.data1$n <- 1
input.data1$approved[input.data1$check=="equal"] <- 'Yes'
input.data1$approved[input.data1$check=="not equal"] <- 'No'
input.data1$approved[input.data1$RCG=='BALT' & input.data1$metier_level_6 %in% c('OTB_DEF_>=120_3_120','OTB_DEF_105-115_1_120','DRB_MOL_>0_0_0',
                                                        'SDN_DEF_>=120_3_120','OTM_SPF_16-31_0_0','GNS_SPF_32-89_0_0',
                                                        'GNS_DEF_32-89_0_0','OTB_SPF_32-89_0_0 ','OTB_SPF_32-89_0_0',
                                                        'SSC_DEF_>=120_3_120','SSC_DEF_105-115_1_120','PTM_SPF_32-89_0_0',
                                                        'OTM_SPF_32-89_0_0','PTB_DEF_>=120_3_120','OTB_SPF_>=120_3_120',
                                                        'PTM_DEF_>=120_3_120',' SSC_DEF_115-120_3_115','LHP_DEF_0_0_0',
                                                        'SDN_DEF_105-115_1_120','OTB_DEF_32-89_0_0','GNS_DEF_90-109_0_0',
                                                        'SSC_DEF_115-120_3_115')] <- 'Yes'

input.data1$approved[input.data1$RCG=='NSEA' & input.data1$metier_level_6 %in% c('OTB_CRU_90-99_1_270','OTB_CRU_90-99_1_300',
                                                        'OTB_DEF_90-99_1_140','OTB_DEF_90-99_1_270','OTB_CRU_90-99_1_140',
                                                        'OTB_CRU_90-99_0_0','SDN_DEF_100-119_0_0','OTB_DEF_100-119_1_140',
                                                        'TB_CRU_90-99_1_180','OTB_DEF_90-99_1_180','OTB_CRU_90-99_1_180',
                                                        'OTB_DEF_90-99_0_0',' SSC_DEF_100-119_0_0','OTB_DEF_90-99_1_300',
                                                        'OTB_DEF_100-119_0_0','OTB_CRU_70-89_0_0','DRB_MOL_>0_0_0',
                                                        'OTB_DEF_90-99_0_0','OTB_CRU_32-69_0_0','OTB_DEF_100-119_1_180',
                                                        'SSC_DEF_100-119_0_0','OTB_DEF_100-119_1_270','GNS_CRU_>=220_0_0',
                                                        'OTB_DEF_70-89_0_0','GNS_CRU_120-219_0_0','GNS_CRU_90-99_0_0',
                                                        'OTB_DWS_>=120_0_0','PTB_CRU_90-99_0_0','OTB_DWS_90-99_0_0',
                                                        'OTB_CRU_100-119_1_140')] <- 'Yes'


tjek <- input.data1 %>%
  group_by(Country, RCG, check, approved, metier_level_6, metier_level_6_DNK) %>%
  summarise(n_count=sum(n), KG_sum=sum(KG), EUR_sum=sum(EUR))

write.csv(tjek,"Q:\\dfad\\users\\joeg\\home\\LOG\\190430_Metier_script_test\\metier_tjek.csv")

ggplot(tjek, aes(x=RCG, y=n_count, fill=approved))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  theme_minimal()

tjek_BALT <- tjek[tjek$RCG=="BALT" & tjek$metier_level_6 !="MIS_MIS_0_0_0",]
tjek_NSEA <- tjek[tjek$RCG=="NSEA" & tjek$metier_level_6 !="MIS_MIS_0_0_0",]


ggplot(tjek_BALT, aes(x=RCG, y=n_count, fill=approved))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  theme_minimal()

ggplot(tjek_NSEA, aes(x=RCG, y=n_count, fill=approved))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  theme_minimal()

tjek_BALT1 <- tjek_BALT %>%
  filter(approved=="No") %>%
  arrange(desc(n_count))

tjek_BALT1

tjek_NSEA1 <- tjek_NSEA %>%
  filter(approved=="No") %>%
  arrange(desc(n_count))

tjek_NSEA1


Output <- input.data1 %>%
  group_by(Country, RCG, metier_level_6) %>%
  summarise(n_count=sum(n), KG_sum=sum(KG), EUR_sum=sum(EUR))


#tjek.metier.data <- input.data[input.data$metier_level_6 == "PTM_DEF_<16_0_0" & input.data$metier_level_6_DNK=="OTB_DEF_<16_0_0" ,] 
tjek.metier.data <- DFAD2[DFAD2$metier_level_6 == "OTB_SPF_<16_0_0" & DFAD2$metier_level6_ret=="OTB_SPF_>=105_1_120" ,] 
tjek.metier.data1 <- input.data1[input.data1$metier_level_6 == "OTB_SPF_<16_0_0" & input.data1$metier_level_6_DNK=="OTB_SPF_>=105_1_120" ,] 

tjek.metier.data2 <- select(tjek.metier.data, c("metier_level_6","metier_level6_ret","metier_ret_mrk"))


tjek.metier.data2 <- result[result$metier_level_6 == "OTB_DEF_105-115_1_120" ,] 

#################################################################################