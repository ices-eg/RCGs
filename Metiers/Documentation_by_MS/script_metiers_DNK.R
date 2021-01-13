library(stringr)
library(data.table)
library(openxlsx)
library(purrr)
library(dplyr)

year <- 2020

setwd("Q:\\dfad\\data\\Program\\Programmer til dannelse af DFAD\\DFAD_SQL")

# Import all functions
for(f in list.files(path="./Functions", full.names = T)){
  source(f)
}

#DNK Prepare input data
DFAD <- readRDS("Q:\\dfad\\data\\Data\\udvidet_data\\dfad_udvidet2020.rds")
DFAD1 <- DFAD
DFAD1$Country <- 'DNK'
DFAD1$vessel_id <- DFAD1$fid
DFAD1$vessel_length <- DFAD1$oal
DFAD1$trip_id <- DFAD1$match_alle
DFAD1$fishing_day <- DFAD1$fngdato
DFAD1$gear <- DFAD1$redskb
DFAD1$gear[DFAD1$gear==""] <- 'NK'
DFAD1$gear[DFAD1$gear %in% c('DRC','DRO','BMS','BRJ','DRH')] <- 'DRB'
DFAD1$gear[DFAD1$gear %in% c('TBN','TB','OTT')] <- 'OTB'
DFAD1$gear[DFAD1$gear %in% c('LX','LLX','LHM')] <- 'LHP'
DFAD1$gear[DFAD1$gear %in% c('LL','LLD')] <- 'LLS'
DFAD1$gear[DFAD1$gear %in% c('TM')] <- 'OTM'
DFAD1$gear[DFAD1$gear %in% c('FIX')] <- 'FPO'
DFAD1$gear[DFAD1$gear %in% c('GN','GTR','GTN')] <- 'GNS'
DFAD1$mesh <- as.numeric(DFAD1$maske)
DFAD1$mesh[DFAD1$gear=='DRB' & DFAD1$mesh==999] <- 120
DFAD1$mesh[DFAD1$gear %in% c('LLS','LHP','MIS','UNK')] <- NA

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
  group_by(Country, year, vessel_id, vessel_length, trip_id, haul_id, fishing_day, area, ices_rectangle, gear, mesh, selection, FAO_species, registered_target_assemblage, metier_level_6, metier_level_6_DNK, measure, logbook) %>%
  summarise(KG=sum(KG), EUR=sum(EUR))

#DNK: to start with, only with logbook
input.data <- data.table(DFAD_sum[DFAD_sum$logbook=='Y',])

# Load the input data
#data.file <- "Metier_data_format_Example_test_input.csv"
#input.data <- loadInputData(data.file)

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





# Save results
print("Saving results ...")
result<-input.data[order(vessel_id,trip_id,fishing_day,area,ices_rectangle,gear,mesh),
               .(Country,RCG,year,vessel_id,vessel_length,trip_id,haul_id,fishing_day,area,ices_rectangle,gear,mesh,selection,FAO_species,
                 registered_target_assemblage,metier_level_6,KG,EUR,species_group,
                 seq_group_KG,seq_group_EUR,seq_dom_group,metier_level_5, seq_no_lvl5, seq_perc_lvl5)]
#write.csv(result,"metier_results.csv", na = "")
#write.xlsx(file = "metier_results_summary.xlsx",result[,.(n_count=.N,
#                                                          KG_sum=sum(KG, na.rm=T),
#                                                          EUR_sum=sum(EUR, na.rm=T)),
#                                                       by=.(Country, RCG, metier_level_6)][order(Country, RCG, metier_level_6)])

#Merge back to DFAD data
result1 <- select(result, haul_id,FAO_species, metier_level_6, metier_level_5)

DFAD$FAO_species <- DFAD$fao_species
DFAD2 <- merge(DFAD, result1, all.x = T, all.y = T, by = c("haul_id","FAO_species"))

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

#DNK test 
table(input.data[,.(metier=ifelse(is.na(metier_level_6),0,1))])

input.data$check[input.data$metier_level_6 == input.data$metier_level_6_DNK] <- "equal"
input.data$check[input.data$metier_level_6 != input.data$metier_level_6_DNK] <- "not equal"
input.data$n <- 1
tjek <- input.data %>%
  group_by(Country, RCG, check, metier_level_6, metier_level_6_DNK) %>%
  summarise(n_count=sum(n), KG_sum=sum(KG), EUR_sum=sum(EUR))

write.csv(tjek,"Q:\\dfad\\users\\joeg\\home\\LOG\\190430_Metier_script_test\\metier_tjek.csv")

Output <- input.data %>%
  group_by(Country, RCG, metier_level_6) %>%
  summarise(n_count=sum(n), KG_sum=sum(KG), EUR_sum=sum(EUR))


tjek.metier.data <- input.data[input.data$metier_level_6 == "OTB_DEF_32-89_0_0" & input.data$metier_level_6_DNK=="OTB_DEF_>=105_1_120" ,] 

tjek.metier.data <- result[result$metier_level_6 == "OTB_DEF_105-115_1_120" ,] 
