library(stringr)
library(data.table)
library(openxlsx)
library(purrr)
library(lubridate)

# Import all functions
for(f in list.files(path="./Scripts/Functions", full.names = T)){
  source(f)
}

# Load the input data
data.file <- "data_input_example.csv"
input.data <- loadInputData(data.file)

# Load reference lists
url <- "https://github.com/ices-eg/RCGs/raw/master/Metiers/Reference_lists/AreaRegionLookup.csv"
area.list <- loadAreaList(url)
url <- "https://github.com/ices-eg/RCGs/raw/master/Metiers/Reference_lists/Metier%20Subgroup%20Species%202020.xlsx"
species.list <- loadSpeciesList(url)
url <- "https://github.com/ices-eg/RCGs/raw/master/Metiers/Reference_lists/RDB_ISSG_Metier_list.csv"
metier.list <- loadMetierList(url)
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

# Assign RCG names to the input data
input.data <- merge(input.data, area.list, all.x = T, by = "area")

# Assign species category to the input data
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

# Apply DWS rules
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

# Missing metiers
input.data[,":="(month=month(dmy(fishing_day)),
                           quarter=quarter(dmy(fishing_day)))]
input.data[,vessel_length_group:=cut(vessel_length,breaks=c(0,10,12,18,24,40,Inf),right=F)]
input.data.sequances <- unique(input.data[metier_level_6!="MIS_MIS_0_0_0",.SD,
                                          .SDcols=c(sequence.def,
                                                    "seq_dom_group","metier_level_6",
                                                    "gear_FR","month","quarter",
                                                    "vessel_length_group")])

input.data[metier_level_6=="MIS_MIS_0_0_0",
           metier_level_6:=as.character(pmap(list(vessel_id,month,area,seq_dom_group,
                                                  quarter,year,vessel_length_group,
                                                  gear_FR),
                                              function(v,m,a,sg,q,y,vg,gfr) 
                                                getMissingMetier(v,m,a,sg,q,y,vg,gfr)))]




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
write.csv(result,"metier_results.csv", na = "")
write.xlsx(file = "metier_results_summary.xlsx",result[,.(n_count=.N,
                                                          KG_sum=sum(KG, na.rm=T),
                                                          EUR_sum=sum(EUR, na.rm=T)),
                                                       by=.(Country, RCG, metier_level_6)][order(Country, RCG, metier_level_6)])







