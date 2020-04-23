library(stringr)
library(data.table)
library(openxlsx)
library(purrr)

# Import all functions
for(f in list.files(path="./Scripts/Functions", full.names = T)){
  source(f)
}

# Load the input data
data.file <- "Metier_data_format_Example_test_input.csv"
input.data <- loadInputData(data.file)

# Load reference lists
url <- "https://github.com/ices-eg/RCGs/raw/master/Metiers/Reference_lists/AreaRegionLookup.csv"
area.list <- loadAreaList(url)
url <- "https://github.com/ices-eg/RCGs/raw/master/Metiers/Reference_lists/Metier%20Subgroup%20Species%202019%2004.xlsx"
species.list <- loadSpeciesList(url)
url <- "https://github.com/ices-eg/RCGs/raw/master/Metiers/Reference_lists/RDB_ISSG_Metier_list.csv"
metier.list <- loadMetierList(url)

#In the variable called sequence.def please include all columns that will constitute a fishing sequence
#This variable will be used as a key for grouping operations
sequence.def <- c("Country","year","vessel_id","vessel_length","trip_id","haul_id",
                  "fishing_day","area","ices_rectangle","gear","mesh","selection",
                  "registered_target_assemblage")

input.data <- prepareInputData(input.data)

input.data <- processInputData(input.data, seq.def = sequence.def)

# Assign metier codes
start_time <- Sys.time()
result <- assignMetiers(input.data)
end_time <- Sys.time()
print(paste0("Metiers assigned in ",end_time-start_time," seconds."))


# Analyse vessel pattern
result <- vesselPattern(result, sequence.def)

# Save results
print("Saving results ...")
result<-result[order(vessel_id,trip_id,fishing_day,area,ices_rectangle,gear,mesh),
               .(Country,RCG,year,vessel_id,vessel_length,trip_id,haul_id,fishing_day,area,ices_rectangle,gear,mesh,selection,FAO_species,
                 registered_target_assemblage,metier_level_6,KG,EUR,species_group,
                 seq_group_KG,seq_group_EUR,seq_dom_group,metier_level_5, seq_no_lvl5, seq_perc_lvl5)]
write.csv(result,"metier_results.csv", na = "")
write.xlsx(file = "metier_results_summary.xlsx",result[,.(n_count=.N,
                                                          KG_sum=sum(KG, na.rm=T),
                                                          EUR_sum=sum(EUR, na.rm=T)),
                                                       by=.(Country, RCG, metier_level_6)][order(Country, RCG, metier_level_6)])





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



