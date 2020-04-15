library(stringr)
library(data.table)
library(openxlsx)
library(purrr)


data.file <- "Scripts/Metier_data_format_Example_test.csv"


# Function that finds a metier code based on the given parameters
getMetier<-function(p.rcg, p.gear, p.target, p.mesh, p.selection){
  #First step - assign metier based on rcg, gear, target assemblage, mesh size, selection dev.
  #Ignore >0 metiers, they will be included at the end when no other metier was assigned
  metier<-as.character(metier.list[!mesh %in% c("0", ">0") & RCG==p.rcg & 
                                     gear==p.gear & 
                                     target==p.target & 
                                     data.table::between(p.mesh,m_size_from,m_size_to) &
                                     (p.selection == sd_mesh | sd_mesh==0),
                                   metier_level_6][1])
  #Second step - if metier is not assigned then try without the info on selection dev.
  #Still ignore >0 metiers
  if(is.na(metier)){
    metier<-as.character(metier.list[!mesh %in% c("0", ">0") & RCG==p.rcg & 
                                       gear==p.gear & 
                                       target==p.target & 
                                       data.table::between(p.mesh,m_size_from,m_size_to),
                                     metier_level_6][1])
  }
  #Third step - if metier is not assigned then try to assign a metier with >0 mesh size range
  if(is.na(metier)){
    metier<-as.character(metier.list[mesh %in% c("0", ">0") & RCG==p.rcg & 
                                       gear==p.gear & 
                                       target==p.target,
                                     metier_level_6][1])
  }
  return(metier)
}
# Function that returns a measure to be used to determine dominant species/group 
getMeasure<-function(p.measure){
  idx<-which(p.measure=="value")
  if(length(idx)>0) return("value")
  else return("weight")
}

# Load the input data
input.data <-data.table(read.csv(data.file,stringsAsFactors = F))
input.data[,EUR:=as.numeric(EUR)]
input.data[,selection_mesh:=tstrsplit(selection,"_")[2]]
if(!"selection_mesh" %in% colnames(input.data)){
  input.data[,selection_mesh:=NA]
}

#Load area list. Can be downloaded from GitHub: https://github.com/ices-eg/RCGs/tree/master/Metiers/Reference_lists
area.list <- data.table(read.csv("https://github.com/ices-eg/RCGs/raw/master/Metiers/Reference_lists/AreaRegionLookup.csv", sep = ",", stringsAsFactors = F))
setnames(area.list, old = c("Code","AreaCode"), new = c("RCG","area"))
area.list <- area.list[,map(.SD,trimws)]
# area 21.0.A is present twice for NA and NSEA. In the next step we merge by area so we must get rid of duplicated areas. Question is how to handle 21.0.A.
area.list<-area.list[!duplicated(area.list$area)]
input.data <- merge(input.data, area.list, all.x = T, by = "area")

# Load species reference list
species.list <- data.table(read.xlsx("https://github.com/ices-eg/RCGs/raw/master/Metiers/Reference_lists/Metier%20Subgroup%20Species%202019%2004.xlsx", sheet = "Species Reference List"))
species.list <- unique(species.list[,.(FAOcode, Grouping.2)])
setnames(species.list, old = c("FAOcode","Grouping.2"), new = c("FAO_species", "species_group"))

#Load new metier file; Can be downloaded from GitHub: https://github.com/ices-eg/RCGs/tree/master/Metiers/Reference_lists
metier.list <- data.table(read.csv("https://github.com/ices-eg/RCGs/raw/master/Metiers/Reference_lists/RDB_ISSG_Metier_list.csv", sep = ",", stringsAsFactors = F, na.strings = ""))
setnames(metier.list, old = "Metier_level6", new = "metier_level_6")

#Split metier by parts
metier.list[,c("gear","target","mesh","sd","sd_mesh") := data.table(str_split_fixed(metier_level_6,"_",5))]
metier.list[str_detect(mesh,"-") == T, c("m_size_from","m_size_to"):=data.table(str_split_fixed(mesh,"-",2))]
metier.list[,":="(m_size_from=as.integer(m_size_from),m_size_to=as.integer(m_size_to),sd=as.integer(sd),sd_mesh=as.integer(sd_mesh))]

metier.list[substr(mesh,1,2) == ">=", ":="(m_size_from=as.integer(gsub("[[:punct:]]", " ",mesh)),
                                           m_size_to=as.integer(999))]
metier.list[substr(mesh,1,1) == ">" & substr(mesh,2,2) != "=", ":="(m_size_from=as.integer(1)+as.integer(gsub("[[:punct:]]", " ",mesh)),
                                                                    m_size_to=as.integer(999))]
metier.list[substr(mesh,1,1) == "<" & substr(mesh,2,2) != "=", ":="(m_size_to=as.integer(gsub("[[:punct:]]", " ",mesh))-as.integer(1),
                                                                    m_size_from=as.integer(1))]
metier.list[mesh == "0", ":="(m_size_from=as.integer(0),
                              m_size_to=as.integer(999))]

#-------Remove >=120 metiers in the Baltic
metier.list<-metier.list[!(RCG == "BS" & mesh == ">=120")]

# Assign species category to the input data
input.data <- merge(input.data, species.list, all.x = T, by = "FAO_species")

#Coutry specific adjustments...


# Determine the dominant species (by weight), its group and percentage in the total catch for each sequence
# (sequence = trip_id+haul_id(if available) +fishing_day+area+ices_rectangle+gear+mesh+selection)
# Information on dominant species can be used to identify the mesh size in case it is missing.
# Function for mesh size determination will be developed. Should the function use national reference lists of mesh sizes corresponding to target species?
#input.data<-input.data[,":="(seq_dom_species_KG = FAO_species[which.max(KG)],
#                               seq_dom_species_group = species_group[which.max(KG)],
#                               seq_dom_species_perc_KG = round(max(KG)/sum(KG)*100,1)),
#            by=.(Country,year,vessel_id,vessel_length,trip_id,haul_id,fishing_day,area,
#                 ices_rectangle,gear,mesh,selection,registered_target_assemblage)][order(vessel_id,trip_id,fishing_day,area,ices_rectangle,gear,mesh)]

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

# Identify sequences where the group of the dominant species differs from the dominant group.
# The results of this step do not affect further calculations. It highlights the above-mentioned situations, that may indicate an input data error.
# input.data[,group.mismatch:=ifelse(seq_dom_species_group!=seq_dom_group,1,0)]


# Assign metier codes
input.data[,metier_level_6:=as.character(pmap(list(RCG, gear, seq_dom_group, mesh, selection_mesh),
                                               function(r,g,t,m,s) getMetier(r,g,t,m,s)))]

# Save results
#input.data<-input.data[order(vessel_id,trip_id,fishing_day,area,ices_rectangle,gear,mesh),
#                       .(Country,year,vessel_id,vessel_length,trip_id,haul_id,fishing_day,area,ices_rectangle,gear,mesh,selection,FAO_species,
#                         registered_target_assemblage,metier_level_6,KG,EUR,species_group,seq_dom_species_KG,seq_dom_species_group,
#                         seq_dom_species_perc_KG,seq_group_KG,seq_group_EUR,seq_dom_group,group.mismatch)]

input.data<-input.data[order(vessel_id,trip_id,fishing_day,area,ices_rectangle,gear,mesh),
                       .(Country,year,vessel_id,vessel_length,trip_id,haul_id,fishing_day,area,ices_rectangle,gear,mesh,selection,FAO_species,
                         registered_target_assemblage,metier_level_6,KG,EUR,species_group,
                         seq_group_KG,seq_group_EUR,seq_dom_group)]
write.csv(input.data,"metier_results.csv")




table(input.data[,.(metier=ifelse(is.na(metier_level_6),0,1))])



