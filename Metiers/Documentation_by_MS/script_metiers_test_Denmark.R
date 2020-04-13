library(stringr)
library(data.table)
library(readxl)
library(purrr)
library(dplyr) #DNK

#DNK paths
data.path <- "Q:\\dfad\\users\\joeg\\home\\LOG\\190430_Metier_script_test\\"
git.path <- "C:\\joeg\\Github\\RCGs\\Metiers\\"
metier.path <- "C:\\joeg\\Metier work\\RCG_intersessional_work_2020\\Metier_test\\"
data.file <- "Metier_input_DNK_2018.csv"

year <- 2018

# Function that finds a metier code based on the given parameters
getMetier<-function(p.rcg, p.gear, p.target, p.mesh, p.selection){
  # First approach, assign metier by RCG, gear, target assemblage, mesh size and meash size in selection device
  metier<-as.character(metier.list[RCG==p.rcg & 
                                     gear==p.gear & 
                                     target==p.target & 
                                     (data.table::between(p.mesh,m_size_from,m_size_to) | (m_size_from <=1 & m_size_to==999)) &
                                     (p.selection == sd_mesh | sd_mesh==0),
                                   metier_level_6][1])
  # Second approach, without selection device
  if(is.na(metier)){
    metier<-as.character(metier.list[RCG==p.rcg & 
                                       gear==p.gear & 
                                       target==p.target & 
                                       (data.table::between(p.mesh,m_size_from,m_size_to) | (m_size_from <=1 & m_size_to==999)),
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
#DNK read input data
input.data <-data.table(read.csv(paste0(data.path,data.file),stringsAsFactors = F))
#DNK make sure that mesh is numeric
input.data$mesh <- as.numeric(input.data$mesh)
input.data[,EUR:=as.numeric(EUR)]
input.data[,selection_mesh:=tstrsplit(selection,"_")[2]]
if(!"selection_mesh" %in% colnames(input.data)){
  input.data[,selection_mesh:=NA]
}
#DNK gear/mesh corrections:
input.data$gear[input.data$gear=="GN"] <- "GNS"
input.data$gear[input.data$gear=="TB"] <- "OTB"
input.data$gear[input.data$gear %like% "TB"] <- "OTB"
input.data$mesh[input.data$gear=="DRB" & input.data$mesh==999] <- 120
input.data$mesh[input.data$mesh==999] <- 998

#Load area list. Can be downloaded from GitHub: https://github.com/ices-eg/RCGs/tree/master/Metiers/Reference_lists
area.list <- data.table(read.csv(paste(metier.path,"AreaRegionLookup.csv",sep = ""), sep = ",", stringsAsFactors = F))
setnames(area.list, old = c("Code","AreaCode"), new = c("RCG","area"))
area.list <- area.list[,map(.SD,trimws)]

#DNK change RCG NA to NAtl
area.list$RCG[area.list$RCG=="NA"] <- "NAtl"

# area 21.0.A is present twice for NA and NSEA. In the next step we merge by area so we must get rid of duplicated areas. Question is how to handle 21.0.A.
area.list<-area.list[!duplicated(area.list$area)]
input.data <- merge(input.data, area.list, all.x = T, by = "area")

# Load species reference list
species.list <- data.table(read_excel(paste(git.path,"Reference_lists\\Metier Subgroup Species 2019 04.xlsx",sep = ""), sheet = "Species Reference List"))
species.list <- unique(species.list[,.(FAOcode,`Grouping 2`)])
setnames(species.list, old = c("FAOcode","Grouping 2"), new = c("FAO_species", "species_group"))

#DNK: In Denmark Starfish is part of the Molluscs fishery
species.list$species_group[species.list$FAO_species=="STH"] <- "MOL"

#Load new metier file; Can be downloaded from GitHub: https://github.com/ices-eg/RCGs/tree/master/Metiers/Reference_lists
metier.list <- data.table(read.csv(paste(metier.path,"RDB_ISSG_Metier_list.csv",sep = ""), sep = ",", stringsAsFactors = F))
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
#-------Remove metier codes that are not assigned to any RCG
#DNK: Recoding North Atlantic from NA to NAtl
metier.list$RCG[is.na(metier.list$RCG)] <- "NAtl"

#DNK Corrected the code
#-------Invalid metier code GNS_DEF>0_0_0
#metier.list<-metier.list[metier_level_6 != "GNS_DEF>0_0_0"]
#-------Remove >=120 metiers in the Baltic

#DNK: Need to specific recode if the >=120 metier is used.
metier.list<-metier.list[!(RCG == "BS" & mesh == ">=120")]
#Extract metier lvl5
metier.list[,metier_level_5 := paste(gear,target,sep="_")]
#Count the number of lvl5 occurencies by RCG
metier.list[,lvl5_n:=.N, by=.(RCG, metier_level_5)]
#Remove metiers with mesh >0 if there other mesh size ranges available for a given RCG and the same metier lvl 5
metier.list<-metier.list[!(lvl5_n>1 & mesh == ">0")]

#DNK: Remove metiers with end_year smaller than current year
metier.list$End_year[is.na(metier.list$End_year)] <- 2030
metier.list<-metier.list[!(End_year<year)]

# Assign species category to the input data
input.data <- merge(input.data, species.list, all.x = T, by = "FAO_species")

#Coutry specific adjustments...
#DNK: in Skagerrak and Kattegat for OTB/PTB/OTT DEF/CRU 90-119 assign to MCD
input.data$species_group[input.data$area %in% c("27.3.a.20", "27.3.a.21") & (input.data$mesh>=90 & input.data$mesh<=119) & input.data$gear %in% c("OTB","PTB","OTT")] <- "MCD"

# Determine the dominant species (by weight), its group and percentage in the total catch for each sequence
# (sequence = trip_id+haul_id(if available) +fishing_day+area+ices_rectangle+gear+mesh+selection)
# Information on dominant species can be used to identify the mesh size in case it is missing.
# Function for mesh size determination will be developed. Should the function use national reference lists of mesh sizes corresponding to target species?
input.data<-input.data[,":="(seq_dom_species_KG = FAO_species[which.max(KG)],
                               seq_dom_species_group = species_group[which.max(KG)],
                               seq_dom_species_perc_KG = round(max(KG)/sum(KG)*100,1)),
            by=.(Country,year,vessel_id,vessel_length,trip_id,haul_id,fishing_day,area,
                 ices_rectangle,gear,mesh,selection,registered_target_assemblage)][order(vessel_id,trip_id,fishing_day,area,ices_rectangle,gear,mesh)]


# Calculate group totals for each sequence
input.data[,":="(seq_group_KG = sum(KG, na.rm = T),
                 seq_group_EUR = sum(EUR, na.rm = T)),
            by=.(Country,year,vessel_id,vessel_length,trip_id,haul_id,fishing_day,area,
                 ices_rectangle,gear,mesh,selection,registered_target_assemblage,species_group)]

# Select a measure to determine the dominant group at a sequence level. If at least one species in a sequence has "value" in a measure column then 
# all species in that sequence get the same measure.
input.data[,":="(seq_measure = getMeasure(measure)),
           by=.(Country,year,vessel_id,vessel_length,trip_id,haul_id,fishing_day,area,
                ices_rectangle,gear,mesh,selection,registered_target_assemblage)]


# Determine the dominant group for each sequence
input.data[seq_measure == "weight",":="(seq_dom_group = species_group[which.max(seq_group_KG)]),
                         by=.(Country,year,vessel_id,vessel_length,trip_id,haul_id,fishing_day,area,
                              ices_rectangle,gear,mesh,selection,registered_target_assemblage)]
input.data[seq_measure == "value",":="(seq_dom_group = species_group[which.max(seq_group_EUR)]),
           by=.(Country,year,vessel_id,vessel_length,trip_id,haul_id,fishing_day,area,
                ices_rectangle,gear,mesh,selection,registered_target_assemblage)]

# Identify sequences where the group of the dominant species differs from the dominant group.
# The results of this step do not affect further calculations. It highlights the above-mentioned situations, that may indicate an input data error.
input.data[,group.mismatch:=ifelse(seq_dom_species_group!=seq_dom_group,1,0)]

#Where dominant species group is CAT Denmark assumes that the gear is FPN
input.data$gear[input.data$seq_dom_group=="CAT" & (input.data$gear %in% c("MIS","NK","GNS"))] <- "FPN"
#For line fisheries the species group is set to FIF
input.data$seq_dom_group[input.data$gear %in% c("LLS","LLD","LHP")] <- "FIF"
input.data$seq_dom_group[input.data$seq_dom_group=="CEP"] <- "DEF"

# Assign metier codes
input.data[,metier_level_6:=as.character(pmap(list(RCG, gear, seq_dom_group, mesh, selection_mesh),
                                               function(r,g,t,m,s) getMetier(r,g,t,m,s)))]

# Save results
input.data2<-input.data[order(vessel_id,trip_id,fishing_day,area,ices_rectangle,gear,mesh),
                       .(Country,year,vessel_id,vessel_length,trip_id,haul_id,fishing_day,area,ices_rectangle,gear,mesh,selection,FAO_species,
                         registered_target_assemblage,metier_level_6,KG,EUR,species_group,seq_dom_species_KG,seq_dom_species_group,
                         seq_dom_species_perc_KG,seq_group_KG,seq_group_EUR,seq_dom_group,group.mismatch)]
write.csv(input.data2,"metier_results.csv")

#DNK test 
table(input.data2[,.(metier=ifelse(is.na(metier_level_6),0,1))])

input.data$check[input.data$metier_level_6 == input.data$metier_level_6_DNK] <- "equal"
input.data$check[input.data$metier_level_6 != input.data$metier_level_6_DNK] <- "not equal"
input.data$n <- 1
tjek <- input.data %>%
  group_by(RCG, check, metier_level_6, metier_level_6_DNK) %>%
  summarise(n_count=sum(n))

tjek.metier.data <- input.data[input.data$metier_level_6 == "OTB_MCD_90-99_0_0" & input.data$RCG=="NSEA",] 
#tjek.metier.data$gear[tjek.metier.data$seq_dom_group=="CAT" & (tjek.metier.data$gear %in% c("MIS","NK") || is.na(tjek.metier.data$gear))] <- "FPN"
#sth <- input.data[input.data$FAO_species=="STH"]
