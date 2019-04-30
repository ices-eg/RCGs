library(stringr)
library(data.table)
library(readxl)
library(purrr)

data.path <- "C:/GitHub/RCGs/Metiers/Scripts/"
data.file <- "Metier_data_format_Example_test.csv"


# Function that finds a metier code based on the given parameters
getMetier<-function(p.gear, p.target, p.area, p.mesh, p.selection){
  metier<-as.character(metier.list.baltic[gear==p.gear &
                                            target==p.target &
                                            AreaCode==p.area &
                                            (data.table::between(p.mesh,m_size_from,m_size_to) | (m_size_from <=1 & m_size_to==999)) &
                                            (p.selection == sd_mesh | sd_mesh==0),
                                metier_level_6][1])
  return(metier)
}
# Function that returns a measure to be used to determine dominant species/group 
getMeasure<-function(p.measure){
  idx<-which(p.measure=="value")
  if(length(idx)>0) return("value")
  else return("weight")
}

# Load the input data
input.data <-data.table(read.csv(paste0(data.path,data.file),stringsAsFactors = F))


# Load species reference list
species.list <- data.table(read_excel(paste(data.path,"Species SG2 Report.xlsx",sep = ""), sheet = "Species Reference List"))
species.list <- unique(species.list[,.(FAOcode,`Grouping 2`)])
setnames(species.list, old = c("FAOcode","Grouping 2"), new = c("FAO_species", "species_group"))

#Load metier file from ICES http://ices.dk/marine-data/Documents/RDB/RDB%20Metiers%20in%20allowed%20areas.zip
metier.list <- data.table(read.csv(paste(data.path,"RDB Metiers in allowed areas.csv",sep = ""), sep = "\t", stringsAsFactors = F))
setnames(metier.list, old = "FishingActivityCategoryEuropeanLvl6Code", new = "metier_level_6")


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

#Filtering out only Baltic metiers
metier.list.baltic <- metier.list[substr(AreaCode,1,5) == "27.3."]
input.data <- input.data[substr(area,1,5) == "27.3."]

# Assign species category to the input data
input.data <- merge(input.data, species.list, all.x = T, by = "FAO_species")



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


# Assign metier codes
input.data[,metier_level_6:=as.character(pmap(list(gear, seq_dom_group, area, mesh, selection),
                                               function(g,t,a,m,s) getMetier(g,t,a,m,s)))]

# Save results
input.data<-input.data[order(vessel_id,trip_id,fishing_day,area,ices_rectangle,gear,mesh),
                       .(Country,year,vessel_id,vessel_length,trip_id,haul_id,fishing_day,area,ices_rectangle,gear,mesh,selection,FAO_species,
                         registered_target_assemblage,metier_level_6,KG,EUR,species_group,seq_dom_species_KG,seq_dom_species_group,
                         seq_dom_species_perc_KG,seq_group_KG,seq_group_EUR,seq_dom_group,group.mismatch)]
write.csv(input.data,paste0(data.path,"metier_results.csv"))





table(input.data[,.(metier=ifelse(is.na(metier_level_6),0,1))])



