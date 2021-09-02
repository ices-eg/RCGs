

# Adapting and correcting scientific names - adaptions to the RDB names

library(dplyr)
library(data.table)
# library(tidyr)
# library(stringr)

path <- "Q:/mynd/RCM/RCGs/NWPtools/table_2_1/"

rfmo_to_test <- c("ICES", "NAFO", "CACAF", "SPRFMO")

linkage <- read.csv(file.path(path, 'EUMAP_Table_2_1_Linkage_EUROSTAT and EC_TAC_version_2021_final_v01.csv'), sep = ";", header = T)

rdb_spp <- read.csv(paste(path, "rdb_spp.csv", sep = "/"), sep = ";")

ASFIS   <- read.csv(paste0(path,'ASFIS_sp_2020.csv'), header=TRUE, sep=";")

linkage$latinName <- gsub("spp.", "spp", linkage$latinName)

linkage <- left_join(linkage, select(ASFIS, Scientific_name, TAXOCODE), by = c("latinName" = "Scientific_name"))

nrow(subset(linkage, is.na(TAXOCODE)))

na_TAXO <- subset(linkage, is.na(TAXOCODE))

# Check how well the rdb lines up with species in table 1 and ASFIS

link_spp <- distinct(subset(linkage, RFMO %in% rfmo_to_test), sppName, latinName)

link_rdb_spp <- subset(full_join(link_spp, rdb_spp, by = c("latinName" = "Species"), keep = T), is.na(Species))

# Adding RDB speciess ----

names(linkage)

linkage$latinNameJoin <- linkage$latinName

# Single species - different naming | misspellings ----

linkage$latinNameJoin[linkage$latinNameJoin == "Scophtalmus maximus"] <- "Scophtalmus maximus,Scophthalmus maximus"

linkage$latinNameJoin[linkage$latinNameJoin == "Aspitrigla cuculus"] <- "Aspitrigla cuculus,Chelidonichthys cuculus"

linkage$latinNameJoin[linkage$latinNameJoin == "Trisopterus esmarki"] <- "Trisopterus esmarki,Trisopterus esmarkii"

linkage$latinNameJoin[linkage$latinNameJoin == "Trigla lucerna"] <- "Trigla lucerna,Chelidonichthys lucerna"

linkage$latinNameJoin[linkage$latinNameJoin == "Scymnodon ringenes"] <- "Scymnodon ringenes,Scymnodon ringens"

linkage$latinNameJoin[linkage$latinNameJoin == "Samniosus microcephalus"] <- "Samniosus microcephalus,Somniosus microcephalus"

linkage$latinNameJoin[linkage$latinNameJoin == "Dipturus batis, Dipturis intermedius"] <- "Dipturus batis, Dipturis intermedius,Dipturus intermedius"


# Chlamydoselachus anguineus - kibi: can't see that one in the rdb 
# Etmopterus princeps - kibi: can't see that one in the rdb (only Etmopterus)
# Maja brachydactyla - kibi: can't see that one in the rdb - (only Maja squinado, Macropodia)

# Group of species - adding species, change naming | misspellings ----

## Look up species in groups

ASFIS$TAXOCODE[ASFIS$Scientific_name %like% "Rajidae"]

## Add

spp <- "Mustelus spp."
linkage$latinNameJoin[linkage$region == "North Sea and Eastern Arctic" &
                        linkage$RFMO == "ICES" & linkage$latinNameJoin == spp] <-
  paste(spp,
        paste(ASFIS$Scientific_name[ASFIS$TAXOCODE %like% "10804007"], collapse = ","),
        sep = ",")

spp1 <- "Ammodytidae"
    linkage$latinNameJoin[linkage$region == "North Sea and Eastern Arctic" &
                            linkage$RFMO == "ICES" & linkage$latinNameJoin == spp1] <-
    paste(spp1,
          paste(ASFIS$Scientific_name[ASFIS$TAXOCODE %like% "17204"], collapse = ","),
          sep = ",")
    
spp2 <- "Anarhichas spp"
linkage$latinNameJoin[linkage$region == "North Sea and Eastern Arctic" &
                        linkage$RFMO == "ICES" & linkage$latinNameJoin == spp2] <-
  paste(spp2,
        paste(ASFIS$Scientific_name[ASFIS$TAXOCODE %like% "17102001"], collapse = ","),
        sep = ",")

spp3 <- "Rajidae"
spp3_1 <- ASFIS$Scientific_name[ASFIS$TAXOCODE %like% "11004"]
spp3_there <- subset(linkage, TAXOCODE %like% "11004" | sppName %like% "ray" | sppName %like% "skate")
spp3_there_1 <- subset(spp3_there, region == "North Sea and Eastern Arctic" & RFMO == "ICES" & latinName != spp3)
spp3_2 <- spp3_1[-which(spp3_1 %in% raj_there_1$latinNameJoin)] 

linkage$latinNameJoin[linkage$region == "North Sea and Eastern Arctic" &
                        linkage$RFMO == "ICES" & linkage$latinNameJoin == spp3] <-
  paste(spp3,
        paste(spp3_2, collapse = ","),
        sep = ",")




write.table(linkage, paste0(path, "EUMAP_Table_2_1_Linkage_EUROSTAT and EC_TAC_version_2021_final_v02.csv"), 
          row.names = F, sep = ";")
