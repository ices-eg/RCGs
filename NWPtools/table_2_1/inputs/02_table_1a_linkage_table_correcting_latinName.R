

# Adapting and correcting scientific names - adaptation to the RDB names
# Kirsten Birch Håkansson, DTU Aqua, Denmark

# The beginning of this script is a really messy - sorry
# The rest is a bit fast and dirty copy / paste coding

# Proper testing is need
# Proper link references are need btweeen 

# 2021-09-24: Nuno Prista
				#fixing of rajidae and sparidae [making use of worms family]; 
				#fixing of some other groups [substring is safer choice to filter partially TAXOCODE than %like%]

################################################################################
# Remember to run "01_table_1a_linkage_table_correcting_latinName.R" before this script
################################################################################

rm(list=ls())
library(dplyr)
library(data.table)
# library(tidyr)
# library(stringr)

path <- "Q:/mynd/RCM/RCGs/NWPtools/table_2_1/"

rfmo_to_test <- c("ICES", "NAFO")

linkage <- read.csv(file.path(path, 'EUMAP_Table_2_1_Linkage_EUROSTAT_RDB and EC_TAC.csv'), sep = ";", header = T)

rdb_spp <- read.csv(paste(path, "rdb_spp.csv", sep = "/"), sep = ";")

ASFIS   <- read.csv(paste0(path,'ASFIS_sp_2020.csv'), header=TRUE, sep=";")

linkage$latinNameASFIS <- gsub("spp.", "spp", linkage$latinName)

linkage <- left_join(linkage, select(ASFIS, Scientific_name, TAXOCODE), by = c("latinNameASFIS" = "Scientific_name"))

nrow(subset(linkage, is.na(TAXOCODE)))

na_TAXO_linkage <- subset(linkage, is.na(TAXOCODE))

ASFIS_for_join <- ASFIS

ASFIS_for_join$Scientific_name <- gsub(" spp", "", ASFIS_for_join$Scientific_name)

rdb_spp  <- left_join(rdb_spp, select(ASFIS_for_join, Scientific_name, TAXOCODE), by = c("Species" = "Scientific_name"))

na_TAXO_rdb_spp <- subset(rdb_spp, is.na(TAXOCODE))

# Check how well the rdb lines up with species in table 1 - these need to be checked

link_spp <- distinct(subset(linkage, RFMO %in% rfmo_to_test), sppName, latinName)

# Adding RDB species ----

names(linkage)

linkage$latinNameJoin <- gsub(" spp.", " spp", linkage$latinName)

# Single species - different naming | misspellings ----

linkage$latinNameJoin[linkage$latinNameJoin == "Scophtalmus maximus"] <- "Scophtalmus maximus,Scophthalmus maximus,Psetta maxima"

linkage$latinNameJoin[linkage$latinNameJoin == "Scophthalmus maximus"] <- "Scophthalmus maximus,Psetta maxima"

linkage$latinNameJoin[linkage$latinNameJoin == "Aspitrigla cuculus"] <- "Aspitrigla cuculus,Chelidonichthys cuculus"

linkage$latinNameJoin[linkage$latinNameJoin == "Trisopterus esmarki"] <- "Trisopterus esmarki,Trisopterus esmarkii"

linkage$latinNameJoin[linkage$latinNameJoin == "Trigla lucerna"] <- "Trigla lucerna,Chelidonichthys lucerna"

linkage$latinNameJoin[linkage$latinNameJoin == "Scymnodon ringenes"] <- "Scymnodon ringenes,Scymnodon ringens"

linkage$latinNameJoin[linkage$latinNameJoin == "Samniosus microcephalus"] <- "Samniosus microcephalus,Somniosus microcephalus"

linkage$latinNameJoin[linkage$latinNameJoin == "Dipturus batis, Dipturis intermedius"] <- "Dipturus batis,Dipturis intermedius,Dipturus intermedius"

# Single species - capros aper ----
linkage$latinNameJoin[linkage$latinNameJoin == "Capros aper"] <- "Capros aper,Caproidae"

# Chlamydoselachus anguineus - kibi: can't see that one in the rdb 
# Etmopterus princeps - kibi: can't see that one in the rdb (only Etmopterus)
# Maja brachydactyla - kibi: can't see that one in the rdb - (only Maja squinado, Macropodia)

# Group of species - adding species, change naming | misspellings ----

## Look up species in groups

ASFIS$TAXOCODE[ASFIS$Scientific_name %like% "Anarhichas"]

## North Sea and Eastern Arctic, ICES ----

spp <- "Mustelus spp"
spps <- unique(c(ASFIS$Scientific_name[substr(ASFIS$TAXOCODE, 1, 8) %like% "10804007"],
          rdb_spp$Species[substr(rdb_spp$TAXOCODE, 1, 8) %like% "10804007"]))
linkage$latinNameJoin[linkage$region == "North Sea and Eastern Arctic" &
                        linkage$RFMO == "ICES" &
                        linkage$latinNameJoin == spp] <-
  paste(spp, paste(spps, collapse = ","), sep = ",")

spp <- "Ammodytidae"
spps <- unique(c(ASFIS$Scientific_name[substr(ASFIS$TAXOCODE, 1, 5) %like% "17204"],
           rdb_spp$Species[substr(rdb_spp$TAXOCODE, 1, 5) %like% "17204"]))
linkage$latinNameJoin[linkage$region == "North Sea and Eastern Arctic" &
                        linkage$RFMO == "ICES" &
                        linkage$latinNameJoin == spp] <-
  paste(spp, paste(spps, collapse = ","), sep = ",")
    
spp <- "Anarhichas spp"
spps <- unique(c(ASFIS$Scientific_name[substr(ASFIS$TAXOCODE, 1, 8) %like% "17102001"],
                  rdb_spp$Species[substr(rdb_spp$TAXOCODE, 1, 8) %like% "17102001"]))
linkage$latinNameJoin[linkage$region == "North Sea and Eastern Arctic" &
                        linkage$RFMO == "ICES" & linkage$latinNameJoin == spp] <-
  paste(spp, paste(spps, collapse = ","), sep = ",")

spp <- "Rajidae"
spps <- unique(c(ASFIS$Scientific_name[substr(ASFIS$TAXOCODE, 1, 5) %like% "11004"],
                rdb_spp$Species[substr(rdb_spp$TAXOCODE, 1, 5) %like% "11004"],
                rdb_spp$Species[rdb_spp$Family %in% "Rajidae"]))

spp_there <- subset(linkage, substr(TAXOCODE, 1, 5) %like% "11004" | sppName %like% "ray" | sppName %like% "skate")
spp_there_1 <- subset(spp_there, region == "North Sea and Eastern Arctic" & RFMO == "ICES" & !latinName %in% c("Rajidae","Dasyatis pastinaca"))
spps_1 <- spps[-which(spps %in% spp_there_1$latinNameJoin)] 

linkage$latinNameJoin[linkage$region == "North Sea and Eastern Arctic" &
                        linkage$RFMO == "ICES" & linkage$latinNameJoin == spp] <-
  paste(spp, paste(spps_1, collapse = ","), sep = ",")

spp <- "Argentina spp"
spps <- unique(c(ASFIS$Scientific_name[ASFIS$TAXOCODE %like% "12305015"],
                  rdb_spp$Species[rdb_spp$TAXOCODE %like% "12305015"]))

spp_there <- subset(linkage, TAXOCODE %like% "12305015")
spp_there_1 <- subset(spp_there, region == "North Sea and Eastern Arctic" & RFMO == "ICES" & latinName != spp)
spps_1 <- spps[-which(spps %in% spp_there_1$latinNameJoin)] 

linkage$latinNameJoin[linkage$region == "North Sea and Eastern Arctic" &
                        linkage$RFMO == "ICES" & linkage$latinNameJoin == spp] <-
  paste(spp, paste(spps_1, collapse = ","), sep = ",")

## North-East Atlantic, ICES ----

ASFIS$TAXOCODE[ASFIS$Scientific_name %like% "Trisopterus"]

spp <- "Ammodytidae"
spps <- unique(c(ASFIS$Scientific_name[substr(ASFIS$TAXOCODE, 1, 5) %like% "17204"],
           rdb_spp$Species[substr(rdb_spp$TAXOCODE, 1, 5) %like% "17204"]))
linkage$latinNameJoin[linkage$region == "North-East Atlantic" &
                        linkage$RFMO == "ICES" &
                        linkage$latinNameJoin == spp] <-
  paste(spp, paste(spps, collapse = ","), sep = ",")

spp <- "Apristurus spp"
spps <- unique(c(ASFIS$Scientific_name[ASFIS$TAXOCODE %like% "10801014"],
                  rdb_spp$Species[rdb_spp$TAXOCODE %like% "10801014"]))
linkage$latinNameJoin[linkage$region == "North-East Atlantic" &
                        linkage$RFMO == "ICES" &
                        linkage$latinNameJoin == spp] <-
  paste(spp, paste(spps, collapse = ","), sep = ",")

spp <- "Beryx spp"
spps <- unique(c(ASFIS$Scientific_name[ASFIS$TAXOCODE %like% "16102003"],
                  rdb_spp$Species[rdb_spp$TAXOCODE %like% "16102003"]))
linkage$latinNameJoin[linkage$region == "North-East Atlantic" &
                        linkage$RFMO == "ICES" &
                        linkage$latinNameJoin == spp] <-
  paste(spp, paste(spps, collapse = ","), sep = ",")

spp <- "Centrophorus spp"
spps <- unique(c(ASFIS$Scientific_name[substr(ASFIS$TAXOCODE, 1, 8) %like% "10901008"],
                  rdb_spp$Species[substr(rdb_spp$TAXOCODE, 1, 8) %like% "10901008"]))
linkage$latinNameJoin[linkage$region == "North-East Atlantic" &
                        linkage$RFMO == "ICES" &
                        linkage$latinNameJoin == spp] <-
  paste(spp, paste(spps, collapse = ","), sep = ",")

spp <- "Mustelus spp"
spps <- unique(c(ASFIS$Scientific_name[ASFIS$TAXOCODE %like% "10804007"],
                 rdb_spp$Species[rdb_spp$TAXOCODE %like% "10804007"]))
linkage$latinNameJoin[linkage$region == "North-East Atlantic" &
                        linkage$RFMO == "ICES" &
                        linkage$latinNameJoin == spp] <-
  paste(spp, paste(spps, collapse = ","), sep = ",")

spp <- "Pandalus spp"
spps <- unique(c(ASFIS$Scientific_name[ASFIS$TAXOCODE %like% "22804002"],
                 rdb_spp$Species[rdb_spp$TAXOCODE %like% "22804002"]))

spp_there <- subset(linkage, TAXOCODE %like% "22804002")
spp_there_1 <- subset(spp_there, region == "North-East Atlantic" & RFMO == "ICES" & latinName != spp)
spps_1 <- spps[-which(spps %in% spp_there_1$latinNameJoin)] 

linkage$latinNameJoin[linkage$region == "North-East Atlantic" &
                        linkage$RFMO == "ICES" & linkage$latinNameJoin == spp] <-
  paste(spp, paste(spps_1, collapse = ","), sep = ",")

spp <- "Sparidae"
spps <- unique(c(ASFIS$Scientific_name[substr(ASFIS$TAXOCODE, 1, 5) %like% "17039"],
                 rdb_spp$Species[substr(rdb_spp$TAXOCODE, 1, 5) %like% "17039"]),
				  rdb_spp$Species[rdb_spp$Family %in% "Sparidae"])

spp_there <- subset(linkage, TAXOCODE %like% "17039")
spp_there_1 <- subset(spp_there, region == "North-East Atlantic" & RFMO == "ICES" & latinName != spp)
spps_1 <- spps[-which(spps %in% spp_there_1$latinNameJoin)] 

linkage$latinNameJoin[linkage$region == "North-East Atlantic" &
                        linkage$RFMO == "ICES" & linkage$latinNameJoin == spp] <-
  paste(spp, paste(spps_1, collapse = ","), sep = ",")

spp <- "Trisopterus spp"
spps <- unique(c(ASFIS$Scientific_name[ASFIS$TAXOCODE %like% "14804032"],
                 rdb_spp$Species[rdb_spp$TAXOCODE %like% "14804032"]))
linkage$latinNameJoin[linkage$region == "North-East Atlantic" &
                        linkage$RFMO == "ICES" &
                        linkage$latinNameJoin == spp] <-
  paste(spp, paste(spps, collapse = ","), sep = ",")

## Other regions, NAFO ----

ASFIS$TAXOCODE[ASFIS$Scientific_name %like% "Sebastes"]

spp <- "Apristurus spp"
spps <- unique(c(ASFIS$Scientific_name[ASFIS$TAXOCODE %like% "10801014"],
                 rdb_spp$Species[rdb_spp$TAXOCODE %like% "10801014"]))
linkage$latinNameJoin[linkage$region == "Other regions" &
                        linkage$RFMO == "NAFO" &
                        linkage$latinNameJoin == spp] <-
  paste(spp, paste(spps, collapse = ","), sep = ",")

spp <- "Beryx spp"
spps <- unique(c(ASFIS$Scientific_name[ASFIS$TAXOCODE %like% "16102003"],
                 rdb_spp$Species[rdb_spp$TAXOCODE %like% "16102003"]))
linkage$latinNameJoin[linkage$region == "Other regions" &
                        linkage$RFMO == "NAFO" &
                        linkage$latinNameJoin == spp] <-
  paste(spp, paste(spps, collapse = ","), sep = ",")

spp <- "Centrophorus spp"
spps <- unique(c(ASFIS$Scientific_name[substr(ASFIS$TAXOCODE, 1, 8) %like% "10901008"],
                 rdb_spp$Species[substr(rdb_spp$TAXOCODE, 1, 8) %like% "10901008"]))
linkage$latinNameJoin[linkage$region == "Other regions" &
                        linkage$RFMO == "NAFO" &
                        linkage$latinNameJoin == spp] <-
  paste(spp, paste(spps, collapse = ","), sep = ",")

spp <- "Sebastes spp"
spps <- unique(c(ASFIS$Scientific_name[ASFIS$TAXOCODE %like% "17801001"],
                 rdb_spp$Species[rdb_spp$TAXOCODE %like% "17801001"]))
linkage$latinNameJoin[linkage$region == "Other regions" &
                        linkage$RFMO == "NAFO" &
                        linkage$latinNameJoin == spp] <-
  paste(spp, paste(spps, collapse = ","), sep = ",")


# Output ----

write.table(linkage, paste0(path, "EUMAP_Table_2_1_Linkage_EUROSTAT_RDB and EC_TAC.csv"), 
          row.names = F, sep = ";")
