

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

linkage$latinNameJoin[linkage$latinNameJoin == "Dipturus batis, Dipturis intermedius"] <- "Dipturus batis,Dipturis intermedius,Dipturus intermedius"


# Chlamydoselachus anguineus - kibi: can't see that one in the rdb 
# Etmopterus princeps - kibi: can't see that one in the rdb (only Etmopterus)
# Maja brachydactyla - kibi: can't see that one in the rdb - (only Maja squinado, Macropodia)

# Group of species - adding species, change naming | misspellings ----

## Look up species in groups

ASFIS$TAXOCODE[ASFIS$Scientific_name %like% "Anarhichas"]

## North Sea and Eastern Arctic, ICES ----

spp <- "Mustelus spp."
spps <- unique(c(ASFIS$Scientific_name[ASFIS$TAXOCODE %like% "10804007"],
          rdb_spp$Species[rdb_spp$TAXOCODE %like% "10804007"]))
linkage$latinNameJoin[linkage$region == "North Sea and Eastern Arctic" &
                        linkage$RFMO == "ICES" &
                        linkage$latinNameJoin == spp] <-
  paste(spp, paste(spps, collapse = ","), sep = ",")

spp1 <- "Ammodytidae"
spp1s <- unique(c(ASFIS$Scientific_name[ASFIS$TAXOCODE %like% "17204"],
           rdb_spp$Species[rdb_spp$TAXOCODE %like% "17204"]))
linkage$latinNameJoin[linkage$region == "North Sea and Eastern Arctic" &
                        linkage$RFMO == "ICES" &
                        linkage$latinNameJoin == spp1] <-
  paste(spp1, paste(spp1s, collapse = ","), sep = ",")
    
spp2 <- "Anarhichas spp"
spp2s <- unique(c(ASFIS$Scientific_name[ASFIS$TAXOCODE %like% "17102001"],
                  rdb_spp$Species[rdb_spp$TAXOCODE %like% "17102001"]))
linkage$latinNameJoin[linkage$region == "North Sea and Eastern Arctic" &
                        linkage$RFMO == "ICES" & linkage$latinNameJoin == spp2] <-
  paste(spp2, paste(spp2s, collapse = ","), sep = ",")

spp3 <- "Rajidae"
spp3s <- unique(c(ASFIS$Scientific_name[ASFIS$TAXOCODE %like% "11004"],
                rdb_spp$Species[rdb_spp$TAXOCODE %like% "11004"],
                rdb_spp$Species[rdb_spp$SpeciesDesc %like% "ray" | rdb_spp$SpeciesDesc %like% "skate"]))

spp3_there <- subset(linkage, TAXOCODE %like% "11004" | sppName %like% "ray" | sppName %like% "skate")
spp3_there_1 <- subset(spp3_there, region == "North Sea and Eastern Arctic" & RFMO == "ICES" & latinName != spp3)
spp3s_1 <- spp3s[-which(spp3s %in% spp3_there_1$latinNameJoin)] 

linkage$latinNameJoin[linkage$region == "North Sea and Eastern Arctic" &
                        linkage$RFMO == "ICES" & linkage$latinNameJoin == spp3] <-
  paste(spp3, paste(spp3s_1, collapse = ","), sep = ",")

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

ASFIS$TAXOCODE[ASFIS$Scientific_name %like% "Argentina"]

spp4 <- "Ammodytidae"
spp4s <- unique(c(ASFIS$Scientific_name[ASFIS$TAXOCODE %like% "17204"],
                  rdb_spp$Species[rdb_spp$TAXOCODE %like% "17204"]))
linkage$latinNameJoin[linkage$region == "North-East Atlantic" &
                        linkage$RFMO == "ICES" &
                        linkage$latinNameJoin == spp4] <-
  paste(spp1, paste(spp4s, collapse = ","), sep = ",")

spp5 <- "Apristurus spp."
spp5s <- unique(c(ASFIS$Scientific_name[ASFIS$TAXOCODE %like% "10801014"],
                  rdb_spp$Species[rdb_spp$TAXOCODE %like% "10801014"]))
linkage$latinNameJoin[linkage$region == "North-East Atlantic" &
                        linkage$RFMO == "ICES" &
                        linkage$latinNameJoin == spp5] <-
  paste(spp1, paste(spp5s, collapse = ","), sep = ",")

spp6 <- "Beryx spp."
spp6s <- unique(c(ASFIS$Scientific_name[ASFIS$TAXOCODE %like% "16102003"],
                  rdb_spp$Species[rdb_spp$TAXOCODE %like% "16102003"]))
linkage$latinNameJoin[linkage$region == "North-East Atlantic" &
                        linkage$RFMO == "ICES" &
                        linkage$latinNameJoin == spp6] <-
  paste(spp1, paste(spp6s, collapse = ","), sep = ",")

spp7 <- "Centrophorus spp."
spp7s <- unique(c(ASFIS$Scientific_name[ASFIS$TAXOCODE %like% "10901008"],
                  rdb_spp$Species[rdb_spp$TAXOCODE %like% "10901008"]))
linkage$latinNameJoin[linkage$region == "North-East Atlantic" &
                        linkage$RFMO == "ICES" &
                        linkage$latinNameJoin == spp7] <-
  paste(spp1, paste(spp7s, collapse = ","), sep = ",")

spp <- "Mustelus spp."
spps <- unique(c(ASFIS$Scientific_name[ASFIS$TAXOCODE %like% "10804007"],
                 rdb_spp$Species[rdb_spp$TAXOCODE %like% "10804007"]))
linkage$latinNameJoin[linkage$region == "North-East Atlantic" &
                        linkage$RFMO == "ICES" &
                        linkage$latinNameJoin == spp] <-
  paste(spp, paste(spps, collapse = ","), sep = ",")




# Output ----

write.table(linkage, paste0(path, "EUMAP_Table_2_1_Linkage_EUROSTAT and EC_TAC_version_2021_final_v02.csv"), 
          row.names = F, sep = ";")
