

# creating codes references from the RDB
# Kirsten Birch Håkansson, DTU Aqua, Denmark

library(dplyr)

path <- "Q:/mynd/RCM/RCGs/NWPtools/table_2_1/"

load(paste(path,'RDB_All_Regions_CL_2009_2020_prepared_20210428.Rdata' ,sep = ''))

# Areas ----

rdb_areas <- unique(cl$Area)

write.table(rdb_areas, paste0(path, "rdb_area_ref.csv"), sep = ";", row.names = F)

# Countries ----

rdb_ctry <- as.character(unique(cl$FlagCountry))

write.table(rdb_ctry, paste0(path, "rdb_ctry_ref.csv"), sep = ";", row.names = F)

# Countries ----

rdb_spp <- dplyr::distinct(cl, SpeciesAphiaID, Species, SpeciesDesc)

write.table(rdb_spp, paste0(path, "rdb_spp.csv"), sep = ";", row.names = F)
