

# Adapting and correcting scientific names - adaptions to the RDB names

# library(dplyr)
# library(tidyr)
# library(stringr)

path <- "Q:/mynd/RCM/RCGs/NWPtools/table_2_1/"


linkage <- read.csv(file.path(path, 'EUMAP_Table_2_1_Linkage_EUROSTAT and EC_TAC_version_2021_final_v01.csv'), sep = ";", header = T)

names(linkage)

linkage$latinNameJoin <- linkage$latinName

linkage$latinNameJoin[linkage$latinNameJoin == "Scophtalmus maximus"] <- "Scophthalmus maximus"

linkage$latinNameJoin[linkage$latinNameJoin == "Mustelus spp."] <- "Mustelus,Mustelus mustelus,Mustelus asterias,Mustelus punctulatus"

linkage$latinNameJoin[linkage$latinNameJoin == "Ammodytidae"] <- "Ammodytidae,Ammodytes,Ammodytes tobianus,Ammodytes marinus,Ammodytes spp"

linkage$latinNameJoin[linkage$latinNameJoin == "Anarhichas spp"] <- "Anarhichas,Anarhichas spp,Anarhichas minor,Anarhichas denticulatus,Anarhichas lupus"

linkage$latinNameJoin[linkage$latinNameJoin == "Argentina spp"] <- "Argentina spp,Argentina spp.,Argentina,Argentina sphyraena,Argentina kagoshimae,Argentina elongata"

linkage$latinNameJoin[linkage$latinNameJoin == "Aspitrigla cuculus"] <- "Chelidonichthys cuculus,Aspitrigla cuculus"

linkage$latinNameJoin[linkage$latinNameJoin == "Trisopterus esmarki"] <- "Trisopterus esmarkii"

linkage$latinNameJoin[linkage$latinNameJoin == "Trigla lucerna"] <- "Trigla lucerna,Chelidonichthys lucerna"

linkage$latinNameJoin[linkage$latinNameJoin == "Rajidae"] <- "Raja" #Mangler alle de andre




write.table(linkage, paste0(path, "EUMAP_Table_2_1_Linkage_EUROSTAT and EC_TAC_version_2021_final_v02.csv"), 
          row.names = F, sep = ";")
