

# Adapting areas in the linkage table to the RDB areas / structure

library(dplyr)
library(tidyr)
library(stringr)

input_path <- "Q:/mynd/RCM/RCGs/NWPtools/table_2_1/NWP table 2.1/"
output_path <- "Q:/mynd/RCM/RCGs/NWPtools/table_2_1/"


linkage <- read.csv(file.path(input_path, 'EUMAP_Table_2_1_Linkage_EUROSTAT and EC_TAC_version_2021_final.csv'), sep = ";", header = T)

names(linkage)

linkage$areaRDB <- gsub(" ", "", linkage$areaBis)

check_1 <- subset(linkage, areaBis != areaRDB)

# Baltic Sea / Baltic Sea (ICES areas 3b-d, FAO area 27)

linkage$areaRDB[linkage$areaRDB == "27_3_D"] <-  "27_3_D,27_3_D_24,27_3_D_25,27_3_D_26,27_3_D_27,27_3_D_28,27_3_D_29,27_3_D_30,27_3_D_31,27_3_D_32"

linkage$areaRDB <- gsub("27_3_D_28,", "27_3_D_28,27_3_D_28_1,27_3_D_28_2,", linkage$areaRDB)

check_2 <- subset(linkage, areaBis != areaRDB)

# North Sea and Eastern Arctic / Eastern Arctic, Norwegian Sea and Barents Sea (ICES areas 1, 2, FAO area 27)

linkage$areaRDB[linkage$areaRDB == "27_1,27_2"] <- "27_1,27_1_A,27_1_B,27_2,27_2_A,27_2_A_1,27_2_A_2,27_2_B,27_2_B_1,27_2_B_2"

linkage$areaRDB[linkage$areaRDB == "27_1,27_2,27_5_A,27_14"] <- "27_1,27_1_A,27_1_B,27_2,27_2_A,27_2_A_1,27_2_A_2,27_2_B,27_2_B_1,27_2_B_2,27_5_A,27_14,27_14_A,27_14_B,27_14_B_1,27_14_B_2"

linkage$areaRDB[linkage$areaRDB == "27_1,27_2,27_14"] <- "27_1,27_1_A,27_1_B,27_2,27_2_A,27_2_A_1,27_2_A_2,27_2_B,27_2_B_1,27_2_B_2,27_14,27_14_A,27_14_B,27_14_B_1,27_14_B_2"

linkage$areaRDB[linkage$areaRDB == "27_2"] <- "27_2,27_2_A,27_2_A_1,27_2_A_2,27_2_B,27_2_B_1,27_2_B_2"

linkage$areaRDB[linkage$areaRDB == "27_2_A"] <- "27_2_A,27_2_A_1,27_2_A_2"


linkage$areaRDB[linkage$areaRDB == "27_4,27_7_D" &
                  linkage$latinName == "Squalus acanthias"] <-
  "27_1,27_1_A,27_1_B,27_2,27_2_A,27_2_A_1,27_2_A_2,27_2_B,27_2_B_1,27_2_B_2" # ERROR

check_3 <- subset(linkage, areaBis != areaRDB)

# North Sea and Eastern Arctic / North Sea and Eastern Channel (ICES areas 3a, 4 and 7d, FAO area 27)

linkage$areaRDB[linkage$areaRDB == "27_3_A,27_4"] <- "27_3_A,27_3_A_20,27_3_A_21,27_4,27_4_A,27_4_B,27_4_C"

linkage$areaRDB[linkage$areaRDB == "27_4"] <- "27_4,27_4_A,27_4_B,27_4_C"

linkage$areaRDB[linkage$areaRDB == "27_3_A"] <- "27_3_A,27_3_A_20,27_3_A_21"

linkage$areaRDB[linkage$areaRDB == "27_3_A,27_4,27_7_D"] <- "27_3_A,27_3_A_20,27_3_A_21,27_4,27_4_A,27_4_B,27_4_C,27_7_D"

linkage$areaRDB[linkage$areaRDB == "27_4,27_7_D"] <- "27_4,27_4_A,27_4_B,27_4_C,27_7_D"

linkage$areaRDB[linkage$areaRDB == "27_7_D"] <- "27_7_D"

linkage$areaRDB[linkage$areaRDB == "27_3_A-4_AB_NK" &
                  linkage$latinName == "Gadus morhua"] <- "27_3_A_20" # ERROR

linkage$areaRDB[linkage$area == "3aS" &
                  linkage$latinName == "Gadus morhua"] <- "27_3_A_21" # ERROR



check_4 <- subset(linkage, areaBis != areaRDB)

# Output


write.table(select(linkage, -X, -X.1), paste0(output_path, "EUMAP_Table_2_1_Linkage_EUROSTAT and EC_TAC_version_2021_final_v01.csv"), 
          row.names = F, sep = ";")
