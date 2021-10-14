

# Adapting and correcting scientific names - adaptation to the RDB names
# Kirsten Birch Håkansson, DTU Aqua, Denmark

################################################################################
# Remember to run "01_table_1a_linkage_table_correcting_latinName.R" before this script
# Remember to run "02_table_1a_linkage_table_correcting_latinName.R" before this script
################################################################################

library(dplyr)
library(data.table)
# library(tidyr)
# library(stringr)

path <-
  "Q:/scientific-projects/eu-data-collection/Work_Plan/2022/Scripts/Table_2_1/RCGs/NWPtools/table_2_1/inputs/"

rfmo_to_test <- c("ICES", "NAFO")

linkage <-
  read.csv(
    file.path(path, 'EUMAP_Table_2_1_Linkage_EUROSTAT_RDB and EC_TAC.csv'),
    sep = ";",
    header = T
  )

linkage_rfmo <- subset(linkage, RFMO %in% rfmo_to_test)

# Remove old comments

linkage$Comments <- NA

# Commeting the 27.3.a. fix in 'filling table ...'

linkage$Comments[linkage$latinName %in% c("Gadus morhua", "Pleuronectes platessa") &
                   linkage$area == "3aN"] <-
  "DNK comments to common script: Assuming that all other countries, than DEU, DNK and SWE, are fishing in 27.3.a.20"

# Commenting other fixes in 'filling table ...'

linkage$Comments[linkage$latinName ==  "Nephrops norvegicus"] <-
  "DNK comments to common script: Removed the Nephrop specific removal of qoutas"

linkage$Comments[linkage$latinName ==  "Salmo salar"] <-
  "DNK comments to common script: Tresholds = None, like Anguilla anguilla"

linkage$Comments[linkage$latinName ==  "Anguilla anguilla"] <-
  "DNK comments to common script: Fixed wrongly coding of regCoord	& coveredLength"

# Fixing all areas for ICCAT - fixed in 'filling table ...'

linkage$areaRDB[linkage$RFMO == "ICCAT"] <- "ICCAT"
linkage$Comments[linkage$RFMO == "ICCAT"] <-
  "DNK comments to common script: Fixed wrongly coding of regCoord	& coveredLength"

# Including quotas DNK fish on in 2020 + a bit more

linkage$FIDES_AREA[linkage$latinName == "Capros aper"] <-
  "678-,-678"
linkage$Comments[linkage$latinName == "Capros aper"] <-
  "DNK comments to common script: Fixed a qouta misspelling"

linkage$FIDES_AREA[linkage$latinName == "Clupea harengus" &
                     linkage$area == "3a, 4, 7d"] <- "03A.,03A-BC,2A47DX,4AB.,4CXB7D"
linkage$Comments[linkage$latinName == "Clupea harengus" &
                   linkage$area == "3a, 4, 7d"] <- "DNK comments to common script: Included 4CXB7D"


linkage$FIDES_AREA[linkage$latinName == "Limanda limanda" &
                     linkage$area == "3a, 4, 7d"] <- "No TAC"
linkage$Comments[linkage$latinName == "Limanda limanda" &
                   linkage$area == "3a, 4, 7d"] <- "DNK comments to common script: Removed qouta"


linkage$X3A_CODE[linkage$latinName %in% c("Lophius budegassa", "Lophius piscatorius") &
                   linkage$area == "3a, 4, 7d"] <- "ANF"
linkage$FIDES_AREA[linkage$latinName %in% c("Lophius budegassa", "Lophius piscatorius") &
                     linkage$area == "3a, 4, 7d"] <- "2AC4-C,04-N."
linkage$latinNameJoin[linkage$latinName %in% c("Lophius budegassa", "Lophius piscatorius") &
                        linkage$area == "3a, 4, 7d"] <-
  paste0(linkage$latinNameJoin[linkage$latinName %in% c("Lophius budegassa", "Lophius piscatorius") &
                                 linkage$area == "3a, 4, 7d"], ",Lophiidae,Lophius")
linkage$Comments[linkage$latinName %in% c("Lophius budegassa", "Lophius piscatorius") &
                   linkage$area == "3a, 4, 7d"] <-
  "DNK comments to common script: Changed X3A_code to ANF. Changed qoutas. Included Lophiidae and Lophius"

linkage$FIDES_AREA[linkage$latinName == "Melanogrammus aeglefinus" &
                     linkage$area == "3a, 4"] <- "03A.,04-N.,2AC4."
linkage$Comments[linkage$latinName == "Melanogrammus aeglefinus" &
                   linkage$area == "3a, 4"] <-
  "DNK comments to common script: Changed qoutas"


linkage$X3A_CODE[linkage$latinName == "Microstomus kitt" &
                   linkage$area == "4, 7d" |
                   linkage$latinName == "Glyptocephalus cynoglossus" &
                   linkage$area == "3a, 4"] <- "L/W"

linkage$FIDES_AREA[linkage$latinName == "Glyptocephalus cynoglossus" &
                     linkage$area == "3a, 4"] <- "2AC4-C"

linkage$Comments[linkage$latinName == "Glyptocephalus cynoglossus" &
                   linkage$area == "3a, 4"] <-
  "DNK comments to common script: Changed X3A_code to L/W. Included qouta"
linkage$Comments[linkage$latinName == "Microstomus kitt" &
                   linkage$area == "4, 7d"] <-
  "DNK comments to common script: Changed X3A_code to L/W"


linkage$FIDES_AREA[linkage$latinName == "Micromesistius poutassou" &
                     linkage$area == "5, 6, 7 (excl. 7d), 8, 9, 10, 12 and 14"] <-
  "1X14,2A4AXF,8C3411"
linkage$Comments[linkage$latinName == "Micromesistius poutassou" &
                   linkage$area == "5, 6, 7 (excl. 7d), 8, 9, 10, 12 and 14"] <-
  "DNK comments to common script: Included 2A4AXF,8C3411"

linkage$FIDES_AREA[linkage$latinName == "Pandalus borealis" &
                     linkage$area == "SA1"] <-
  "N1GRN."
linkage$Comments[linkage$latinName == "Pandalus borealis" &
                   linkage$area == "SA1"] <-
  "DNK comments to common script: Corrected qouta misspelling"


linkage$FIDES_AREA[linkage$latinName == "Pandalus borealis" &
                     linkage$area == "3a, 4"] <-
  "03A.,04-N.,2AC4-C"
linkage$Comments[linkage$latinName == "Pandalus borealis" &
                   linkage$area == "3a, 4"] <-
  "DNK comments to common script: Included 2AC4-C"


linkage$FIDES_AREA[linkage$latinName == "Pandalus borealis" &
                     linkage$area == "1, 2"] <-
  "No TAC"
linkage$Comments[linkage$latinName == "Pandalus borealis" &
                   linkage$area == "1, 2"] <-
  "DNK comments to common script: Removed qouta. All landing from this area is from 27.1 or 27.2.b in the RDB and the qouta used covered Union waters of 2a and 4"


linkage$X3A_CODE[linkage$latinName %in% c("Scophthalmus maximus", "Scophthalmus rhombus") &
                   linkage$area == "3a, 4, 7d"] <- "T/B"
linkage$FIDES_AREA[linkage$latinName %in% c("Scophthalmus maximus", "Scophthalmus rhombus") &
                     linkage$area == "3a, 4, 7d"] <- "2AC4-C"

linkage$Comments[linkage$latinName %in% c("Scophthalmus maximus", "Scophthalmus rhombus") &
                   linkage$area == "3a, 4, 7d"] <-
  "DNK comments to common script: Aligned codes to MasterCodeList.Changed X3A_code to T/B. Added 2AC4-C"

# Scomber scombrus qoutas missing

linkage$latinNameJoin[linkage$latinName %in% c("Trachurus trachurus") &
                        linkage$area %in% c("Union waters of 4b, 4c and 7d", "4a, 5b, 6a, 7a-c, 7e-k, 8")] <-
  paste0(linkage$latinNameJoin[linkage$latinName %in% c("Trachurus trachurus") &
                                 linkage$area %in% c("Union waters of 4b, 4c and 7d", "4a, 5b, 6a, 7a-c, 7e-k, 8")], ",Trachurus")
linkage$Comments[linkage$latinName %in% c("Trachurus trachurus") &
                   linkage$area %in% c("Union waters of 4b, 4c and 7d", "4a, 5b, 6a, 7a-c, 7e-k, 8")] <-
  "DNK comments to common script: Included Trachurus"


linkage$RFMO[is.na(linkage$RFMO)] <- "NA"

# Output ----

write.table(
  linkage,
  paste0(
    path,
    "EUMAP_Table_2_1_Linkage_EUROSTAT_RDB and EC_TAC_DNK.csv"
  ),
  row.names = F,
  sep = ";"
)
