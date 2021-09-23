

# Adding agreed changed to linkage file
# Only making changes to ICES & NAFO

library(dplyr)
library(tidyr)
# library(stringr)
library(data.table)

path <- "Q:/mynd/RCM/RCGs/NWPtools/table_2_1/"

test <- "yes" # Testing for duplicated areas or not
rfmo_to_test <- c("ICES", "NAFO") # Which RFMO's to include - the script only handles areas from ICES and NAFO


linkage <- read.csv(file.path(path, 'EUMAP_Table_2_1_Linkage_EUROSTAT and EC_TAC_version_2021_final.csv'), sep = ";", header = T)

# All areas ----

all_areas <- subset(linkage, area %in% c("All areas", "all areas"))


linkage$area[linkage$latinName == "Squalus acanthias" &
               linkage$region == "North Sea and Eastern Arctic" &
               linkage$areaBis == "27_4,27_7_D"] <- "1, 2"
linkage$areaBis[linkage$latinName == "Squalus acanthias" &
               linkage$region == "North Sea and Eastern Arctic" &
               linkage$areaBis == "27_4,27_7_D"] <- "27_1,27_2"

linkage$area[linkage$latinName == "Squalus acanthias" &
               linkage$region == "North Sea and Eastern Arctic" &
               linkage$areaBis == "27_3_A,27_4,27_7_D"] <- "3a, 4 and 7d"

linkage$area[linkage$region == "North-East Atlantic" &
               linkage$area %in% c("All areas", "all areas")] <-
  "5, 6, 7 (excl. 7d), 8, 9, 10, 12 and 14"


linkage$areaBis[linkage$region == "North-East Atlantic" &
               linkage$area == "5, 6, 7 (excl. 7d), 8, 9, 10, 12 and 14"] <-
  "27_5,27_6,27_7_A,27_7_B,27_7_C,27_7_E,27_7_F,27_7_G,27_7_H,27_7_J,27_7_K,27_8,27_9,27_10,27_12,27_14"

check <- distinct(filter(linkage, area == "5, 6, 7 (excl. 7d), 8, 9, 10, 12 and 14"), areaBis)

all_areas <- subset(linkage, area %in% c("All areas", "all areas"))

# Overlapping stocks ----

linkage$area[linkage$region == "North-East Atlantic" &
               linkage$latinName == "Argentina silus" &
               linkage$area == "5, 6, 7"] <- "5b, 6, 7"

linkage$areaBis[linkage$region == "North-East Atlantic" &
               linkage$latinName == "Argentina silus" &
               linkage$area == "5b, 6, 7"] <- "27_5_B,27_6,27_7"

names(linkage)

silus_new <- data.frame(region = "North-East Atlantic",
                        sppName = "Greater silver smelt",
                        latinName = "Argentina silus",
                        RFMO = "ICES",
                        RFMO_Stock_ID = NA,
                        X3A_CODE = "ARU", 
                        FIDES_AREA =  "No TAC",
                        area = "5a, 14",
                        areaBis = "27_5_A,27_14",
                        TAC.area.description = " ",
                        Comments = "")

linkage <- bind_rows(linkage, silus_new)

linkage$area[linkage$region == "North Sea and Eastern Arctic" &
               linkage$latinName == "Argentina silus" &
               linkage$area == "1, 2, 5a, 14"] <- "1, 2"

linkage$areaBis[linkage$region == "North Sea and Eastern Arctic" &
                  linkage$latinName == "Argentina silus" &
                  linkage$area == "1, 2"] <- "27_1,27_2"


linkage$area[linkage$region == "North-East Atlantic" &
               linkage$latinName == "Micromesistius poutassou" &
               linkage$area == "1-9, 12, 14"] <-
  "5, 6, 7 (excl. 7d), 8, 9, 10, 12 and 14"

linkage$areaBis[linkage$region == "North-East Atlantic" &
                  linkage$latinName == "Micromesistius poutassou" &
                  linkage$area == "5, 6, 7 (excl. 7d), 8, 9, 10, 12 and 14"] <-
  "27_5,27_6,27_7_A,27_7_B,27_7_C,27_7_E,27_7_F,27_7_G,27_7_H,27_7_J,27_7_K,27_8,27_9,27_12,27_14"

# micro_new <- data.frame(region = "Baltic Sea",                 # Not relevant - the areas are not in the WGWIDE report
#                         sppName = "Blue whiting",
#                         latinName = "Micromesistius poutassou",
#                         RFMO = "ICES",
#                         RFMO_Stock_ID = NA,
#                         X3A_CODE = "WHB", 
#                         FIDES_AREA =  "No TAC",
#                         area = "22-32",
#                         areaBis = "27_3_B,27_3_C,27_3_D",
#                         TAC.area.description = " ",
#                         Comments = "")


linkage$area[linkage$region == "North Sea and Eastern Arctic" &
               linkage$latinName == "Mustelus spp." &
               linkage$area == "1, 2, 14"] <-
  "1, 2"

linkage$areaBis[linkage$region == "North Sea and Eastern Arctic" &
                  linkage$latinName == "Mustelus spp." &
                  linkage$area == "1, 2"] <-
  "27_1,27_2"


linkage$area[linkage$region == "North Sea and Eastern Arctic" &
               linkage$latinName == "Pandalus borealis" &
               linkage$area == "3a, 4 and 2a Union waters"] <-
  "3a, 4"

linkage$areaBis[linkage$region == "North Sea and Eastern Arctic" &
                  linkage$latinName == "Pandalus borealis" &
                  linkage$area == "3a, 4"] <-
  "27_3_A,27_4"

linkage <- subset(linkage, !(latinName == "Pandalus borealis" & area == "4 Norwegian waters south of 62N"))

linkage$area[linkage$region == "Baltic Sea" &
               linkage$latinName == "Pleuronectes platessa" &
               linkage$area == "21-23"] <-
  "22-23"

linkage$areaBis[linkage$region == "Baltic Sea" &
                  linkage$latinName == "Pleuronectes platessa" &
                  linkage$area == "22-23"] <-
  "27_3_B_23,27_3_C_22"


linkage$area[linkage$region == "North Sea and Eastern Arctic" &
               linkage$latinName == "Raja brachyura" &
               linkage$area == "4a, 4c, 7d"] <-
  "4c, 7d"

linkage$areaBis[linkage$region == "North Sea and Eastern Arctic" &
                  linkage$latinName == "Raja brachyura" &
                  linkage$area == "4c, 7d"] <-
  "27_4_C,27_7_D"

linkage$FIDES_AREA[linkage$region == "North Sea and Eastern Arctic" &
                  linkage$latinName == "Raja brachyura" &
                  linkage$area == "4a, 7d"] <-
  "04-C.07D."


linkage$area[linkage$region == "Other regions" &
               linkage$latinName == "Salmo salar" &
               linkage$area == "NAFO S1 + 1CES Sub- area 14, NEAFC, NASCO"] <-
  "SA1"

linkage$areaBis[linkage$region == "Other regions" &
                  linkage$latinName == "Salmo salar" &
                  linkage$area == "SA1"] <-
  "21_1"


linkage$area[linkage$region == "North Sea and Eastern Arctic" &
                  linkage$latinName == "Solea solea" &
                  linkage$area == "Union waters of 2a, 3a and 4"] <-
  "Union waters of 2a and 4"

linkage$areaBis[linkage$region == "North Sea and Eastern Arctic" &
                     linkage$latinName == "Solea solea" &
                     linkage$area == "Union waters of 2a and 4"] <-
  "27_2_A,27_4"

linkage$FIDES_AREA[linkage$region == "North Sea and Eastern Arctic" &
                  linkage$latinName == "Solea solea" &
                  linkage$area == "Union waters of 2a and 4"] <-
  "24-C."

linkage$FIDES_AREA[linkage$region == "Baltic Sea" &
                     linkage$latinName == "Solea solea" &
                     linkage$area == "20-24"] <-
  "3ABC24"


linkage$areaBis[linkage$region == "Baltic Sea" &
                     linkage$latinName == "Solea solea" &
                     linkage$area == "20-24"] <-
  "27_3_A,27_3_C_22,27_3_B_23,27_3_D_24"


write.table(select(linkage, -X, -X.1), paste0(path, "EUMAP_Table_2_1_Linkage_EUROSTAT_RDB and EC_TAC.csv"), 
            row.names = F, sep = ";")