

# Adapting areas in the linkage table to the RDB areas / structure
# A bit fast and dirty copy / paste coding
# Kirsten Birch Håkansson, DTU Aqua, Denmark

library(dplyr)
library(tidyr)
# library(stringr)
library(data.table)

path <- "Q:/mynd/RCM/RCGs/NWPtools/table_2_1/"

test <- "yes" # Testing for duplicated areas
rfmo_to_test <- c("ICES", "NAFO")


linkage <- read.csv(file.path(path, 'EUMAP_Table_2_1_Linkage_EUROSTAT and EC_TAC_version_2021_final.csv'), sep = ";", header = T)
rdb_areas <- arrange(read.csv(paste0(path, "rdb_area_ref.csv"), sep = ";"), x)

names(linkage)

# Removing blanks from areaBis

unique(linkage$areaBis)

linkage$areaBis <- gsub(" ", "", linkage$areaBis)

# Fixing a couple of areaBis errors ----

linkage$areaBis[linkage$latinName == "Merlangius merlangus" & linkage$area == "7a"] <- "27_7_A"
linkage$areaBis[linkage$area == "6, 7, 8, 9"] <- "27_6,27_7_A,27_7_B,27_7_C,27_7_E,27_7_F,27_7_G,27_7_H,27_7_J,27_7_K,27_8,27_9"
linkage$areaBis[linkage$area == "5, 14 (demersal) "] <- "27_5,27_14"

linkage$areaBis[linkage$area == "SA1-6"] <- "21_1,21_2,21_3,21_4,21_5,21_6"
linkage$areaBis[linkage$area == "SA1-3"] <- "21_1,21_2,21_3"


# Fixing duplicated regions in areaBis ----

linkage$areaBis[linkage$area == "3aS" &
                  linkage$latinName == "Pleuronectes platessa"] <- " " # Duplicate - already in the Baltic Sea

linkage$areaRDB[linkage$area == "Union waters of 2a, 3a and 4" &
                  linkage$latinName == "Solea solea"] <- "27_2_A,27_4" # 3a a duplicate - already in the Baltic Sea

# Coding RDB areas ----

linkage$areaRDB <- linkage$areaBis

linkage$areaRDB <- paste0(",", gsub('_', '.', linkage$areaRDB), ",") # , add to make the gsub a bit easier


linkage$areaRDB[linkage$region %in% c("Baltic Sea",
                                      "North Sea and Eastern Arctic",
                                      "North-East Atlantic")] <-
  tolower(linkage$areaRDB[linkage$region %in% c("Baltic Sea",
                                                "North Sea and Eastern Arctic",
                                                "North-East Atlantic")])

unique(linkage$areaRDB)

# Baltic Sea / Baltic Sea (ICES areas 3b-d, FAO area 27)

linkage$areaRDB <- gsub("27.3.d.28,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.3.d.28"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <- gsub("27.3.d,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.3.d"], collapse = ","), ","), linkage$areaRDB)


check_2 <- distinct(subset(linkage, region == "Baltic Sea"), area, areaBis, areaRDB)

# North Sea and Eastern Arctic / Eastern Arctic, Norwegian Sea and Barents Sea (ICES areas 1, 2, FAO area 27)

linkage$areaRDB[linkage$areaBis == "27_4,27_7_D" &
                  linkage$latinName == "Squalus acanthias"] <- "27.1,27.2," #Error

linkage$areaRDB <-
  gsub("27.1,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.1" &
                                           !(substr(rdb_areas$x, 5, 5) %in% c("0", "2", "4"))], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.2.a,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.2.a"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.2,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.2"], collapse = ","), ","), linkage$areaRDB)



# North Sea and Eastern Arctic / North Sea and Eastern Channel (ICES areas 3a, 4 and 7d, FAO area 27)

linkage$areaRDB <-
  gsub("27.3.a,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.3.a"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.4,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.4"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.7.d,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.7.d"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB[linkage$area == "3aN" &
                  linkage$latinName == "Gadus morhua"] <- "27.3.a.20" # ERROR

linkage$areaRDB[linkage$area == "3aS" &
                  linkage$latinName == "Gadus morhua"] <- "27.3.a.21" # ERROR

check_3 <- distinct(subset(linkage, region == "North Sea and Eastern Arctic"), latinName, area, areaBis, areaRDB)

# North-East Atlantic / North-East Atlantic and Western Channel (ICES areas 5, 6, 7 (excl. 7d), 8, 9, 10, 12 and 14, FAO area 27)

linkage$areaRDB <-
  gsub("27.5.a,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.5.a"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.5.b,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.5.b"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.5,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.5"], collapse = ","), ","), linkage$areaRDB)


linkage$areaRDB <-
  gsub("27.6.a,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.6.a"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.6.b,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.6.b"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.6,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.6"], collapse = ","), ","), linkage$areaRDB)


linkage$areaRDB <-
  gsub("27.7.a,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.7.a"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.7.b,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.7.b"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.7.c,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.7.c"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.7.e,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.7.e"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.7.f,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.7.f"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.7.g,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.7.g"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.7.h,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.7.h"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.7.j,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.7.j"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.7.k,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.7.k"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.7,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.7"], collapse = ","), ","), linkage$areaRDB)


linkage$areaRDB <-
  gsub("27.8.a,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.8.a"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.8.b,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.8.b"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.8.c,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.8.c"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.8.d,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.8.d"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.8.e,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.8.e"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.8,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.8"], collapse = ","), ","), linkage$areaRDB)


linkage$areaRDB <-
  gsub("27.9.a,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.9.a"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.9.b,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.9.b"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.9,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.9"], collapse = ","), ","), linkage$areaRDB)


linkage$areaRDB <-
  gsub("27.10.a,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.10.a"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.10.b,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.10.b"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.10,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.10"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.12.a,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.12.a"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.12.b,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.12.b"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.12.c,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.12.c"], collapse = ","), ","), linkage$areaRDB)
linkage$areaRDB <-
  gsub("27.12,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.12"], collapse = ","), ","), linkage$areaRDB)


linkage$areaRDB <-
  gsub("27.14.a,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.14.a"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.14.b,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.14.b"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub("27.14,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "27.14"], collapse = ","), ","), linkage$areaRDB)


check_4 <- distinct(subset(linkage, region == "North-East Atlantic"), area, areaBis, areaRDB)

# Other regions / North-West Atlantic (FAO area 21)

linkage$areaRDB <-
  gsub(",21.1,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "21.1"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub(",21.2,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "21.2"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub(",21.3,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "21.3"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub(",21.4,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "21.4"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub(",21.5,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "21.5"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub(",21.6,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "21.6"], collapse = ","), ","), linkage$areaRDB)

linkage$areaRDB <-
  gsub(",21,", paste0(paste(rdb_areas$x[rdb_areas$x %like% "21"], collapse = ","), ","), linkage$areaRDB)


check_5 <- distinct(subset(linkage, region == "Other regions" & substr(areaBis, 1, 2) == "21"), area, areaBis, areaRDB)

# Tests
# Duplicated areas and species

if (test == "yes"){
  
  test <- subset(linkage, RFMO %in% rfmo_to_test)
  
  test1 <- select(test, region, latinName, area, areaRDB)
  
  areaRDB_split <- separate(data = test1, col = areaRDB, sep = ",", into = paste0("var", seq(1:100)))
  
  areaRDB_split_t <- gather(areaRDB_split, key = "areaR", value = "areaRDB", -region, -area, -latinName)
  
  areaRDB_split_t2 <- areaRDB_split_t[!is.na(areaRDB_split_t$areaRDB) & areaRDB_split_t$areaRDB != "", ]
  
  areaRDB_split_t2 <- mutate(arrange(areaRDB_split_t2, latinName, areaRDB), no = 1)
  
  areaRDB_split_t3 <- subset(mutate(group_by(areaRDB_split_t2, latinName, areaRDB), no_sum = sum(no)), no_sum > 1)
  
  dup_areas_full <- distinct(ungroup(areaRDB_split_t3), region, latinName, area, areaRDB)
  
  dup_areas <- distinct(ungroup(areaRDB_split_t3), region, latinName, area)
  
  write.table(dup_areas, paste0(path, "table_2.1_duplicated_areas.csv"), row.names = F, sep = ";")
  
  test2 <- distinct(test,  area, areaBis, areaRDB)
  
  write.table(test2, paste0(path, "table_2.1_distinct_areas.csv"), row.names = F, sep = ";")
  
}

# A bit of cleaning

linkage$areaRDB <- gsub("^\\,|\\,$", "", linkage$areaRDB)

unique(linkage$areaRDB)

# Output


write.table(select(linkage, -X, -X.1), paste0(path, "EUMAP_Table_2_1_Linkage_EUROSTAT_RDB and EC_TAC.csv"), 
          row.names = F, sep = ";")
