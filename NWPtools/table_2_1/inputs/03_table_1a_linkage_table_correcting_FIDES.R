

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
# Remember to run "02_table_1a_linkage_table_correcting_latinName.R" before this script
################################################################################

rm(list=ls())
library(dplyr)
library(data.table)
# library(tidyr)
# library(stringr)

path <- "Q:/mynd/RCM/RCGs/NWPtools/table_2_1/"

rfmo_to_test <- c("ICES", "NAFO")

linkage <- read.csv(file.path(path, 'EUMAP_Table_2_1_Linkage_EUROSTAT_RDB and EC_TAC.csv'), sep = ";", header = T)

linkage_rfmo <- subset(linkage, RFMO %in% rfmo_to_test)

# Adding X3A_CODE_new, FIDES_AREA_new, Comments_new

linkage$X3A_CODE_new <- linkage$X3A_CODE
linkage$FIDES_AREA_new <- linkage$FIDES_AREA

# Correcting X3A_CODE_new, FIDES_AREA_new

linkage$FIDES_AREA_new[linkage$X3A_CODE == "RJR" & linkage$area == "6, 12"] <- "No TAC"



# Output ----

write.table(linkage, paste0(path, "EUMAP_Table_2_1_Linkage_EUROSTAT_RDB and EC_TAC.csv"), 
          row.names = F, sep = ";")
