

# creating codes references from the RDB
# Kirsten Birch Håkansson, DTU Aqua, Denmark
	# 2021-09-04: Nuno Prista added worms Family, Order, Class to rdb_spp

library(dplyr)
library("worrms")

path <- "Q:/mynd/RCM/RCGs/NWPtools/table_2_1/"

load(paste(path,'RDB_All_Regions_CL_2009_2020_prepared_20210428.Rdata' ,sep = ''))

# Areas ----

rdb_areas <- unique(cl$Area)

write.table(rdb_areas, paste0(path, "rdb_area_ref.csv"), sep = ";", row.names = F)

# Countries ----

rdb_ctry <- as.character(unique(cl$FlagCountry))

write.table(rdb_ctry, paste0(path, "rdb_ctry_ref.csv"), sep = ";", row.names = F)

# Species ----

rdb_spp <- dplyr::distinct(cl, SpeciesAphiaID, Species, SpeciesDesc)

rdb_spp$Family<-NA
rdb_spp$Order<-NA
rdb_spp$Class<-NA

worms_extra<-data.frame(worrms::wm_classification_(id = c(rdb_spp$SpeciesAphiaID)))
rdb_spp$Family<-worms_extra[worms_extra$rank=="Family","scientificname"][match(rdb_spp$SpeciesAphiaID, worms_extra[worms_extra$rank=="Family",]$id)]
rdb_spp$Order<-worms_extra[worms_extra$rank=="Order","scientificname"][match(rdb_spp$SpeciesAphiaID, worms_extra[worms_extra$rank=="Order",]$id)]
rdb_spp$Class<-worms_extra[worms_extra$rank=="Class","scientificname"][match(rdb_spp$SpeciesAphiaID, worms_extra[worms_extra$rank=="Class",]$id)]

# removes a few duplicates generated during call to worms
rdb_spp<-unique(rdb_spp)

write.table(rdb_spp, paste0(path, "rdb_spp.csv"), sep = ";", row.names = F)
