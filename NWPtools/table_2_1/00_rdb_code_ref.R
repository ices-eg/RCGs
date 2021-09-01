

# creating codes references from the RDB

path <- "Q:/mynd/RCM/RCGs/NWPtools/table_2_1/"

load(paste(path,'RDB_All_Regions_CL_2009_2020_prepared_20210428.Rdata' ,sep = ''))

rdb_areas <- unique(cl$Area)

write.table(rdb_areas, paste0(path, "rdb_area_ref.csv"), sep = ";", row.names = F)
