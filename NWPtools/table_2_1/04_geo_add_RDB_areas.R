
# Adapting the GEO file to the RDB country codes
# Kirsten Birch Håkansson, DTU Aqua, Denmark

path <- "Q:/mynd/RCM/RCGs/NWPtools/table_2_1/"


GEO     <- read.table(paste(path,'geo.def',sep='/'),header=TRUE,sep=";", as.is=TRUE)
GEO <- GEO[-1,] #EU (27MS) instead of EU (28MS)
names(GEO)[2] <- "Country"
GEO$geo <- toupper(GEO$geo) #2-letter code should be in capitals

rdb_ctry <- arrange(read.csv(paste0(path, "rdb_ctry_ref.csv"), sep = ";"), x)

geo <- merge(GEO, rdb_ctry, by.x = "Level.Description", by.y = "x", all.y = T)

subset(geo, is.na(geo))

geo$geo[geo$Level.Description %in% c("CHA", "ENG", "NIR", "SCT", "WLS")] <- "UK"

geo$Country[geo$Level.Description %in% c("CHA", "ENG", "NIR", "SCT", "WLS")] <- "United Kingdom"


geo$geo[geo$Level.Description %in% c("IRL")] <- "IE"

geo$Country[geo$Level.Description %in% c("IRL")] <- "Ireland"


write.table(geo, paste0(path, "GEO_RDB.csv"), row.names = F, sep = ";")
