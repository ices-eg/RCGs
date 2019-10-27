
library(dplyr)
library(reshape2)
library(data.table)
library(stringr)

###Checking EUROSTAT for how spp is reported
input_path <- "Q:/scientific-projects/eu-data-collection/Work_Plan/2020/scripts/gits/RCGs/NWPtools/common/input"
ASFIS   <- read.table(file.path(input_path, 'ASFIS_sp_Feb_2016.txt'), header = TRUE, sep = "\t", as.is = TRUE)

DF <- read.table(file.path(input_path, "fish_ca_atl27.tsv"), header = TRUE, sep = '\t', as.is = TRUE)  # Atlantique NE
GEO <- read.table(file.path(input_path, "geo.def"), header = TRUE, sep = ";", as.is = TRUE)


names(GEO)[2] <- "Country"
GEO$geo <- toupper(GEO$geo)
SRG <- strsplit(as.character(DF$species.fishreg.unit.geo.time),split = ",")
SRG.m <- matrix(unlist(SRG), ncol = 4, byrow = TRUE)
DFT <- data.frame(X3A_CODE = toupper(SRG.m[,1]), Region = toupper(SRG.m[,2]), geo = SRG.m[,4],
                  Y2018 = DF$X2018, Y2017 = DF$X2017, Y2016 = DF$X2016, Y2015 = DF$X2015, Y2014 = DF$X2014, Y2013 = DF$X2013)
DFM <- merge(DFT, GEO, all.x = TRUE)
DFM$Y2013 <- as.numeric(str_remove_all(as.character(DFM$Y2013), "[bcdefinpzsu]")) 
DFM$Y2014 <- as.numeric(str_remove_all(as.character(DFM$Y2014), "[bcdefinpzsu]"))
DFM$Y2015 <- as.numeric(str_remove_all(as.character(DFM$Y2015), "[bcdefinpzsu]"))
DFM$Y2016 <- as.numeric(str_remove_all(as.character(DFM$Y2016), "[bcdefinpzsu]"))
DFM$Y2017 <- as.numeric(str_remove_all(as.character(DFM$Y2017), "[bcdefinpzsu]"))
DFM$Y2018 <- as.numeric(str_remove_all(as.character(DFM$Y2018), "[bcdefinpzsu]"))
DFM <- DFM[!is.na(DFM$Country),]

#Is all data from 2017 uploaded?
sum_year <- summarise(group_by(DFM, Country), sum_2018 = sum(Y2018, na.rm = T), sum_2017 = sum(Y2017, na.rm = T), sum_2016 = sum(Y2016, na.rm = T), sum_2015 = sum(Y2015, na.rm = T)
                      , sum_2014 = sum(Y2014, na.rm = T), sum_2013 = sum(Y2013, na.rm = T))

#Sarda sarda
bon <- subset(DFM, X3A_CODE %in% c("BON") & geo == "DK")

#Whiting
whg <- subset(DFM, X3A_CODE %in% c("WHG") & geo == "DK")

#Herring
her <- subset(DFM, X3A_CODE %in% c("HER") & geo == "DK")

#Flounder 22-24 per stock

fle <- subset(DFM, X3A_CODE %in% c("FLE") & substr(Region, 1, 6) %in% c("27_3_B", "27_3_C", "27_3_D") & !(Region %in% c("27_3_B", "27_3_C", "27_3_D")))

fle <- mutate(fle, stock = ifelse(Region %in% c("27_3_B_23", "27_3_C_22"), "fle.27.2223", 
                                  ifelse(Region %in% c("27_3_D_24", "27_3_D_25"), "fle.27.2425",
                                         ifelse(Region %in% c("27_3_D_26", "27_3_D_28"), "fle.27.2628",
                                               ifelse(Region %in% c("27_3_D_27", "27_3_D_29", "27_3_D_30", "27_3_D_31", "27_3_D_32"), "fle.27.2729-32", NA)))))

sum_fle <- summarise(group_by(filter(fle, !(is.na(stock))), Country, stock), sum_2018 = sum(Y2018, na.rm = T), sum_2017 = sum(Y2017, na.rm = T), sum_2016 = sum(Y2016, na.rm = T), sum_2015 = sum(Y2015, na.rm = T)
                      , sum_2014 = sum(Y2014, na.rm = T), sum_2013 = sum(Y2013, na.rm = T))

#Gadus morhua 
subset(DFM, X3A_CODE %in% c("COD") & Region == "27_3_A")

#Ammodytes spp
subset(ASFIS, Scientific_name %like% "Ammo")
subset(ASFIS, TAXOCODE %like% "17204002")

subset(DFM, X3A_CODE %in% c("SAN","ABZ") & Region %in% c("27_3", "27_4"))

#Anarhichas spp
subset(ASFIS, Scientific_name %like% "Anarhi")
subset(ASFIS, TAXOCODE %like% "17102001")

subset(DFM, X3A_CODE %in% c("CAA","CAB","CAS","CAT") & Region == "27_4")

#Aphanopus
subset(ASFIS, Scientific_name %like% "Aphan")
subset(ASFIS, TAXOCODE %like% "17506012")

subset(DFM, X3A_CODE %in% c("BSF","APH","BOX") & Region == "27" )

#Argentina spp
subset(ASFIS, Scientific_name %like% "Argen")
subset(ASFIS, TAXOCODE %like% "12305015")

subset(DFM, X3A_CODE %in% c("ARE","ARO","ARU","ARY","ARG") & Region == "27")

#Beryx
subset(ASFIS, Scientific_name %like% "Beryx")
subset(ASFIS, TAXOCODE %like% "16102003")

subset(DFM, X3A_CODE %in% c("BXD","BYS","ALF") & Region == "27")

#Capros aper
subset(ASFIS, Scientific_name %like% "Capr")

subset(DFM, X3A_CODE %in% c("BOR","BOC") & Region == "27")

#Eutrigla
subset(ASFIS, Family %like% "Trigl")
subset(ASFIS, TAXOCODE %like% "17802002")

subset(DFM, X3A_CODE %in% c("BXD","BYS","ALF") & Region == "27")


#Illex
subset(ASFIS, Scientific_name %like% "Ille")
subset(ASFIS, TAXOCODE %like% "32105010")

subset(DFM, X3A_CODE %in% c("SQI","SQM","SQA","IXO","ILL") & Region == "27")

#Todarodes
subset(ASFIS, Scientific_name %like% "Todaro")
subset(ASFIS, TAXOCODE %like% "32105058")

subset(DFM, X3A_CODE %in% c("SQE","TFP","SQJ","SQG","QSX") & Region == "27")

#Pandalus
subset(ASFIS, Scientific_name %like% "Pandal")
subset(ASFIS, TAXOCODE %like% "22804002")

subset(DFM, X3A_CODE %in% c("PYX","PWY","PRA","PJK","AES","DUD","DUJ","DUK","DLN","DLS","PAN") & Region == "27")

#Scomber colias
subset(ASFIS, English_name %like% "Chub")

#Scomber spp
subset(ASFIS, Scientific_name %like% "Scomb")
subset(ASFIS, TAXOCODE %like% "17501002")

subset(DFM, X3A_CODE %in% c("MAS","MAC","MAA","MAZ") & Region == "27")

#Trisopterus spp
subset(ASFIS, Scientific_name %like% "Trisopterus spp")
subset(ASFIS, TAXOCODE %like% "14804032")

subset(DFM, X3A_CODE %in% c("NOP","POD","BIB","XOD") & Region == "27")

#Lophius
subset(ASFIS, Scientific_name %like% "Lophi")
subset(ASFIS, TAXOCODE %like% "19501001")

subset(DFM, X3A_CODE %in% c("MON","ANK","ANG","MVA","MVO","MVJ","MVN","MNZ") & Region == "27")


#Megrims
subset(ASFIS, Scientific_name %like% "Lepidor")
subset(ASFIS, TAXOCODE %like% "18305003")

subset(DFM, X3A_CODE %in% c("MEG","LDB","LEZ") & Region != "27")

#Boarfish
subset(ASFIS, Scientific_name %like% "Capro")
subset(ASFIS, TAXOCODE %like% "16203002")

subset(DFM, X3A_CODE %in% c("BOC","BOR") & Region == "27")

subset(DFM, X3A_CODE %in% c("BOC","BOR") & Region %in% c("27_6", "27_7", "27_8"))


#Lodde
subset(ASFIS, Scientific_name %like% "Mallo")
subset(ASFIS, TAXOCODE %like% "16203002")

subset(DFM, X3A_CODE %in% c("CAP") & Region == "27")

#Cod in I and II
subset(DFM, X3A_CODE %in% c("COD") & Region %in% c("27_1","27_2"))

#Pandalus in area I and II
subset(DFM, X3A_CODE %in% c("PYX","PWY","PRA","PJK","AES","DUD","DUJ","DUK","DLN","DLS","PAN")  & Region %in% c("27_1","27_2"))

#Mac outside area 3 and 4
subset(DFM, X3A_CODE %in% c("MAC")  & !(Region %in% c("27_4","27_3")))

#Mac overall
subset(DFM, X3A_CODE %in% c("MAC")  & Region %in% c("27"))


