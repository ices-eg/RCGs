
#Based on 2_Table1A_stockID_Aqua_vs_linkage from 2016

library(plyr)

#Path <- "M:/DCF/AP/2017/Programs/Tabel_1A/From ICES share point/20161021/"
path <-  "M:/DCF/AP/2017/Programs/Tabel_1A"

share<-read.table(file.path(path,'EUMAP_Table1A_Linkage_EUROSTAT and EC_TAC_DNK_v3.csv'), sep=";",header=T, colClasses = "character")
aqua<-read.table(file.path(path, 'EUMAP_Table_1A_Aqua.csv'), sep=";",header=T, colClasses = "character")

com<-merge(share,aqua,all=T,by.x=c("latinName","area"),by.y=c("latinName","area"))

com2<-subset(com, stockID.x!=stockID.y & stockID.x!="No TAC" & stockID.y!="")
names(com2)
com3<-com2[,c(1,2,3,4,5,11)]

write.table(com3,"M:/DCF/AP/2017/Programs/Tabel_1A/Checks/Check_stockID_aqua_vs_share.csv", sep=";", row.names = F)

#Change stock

share_old<-share

share$stockID_new<-ifelse(share$reportingName=="Ammodytes spp" & share$area=="IV" & share$region=="North Sea and Eastern Arctic","SAN2A3A4., SAN04-N.",
                  ifelse(share$reportingName=="Brosme brosme" & share$area=="IV" & share$region=="North Sea and Eastern Arctic","USK04-C.,USK04-N.",
                  ifelse(share$reportingName=="Clupea harengus" & share$area=="30" & share$region=="Baltic Sea","HER30/31.",
                  ifelse(share$reportingName=="Clupea harengus" & share$area=="31" & share$region=="Baltic Sea","HER30/31.",
                  ifelse(share$reportingName=="Clupea harengus" & share$area=="IV, VIId" & share$region=="North Sea and Eastern Arctic","HER4CXB7D,HER4AB.,HER04-N.,HER2A47DX.",
                  ifelse(share$reportingName=="Clupea harengus" & share$area=="VIa" & share$region=="North Atlantic","HER5B6ANB,HER06ACL.",
                  ifelse(share$reportingName=="Gadus morhua" & share$area=="I, II" & share$region=="North Sea and Eastern Arctic","COD1/2B.,COD1N2AB.",
                  ifelse(share$reportingName=="Gadus morhua" & share$area=="IV, VIId" & share$region=="North Sea and Eastern Arctic","COD2A3AX4,COD07D.,COD04-N.",
                  ifelse(share$reportingName=="Lophius budegassa" & share$area=="IV, VI" & share$region=="North Atlantic","ANF56-14,ANF04-N.,ANF2AC4-C",
                  ifelse(share$reportingName=="Lophius budegassa" & share$area=="IV, VIId" & share$region=="North Sea and Eastern Arctic","ANF04-N.,ANF2AC4-C",
                  ifelse(share$reportingName=="Lophius piscatorius" & share$area=="IV" & share$region=="North Sea and Eastern Arctic","ANF04-N.,ANF2AC4-C",
                  ifelse(share$reportingName=="Melanogrammus aeglefinuss" & share$area=="IV" & share$region=="North Sea and Eastern Arctic","HAD2AC4.,HAD04-N.",
                  ifelse(share$reportingName=="Merlangius merlangus" & share$area=="IV, VIId" & share$region=="North Sea and Eastern Arctic","WHG2AC4.,WHG04-N.",
                  ifelse(share$reportingName=="Merluccius merluccius" & share$area=="IIIa, IV, VI, VII, VIIIab" & share$region=="North Sea and Eastern Arctic","HKE3A/BCD,HKE2AC4-C,HKE571214,HKE8ABDE.",
                  ifelse(share$reportingName=="Micromesistius poutassou" & share$area=="I-IX, XII, XIV" & share$region=="North Sea and Eastern Arctic","WHB1X14,WHB24AXF",
                  ifelse(share$reportingName=="Molva molva" & share$area=="all share$areas" & share$region=="North Atlantic","LIN05EI.,LIN6X14.",
                  ifelse(share$reportingName=="Molva molva" & share$area=="IV" & share$region=="North Sea and Eastern Arctic","LIN04-C.,LIN04-N.",
                  ifelse(share$reportingName=="Pandalus borealis" & share$area=="IV" & share$region=="North Sea and Eastern Arctic","PRA2AC4-C, PRA04-N.",
                  ifelse(share$reportingName=="Pollachius virens" & share$area=="I, II" & share$region=="North Sea and Eastern Arctic","POK1/2INT,POK1N2AB.,POK2A34.",
                  ifelse(share$reportingName=="Pollachius virens" & share$area=="IV" & share$region=="North Sea and Eastern Arctic","POK2A34.,POK04-N.",
                  ifelse(share$reportingName=="Pollachius virens" & share$area=="IV, IIIa, VI" & share$region=="North Atlantic","POK56-14, POK2A34.,POK04-N.",
                  ifelse(share$reportingName=="Scomber scombrus" & share$area=="II" & share$region=="North Sea and Eastern Arctic","MAC2A34.,MAC2A4A-N",
                  ifelse(share$reportingName=="Scomber scombrus" & share$area=="II, IIIa, IV, V, VI, VII, VIII, IX" & share$region=="North Atlantic","MAC2CX14-,MAC2A34.,MAC2A4A-N",
                  ifelse(share$reportingName=="Scomber scombrus" & share$area=="IV, VIId" & share$region=="North Sea and Eastern Arctic","MAC2A34.,MAC2A4A-N",
                  ifelse(share$reportingName=="Trisopterus esmarkii" & share$area=="IV" & share$region=="North Sea and Eastern Arctic","NOP04-N.,NOP2A3A4.",
                         "NA")))))))))))))))))))))))))

check<-subset(share,stockID_new!="NA")

share$stockIDComm<-ifelse(share$stockID_new!="NA","stockID changed", "NA")
share$stockID_new<-ifelse(share$stockID_new=="NA", share$stockID, share$stockID_new)

names(share)

share_new<-rename(share, c("stockID"="stockID_old", "stockID_new"="stockID"))

names(share_old)
names(share_new)

share_new<-share_new[, c("region","sppName","latinName","stockID","area","areaBis","TAC.area.description","reportingName","latinName_old","stockID_old")]

write.table(share_new,file.path(path, "EUMAP_Table1A_Linkage_EUROSTAT and EC_TAC_DNK_v3.csv"), sep=";",row.names=FALSE)
