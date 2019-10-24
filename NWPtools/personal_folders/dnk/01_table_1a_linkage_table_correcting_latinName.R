
#Based on 1_Table1A_latinName_vs_ASFIS from 2016

library(dplyr)

input_path_common <- "Q:/scientific-projects/eu-data-collection/Work_Plan/2020/scripts/gits/RCGs/NWPtools/common/input"
input_path_dnk <- "Q:/scientific-projects/eu-data-collection/Work_Plan/2020/scripts/gits/RCGs/NWPtools/personal_folders/dnk/input"


linkage <- read.csv(file.path(input_path_common, 'EUMAP_Table1A_Linkage_EUROSTAT and EC_TAC.csv'), sep = ";", header = T)

linkage <- subset(linkage, latinName!="Elasmobranchii")

ASFIS   <- read.table(file.path(input_path_common,'ASFIS_sp_Feb_2016.txt'), header=TRUE, sep="\t", as.is=TRUE)
ASFIS$latinName <- ASFIS$Scientific_name

#Compare latinName to ASFIS

asfis <- merge(linkage,ASFIS,all.x=T,by.x=c("latinName"),by.y=c("latinName"))
asfisNo<-subset(asfis,is.na(Scientific_name))

#run spp program to find the correct species

asfis$latinName_new<-ifelse(asfis$latinName=="Ammodytidae", "Ammodytes spp,Ammodytes tobianus", 
                            ifelse(asfis$latinName=="Anarhichas spp", "Anarhichas spp,Anarhichas minor,Anarhichas denticulatus,Anarhichas lupus", 
                            ifelse(asfis$latinName=="Aphanopus spp", "Aphanopus spp,Aphanopus intermedius,Aphanopus carbo",
                            ifelse(asfis$latinName=="Argentina spp","Argentina spp,Argentina sphyraena,Argentina silus,Argentina kagoshimae,Argentina elongata",
                            ifelse(asfis$latinName=="Aristeomorpha foliacea", "Aristaeomorpha foliacea",
                            ifelse(asfis$latinName=="Beryx spp", "Beryx spp,Beryx splendens,Beryx decadactylus",
                            ifelse(asfis$latinName=="Illex spp, Todarodes spp.", "Illex spp,Todarodes spp,Illex oxygonius,Illex argentinus,Illex coindetii,Illex illecebrosus,Todarodes angolensis,Todarodes pacificus,Todarodes filippovae,Todarodes sagittatus",
                            ifelse(asfis$latinName=="Capros aper", "Capros aper,Caproidae",       
                            ifelse(asfis$latinName=="Pandalus spp", "Pandalus spp,Pandalus amplus,Pandalus nipponensis,Pandalus kessleri,Pandalus goniurus,Pandalus danae,Pandalus montagui,Pandalus jordani,Pandalus borealis,Pandalus platyceros,Pandalus hypsinotus",
                            ifelse(asfis$latinName=="Scomber colias", "Scomber japonicus",
                            ifelse(asfis$latinName=="Scomber spp", "Scomber spp,Scomber australasicus,Scomber scombrus,Scomber japonicus",
                            ifelse(asfis$latinName=="Solea vulgaris","Solea solea",
                            ifelse(asfis$latinName=="Trigla lucerna", "Chelidonichthys lucerna",
                            ifelse(asfis$latinName=="Trisopterus esmarki", "Trisopterus esmarkii",
                            ifelse(asfis$latinName=="Trisopterus spp", "Trisopterus spp,Trisopterus luscus,Trisopterus minutus,Trisopterus esmarkii",
                                  as.character(asfis$latinName))))))))))))))))

asfis$latinComm<-ifelse(is.na(asfis$X3A_CODE),"latinName_old not in ASFIS", "NA")

names(asfis)

linkage_new <- mutate(asfis, latinName_old = latinName, latinName = latinName_new)

names(linkage)
names(linkage_new)

for (i in 1:nrow(linkage_new)) {
  linkage_new$reportingName[i] <- unlist(strsplit(linkage_new$latinName[i], split=","))[1]
  }

linkage_new<-linkage_new[, c("region","sppName","latinName","RFMO","RFMO_Stock_ID","FIDES_stockID","area","areaBis",
                         "TAC.area.description","Comments","reportingName","latinName_old")]

asfis_new<-merge(linkage_new,ASFIS,all.x=T,by.x=c("latinName"),by.y=c("latinName"))
asfis_newNo<-subset(asfis_new,is.na(Scientific_name))

write.table(linkage_new,file.path(input_path_dnk,"EUMAP_Table1A_Linkage_EUROSTAT and EC_TAC_dnk.csv"), sep=";",row.names=FALSE)
