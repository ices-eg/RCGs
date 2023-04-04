# on the basis of the script prepared by RCG subgroup work on Regional Fisheries and Sampling Overview

# ========================
# reads in data
# ========================
rm(list=ls())
library(data.table)

# read file names
#file_cl <- "..\\RCG LDF Data\\RCG_LDF_09072020\\LDF_CL_Landings2014-2019.csv"
#file_ce <- "..\\RCG LDF Data\\RCG_LDF_09072020\\LDF_CE_Effort2014-2019.csv"

# NOTE: I got the data as the excel files. I saved it as csv and had to replace all ",," with ",NULL,"
file_cl <- "..\\RCG LDF Data\\RCG_LDF_14072020\\CL Landing table LDF 2014 2019.csv"
file_ce <- "..\\RCG LDF Data\\RCG_LDF_14072020\\CE Effort table LDF 2014 2019.csv" 

# read data
ce<-fread(file_ce, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=",", na.strings="NULL")
cl<-fread(file_cl, stringsAsFactors=FALSE, verbose=FALSE, fill = TRUE, sep=",", na.strings="NULL")

# reads aux_countries dataset
aux_countries<-read.table("data/aux_countries.txt", sep=",", header=T, colClasses="character", na.strings = " ")
# bug with Namibia fixed with na.strings  = " "

# QCA: duplicates (eliminates if existing)
dim(cl); cl<-unique(cl); dim(cl)
dim(ce); ce<-unique(ce); dim(ce)

# ====================== 
# Set Prep Options 
# ======================  

target_region <- "RCG_LDF"
year_start <- 2014
year_end <- 2019
dir_output_rcg<-paste("..\\RCG LDF Data",target_region,sep="")
dir_output_all<-"..\\RCG LDF Data"

# ========================
# add Region information
# ========================	

cl$Region = 'LDF'
ce$Region = 'LDF'

# ========================
# formats variables
# ======================== 

# formats CL 
cl[,HarbourDesc:=iconv(Harbour, from="UTF-8", to="")] # in 2020 in the first ver of the data there was Harour given, however in the next version it was missing
cl[,HarbourDesc:=toupper(HarbourDesc)]
cl[,OfficialLandingCatchWeight:=as.numeric(OfficialLandingCatchWeight)]

# formats CE 
ce[,HarbourDesc:=iconv(Harbour, from="UTF-8", to="")]
ce[,HarbourDesc:=toupper(HarbourDesc)]

# ========================	
# Creates additional variables
# ========================	

# CL

# OfficialLandingCatchWeight_1000ton
cl[,OfficialLandingCatchWeight_ton := OfficialLandingCatchWeight/1000]
cl[,OfficialLandingCatchWeight_1000ton := OfficialLandingCatchWeight/1000000]
# fleet segment (FlagCountry_Loa)
cl[,FlagCountry_Loa:=paste(FlagCountry, vesselLengthCategory, sep="_")]
# HarbourCountry (ISO3) and HarbourCountry2 (ISO2)
cl[,HarbourCountry2:=substring(Harbour,1,2)]
cl[,HarbourCountry:=aux_countries$ISO3Code[match(HarbourCountry2, aux_countries$ISO2Code)]]
# QCA: should yield TRUE otherwise debug on cl and cl_rcg
nrow(cl[is.na(HarbourCountry) & !is.na(HarbourCountry2),]) == 0

# CE 
# KWDays_thousands
ce[,KWDays_1000x := KWDays/1000]				

# GTDays_thousands
ce[,GTDays_1000x := GTDays/1000]		

# fleet segment (FlagCountry_Loa)	
ce[,FlagCountry_Loa:=paste(FlagCountry, VesselLengthCategory, sep="_")]

# HarbourCountry (ISO3) and HarbourCountry2 (ISO2)			
ce[,HarbourCountry2:=substring(Harbour,1,2)]

ce[,HarbourCountry:=aux_countries$ISO3Code[match(HarbourCountry2, aux_countries$ISO2Code)]]

# QCA: should yield TRUE otherwise debug on ce and ce_rcg
nrow(ce[is.na(HarbourCountry) & !is.na(HarbourCountry2),]) == 0

# AREAS 

# in 2020 all analysis was done on Area level
# in 2021 according to the datacall there should be Division level
sort(unique(cl$Area))

# Inconsistency in areas
# some countries reported SubDivisions -> converted into Divisions
# some countries reported Area insted of Division (NLD reported 34)
#  for 34 - will manage to get Division from Subpolygon column, more or  less. 
#  For other areas - will not be able

# only NLD reported Area 34 - in 2019 and previous years
# only DEU and NLD reported Area 87 in previous years


# Division level

# cl
cl[Area %in% c("34.1.1.1", "34.1.1.2", "34.1.1.3",  "34.1.1"), Division := "34.1.1"]
cl[Area %in% c("34.1.2"), Division := "34.1.2"]
cl[Area %in% c("34.1.3.1", "34.1.3.2", "34.1.3"), Division := "34.1.3"]
cl[Area %in% c("34.2") & is.na(SubPolygon), Division := "34.2.0"] 
cl[Area %in% c("34.3.1.1", "34.3.1.2", "34.3.1.3", "34.3.1"), Division := "34.3.1"]
cl[Area %in% c("34.3.3"), Division := "34.3.3"]
cl[Area %in% c("34.3.6"), Division := "34.3.6"]
cl[Area %in% c("34") & SubPolygon == 'Morocco', Division := "34.1.1"] # inconsistency, either 34.1.1 or 34.1.3. As 34.1.1 apeears much more often I assigned this one
cl[Area %in% c("34") & SubPolygon == 'Guinea', Division := "34.3.1"]
cl[Area %in% c("34") & SubPolygon == 'Canaries', Division := "34.1.2"]

cl[Area %in% c("41.3.1"), Division := "41.3.1"]
cl[Area %in% c("41.3.2"), Division := "41.3.2"]

cl[Area %in% c("47.1.1"), Division := "47.1.1"]
cl[Area %in% c("47.1.2"), Division := "47.1.2"]
cl[Area %in% c("47.1.3"), Division := "47.1.3"]
cl[Area %in% c("47.1.4"), Division := "47.1.4"]
cl[Area %in% c("47.1.5"), Division := "47.1.5"]

cl[Area %in% c("87.2.6"), Division := "87.2.6"]
cl[Area %in% c("87.3.3"), Division := "87.3.3"]

cl[Area %in% c("41", "87"), Division := NA] # too low
cl[Area %in% c("34") & is.na(SubPolygon), Division := NA] # ask Sieto


## ce
ce[Area %in% c("34.1.1.1", "34.1.1.2", "34.1.1.3",  "34.1.1"), Division := "34.1.1"]
ce[Area %in% c("34.1.2"), Division := "34.1.2"]
ce[Area %in% c("34.1.3.1", "34.1.3.2", "34.1.3"), Division := "34.1.3"]
ce[Area %in% c("34.2") & is.na(SubPolygon), Division := "34.2.0"] 
ce[Area %in% c("34.3.1.1", "34.3.1.2", "34.3.1.3", "34.3.1"), Division := "34.3.1"]
ce[Area %in% c("34.3.3"), Division := "34.3.3"]
ce[Area %in% c("34.3.6"), Division := "34.3.6"]
ce[Area %in% c("34") & SubPolygon == 'Morocco', Division := "34.1.1"] # # inconsistency, either 34.1.1 or 34.1.3
ce[Area %in% c("34") & SubPolygon == 'Guinea', Division := "34.3.1"]
ce[Area %in% c("34") & SubPolygon == 'Canaries', Division := "34.1.2"]

ce[Area %in% c("41.3.1"), Division := "41.3.1"]
ce[Area %in% c("41.3.2"), Division := "41.3.2"]

ce[Area %in% c("47.1.1"), Division := "47.1.1"]
ce[Area %in% c("47.1.2"), Division := "47.1.2"]
ce[Area %in% c("47.1.3"), Division := "47.1.3"]
ce[Area %in% c("47.1.4"), Division := "47.1.4"]
ce[Area %in% c("47.1.5"), Division := "47.1.5"]

ce[Area %in% c("87.2.6"), Division := "87.2.6"]
ce[Area %in% c("87.3.3"), Division := "87.3.3"]

ce[Area %in% c("41", "87"), Division := NA] # too low
ce[Area %in% c("34") & is.na(SubPolygon), Division := NA] # ask Sieto


# Area level
cl[,AreaOrg := Area]
ce[,AreaOrg := Area]

cl[,Area:=stringr::str_sub(AreaOrg, end = 2),]
ce[,Area:=stringr::str_sub(AreaOrg, end = 2),]

# before, the mapping variable was called AreaMap
# cl[,AreaMap:=Area2,]
# ce[,AreaMap:=Area2,]

# QCA: visual
cl[, list(N=.N,ton1000 = round(sum(OfficialLandingCatchWeight_1000ton),1)),list(Area,Division)][order(Area)]
cl[, list(N=.N,ton1000 = round(sum(OfficialLandingCatchWeight_1000ton),1)),list(Area,Division, FlagCountry, Year)][order(Area)][Area=="NA",] #<- copy
ce[, list(N=.N,TripsNumber = sum(TripsNumber)),list(Area,Division)][order(Area)]
ce[, list(N=.N,TripsNumber = sum(TripsNumber)),list(Area,Division, FlagCountry, Year)][order(Area)][Area=="NA",]

# ========================	
# Creates and tweaks ISSCAAP codes
# ========================		
ASFIS_WoRMS_updt <- read.table (file="data/ASFIS_WoRMS_updt.csv", header=T, sep=";", stringsAsFactors=FALSE) # on the data sharepoint of the RCG subgroup


# =====================
# CL [note: at the moment this is only being run on cl_rcg, not on the larger cl]
# =====================		

cl[,ISSCAAP:=ASFIS_WoRMS_updt$ISSCAAP[match(cl$SpeciesAphiaID, ASFIS_WoRMS_updt$AphiaID_accepted)]]

# QCA should yield zero, if not more tweaks are needed
sum(is.na(cl$Species))
sum(is.na(cl$SpeciesAphiaID))

# QCA should yield zero, if not more tweaks are needed
sum(is.na(cl$ISSCAAP))

cl[Species == "Cottus gobio",ISSCAAP:=13] # Miscellaneous freshwater fishes
cl[Species == "Gasterosteidae",ISSCAAP:=25] # Miscellaneous diadromous fishes
cl[Species == "Gaidropsarus guttatus",ISSCAAP:=32] # Cods, hakes, haddocks
cl[Species == "Mullus barbatus",ISSCAAP:=33] # Miscellaneous coastal fishes
cl[Species == "Auxis rochei",ISSCAAP:=36] # Tunas, bonitos, billfishes
cl[Species == "Auxis thazard",ISSCAAP:=36] # Tunas, bonitos, billfishes
cl[Species == "Scombrinae Rafinesque",ISSCAAP:=36] # Tunas, bonitos, billfishes
cl[Species == "Scomberesox saurus",ISSCAAP:=37] # Miscellaneous pelagic fishes
cl[Species == "Selachii",ISSCAAP:=38] # Sharks, rays, chimaeras
cl[Species == "Squatina",ISSCAAP:=38] # Sharks, rays, chimaeras
cl[Species == "Rajella lintea",ISSCAAP:=38] # Sharks, rays, chimaeras
cl[Species == "Pisces",ISSCAAP:=39] # Marine fishes not identified
cl[Species == "Macropodia",ISSCAAP:=42] # Crabs, sea-spiders
cl[Species == "Dardanus arrosor",ISSCAAP:=44] # King crabs, squat-lobsters
cl[Species == "Pasiphaea",ISSCAAP:=45] # Shrimps, prawns
cl[Species == "Pasiphaeidae",ISSCAAP:=45] # Shrimps, prawns
cl[Species == "Dendrobranchiata",ISSCAAP:=45] # Shrimps, prawns
cl[Species == "Crangon",ISSCAAP:=45] # Shrimps, prawns
cl[Species == "Decapodiformes",ISSCAAP:=47] # Miscellaneous marine crustaceans
cl[Species == "Megabalanus azoricus",ISSCAAP:=47] # Miscellaneous marine crustaceans
cl[Species == "Gibbula",ISSCAAP:=52] # Abalones, winkles, conchs
cl[Species == "Venerupis philippinarum",ISSCAAP:=56] # Clams, cockles, arkshells
cl[Species == "Arcopagia crassa",ISSCAAP:=56] # Clams, cockles, arkshells
cl[Species == "Loligo forbesii",ISSCAAP:=57] # Squids, cuttlefishes, octopuses
cl[Species == "Echinidae",ISSCAAP:=76] # Sea-urchins and other echinoderms
cl[Species == "Laminaria",ISSCAAP:=91] # Brown seaweeds
cl[Species == "Patagonotothen brevicauda",ISSCAAP:=34]
# new:
cl[Species == "Sepiolidae",ISSCAAP:=57]
cl[Species == "Sepiida",ISSCAAP:=57]
cl[Species == "Scorpaeniformes",ISSCAAP:=34]

#cl[Species == "Plantae",ISSCAAP:=34] <- ask Sieto
#cl[Species == "Seriola",ISSCAAP:=34] #Amberjacks nei


# QCA should be zero
sum(is.na(cl$ISSCAAP))
# code for debugging:
unique(cl[is.na(ISSCAAP)]$Species)


# Ammodytes and Norway Pout appear classified as "33" - better assign to "37"
cl[grepl(Species, pat= "Ammodytes"),ISSCAAP:=37] # Miscellaneous pelagic fishes
cl[grepl(Species, pat= "Ammodytidae"),ISSCAAP:=37]  # Miscellaneous pelagic fishes
cl[grepl(Species, pat= "Trisopterus esmarkii"),ISSCAAP:=37] # Miscellaneous pelagic fishes

# Actinopterygii 39
cl[grepl(Species, pat= "Actinopterygii"),ISSCAAP:=39]




# Adds RCG Catch_group
aux_spp_categ<-read.table("data/Table_Species_Categ.txt", header=T, sep="\t")
cl[,Catch_group:=aux_spp_categ$RCM_NSEA_categ[match(cl$ISSCAAP,aux_spp_categ$ISSCAAP)]]


# QCA should be zero			
sum(is.na(cl$Catch_group))
unique(cl[is.na(Catch_group)]$Species)


# some additional tweaks
cl[grepl(Species, pat="Trachurus"),Species:="Trachurus spp."]
cl[grepl(Species, pat="Lepidorhombus"),Species:="Lepidorhombus spp."]
cl[grepl(Species, pat="Lophi"),Species:="Lophiidae"]

cl[Species == "Myxine glutinosa",Catch_group:="other"]
cl[Species == "Lichia amia",Catch_group:="large pelagic"]
cl[Species == "Acipenser sturio",Catch_group:="diadromous"]

sum(is.na(cl$Catch_group))

# give it a check (see if it makes sense)
# check demersal
head(cl[Catch_group == "demersal",list(Kg=sum(OfficialLandingCatchWeight)),list(Species)] [order(-Kg),],20)
# check flatfish
head(cl[Catch_group == "flatfish",list(Kg=sum(OfficialLandingCatchWeight)),list(Species)] [order(-Kg),],20)
# check small pelagic
head(cl[Catch_group == "small pelagic",list(Kg=sum(OfficialLandingCatchWeight)),list(Species)] [order(-Kg),],20)
# check large pelagic
head(cl[Catch_group == "large pelagic",list(Kg=sum(OfficialLandingCatchWeight)),list(Species)] [order(-Kg),],20)
# check molluscs
head(cl[Catch_group == "molluscs",list(Kg=sum(OfficialLandingCatchWeight)),list(Species)] [order(-Kg),],20)
# check crustaceans
head(cl[Catch_group == "crustaceans",list(Kg=sum(OfficialLandingCatchWeight)),list(Species)] [order(-Kg),],20)
# check	elasmobranchs
head(cl[Catch_group == "elasmobranchs",list(Kg=sum(OfficialLandingCatchWeight)),list(Species)] [order(-Kg),],20)
# check	diadromous
head(cl[Catch_group == "diadromous",list(Kg=sum(OfficialLandingCatchWeight)),list(Species)] [order(-Kg),],20)
# check	incidental by-catch
head(cl[Catch_group == "incidental by-catch",list(Kg=sum(OfficialLandingCatchWeight)),list(Species)] [order(-Kg),],20)			
# check	other
head(cl[Catch_group == "other",list(Kg=sum(OfficialLandingCatchWeight)),list(Species)] [order(-Kg),],20)


# ========================
# factorization [establishes the order in unsorted bar graphs]
# ========================

cl[,FlagCountry:=factor(FlagCountry, levels=sort(unique(FlagCountry))),]
cl[,LandingCountry:=factor(LandingCountry, levels=sort(unique(LandingCountry))),]
cl[,FishingActivityCategoryEuropeanLvl5:=factor(FishingActivityCategoryEuropeanLvl5, levels=sort(unique(FishingActivityCategoryEuropeanLvl5))),]
cl[,FishingActivityCategoryEuropeanLvl6:=factor(FishingActivityCategoryEuropeanLvl6, levels=sort(unique(FishingActivityCategoryEuropeanLvl6))),]
cl[,Harbour:=factor(Harbour, levels=sort(unique(Harbour))),]
cl[,Species:=factor(Species, levels=sort(unique(Species))),]
cl[,VesselLengthCategory:=factor(vesselLengthCategory, levels=c("<10","10-<12","12-<18","18-<24","24-<40",">40"))]

ce[,FlagCountry:=factor(FlagCountry, levels=sort(unique(FlagCountry))),]
ce[,FishingActivityCategoryEuropeanLvl5:=factor(FishingActivityCategoryEuropeanLvl5, levels=sort(unique(FishingActivityCategoryEuropeanLvl5))),]
ce[,FishingActivityCategoryEuropeanLvl6:=factor(FishingActivityCategoryEuropeanLvl6, levels=sort(unique(FishingActivityCategoryEuropeanLvl6))),]
ce[,Harbour:=factor(Harbour, levels=sort(unique(Harbour))),]
ce[,VesselLengthCategory:=factor(VesselLengthCategory, levels=c("<10","10-<12","12-<18","18-<24","24-<40",">40"))]
ce[,DaysAtSea:=as.numeric(DaysAtSea)]
ce[,KWDays:=as.numeric(KWDays)]
ce[,GTDays:=as.numeric(GTDays)]
ce[,KWDays_1000x:=as.numeric(KWDays_1000x)]
ce[,GTDays_1000x:=as.numeric(GTDays_1000x)]


# take all data from RCG LDF
# target_Areas<-sort(unique(unique(cl$Area), unique(ce$Area)))
# cl[,Area:=factor(Area, levels=target_Areas),]
# ce[,Area:=factor(Area, levels=target_Areas),]
#   
# target_AreaMap<-sort(unique(unique(cl$AreaMap), unique(ce$AreaMap)))
# cl[,AreaMap:=factor(AreaMap, levels=target_AreaMap),]
# ce[,AreaMap:=factor(AreaMap, levels=target_AreaMap),]
#   


# QCA: visual
cl[, list(N=.N,ton1000 = round(sum(OfficialLandingCatchWeight_1000ton),1)),list(Area,Division)][order(Area)]
cl[, list(N=.N,ton1000 = round(sum(OfficialLandingCatchWeight_1000ton),1)),list(Area,Division, FlagCountry, Year)][order(Area)][is.na(Area),]
ce[, list(N=.N,TripsNumber = sum(TripsNumber)),list(Area,Division)][order(Area)]
ce[, list(N=.N,TripsNumber = sum(TripsNumber)),list(Area,Division, FlagCountry, Year)][order(Area)][is.na(Area),]

# ================
# quick and dirty check
# ================	
  
  # country level
  
  test_ctry<-"POL"
  
  cl[FlagCountry==test_ctry,sum(OfficialLandingCatchWeight),list(Year)]
  ce[FlagCountry==test_ctry,sum(TripsNumber),list(Year)]
  
  
# all countries: 2018 and 2019
cl[Year %in% c(2017,2018,2019),sum(OfficialLandingCatchWeight),list(FlagCountry, Year)] [order(FlagCountry, Year)]
ce[Year %in% c(2017,2018,2019),sum(TripsNumber),list(FlagCountry, Year)] [order(FlagCountry, Year)]
# missing data 2019: GBR, ITA, IRL

# ================
# creates a few additional variables with shorter names (convenient for titles of barplot and maps sake)
# ================		
# note: duplication to be avoided in the future after group discussion

cl[, LandingWeight_ton:=OfficialLandingCatchWeight_ton]    

cl[, LandingWeight_1000ton:=OfficialLandingCatchWeight_1000ton]    


cl[, FishingActivityLvl5:=FishingActivityCategoryEuropeanLvl5]    


cl[, FishingActivityLvl6:=FishingActivityCategoryEuropeanLvl6]    


ce[, FishingActivityLvl5:=FishingActivityCategoryEuropeanLvl5]    


ce[, FishingActivityLvl6:=FishingActivityCategoryEuropeanLvl6]    



# ========================
# saves data
# ========================	

file_info_cl<-file.info(file_cl)
file_info_ce<-file.info(file_ce)

time_tag<-format(Sys.time(), "%Y%m%d%H%M")
#time_tag<-201905101612


save(cl, file_info_cl, file = paste(dir_output_all, paste("\\RDB","LDF","CL", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
save(ce, file_info_ce, file = paste(dir_output_all, paste("\\RDB","LDF","CE", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))



