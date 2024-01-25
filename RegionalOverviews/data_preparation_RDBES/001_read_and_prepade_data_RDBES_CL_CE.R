# based on the 001_read_and_prepare_data_rdb_2009_2021_CL_CE.r
# script prepares datasets for further analysis

rm(list=ls())
library(data.table)
gc()
getwd()

 
################################################################################################################################################################
################################################################################################################################################################
#
#                                 SER PREP OPTIONS
#
################################################################################################################################################################
################################################################################################################################################################

# ========================
# set params
# ======================== 

target_region <- "RCG_NSEA" # RCG_BA, RCG_NA, RCG_NSEA
year_start <- 2021
year_end <- 2021
time_tag<-format(Sys.time(), "%Y%m%d")

# ========================
# downloads data from sharepoint
# ======================== 

# either download the data manually, or use the function below (some adjustment might be needed)
# source("funs/func_download_data_from_sharepoint.r")

# ========================
# reads in data
# ========================

# reads aux_countries dataset
aux_countries<-read.table("RegionalOverviews\\data\\aux_countries.txt", sep=",", header=T, colClasses="character", na.strings = "")

# reads RDBES data
RDBESdataPath = 'RegionalOverviews/data_RDBES/001_raw'

file_cl <- paste(RDBESdataPath, "/RDBES CL/CommercialLanding.csv", sep = '')
file_ce <- paste(RDBESdataPath, "/RDBES CE/CommercialEffort.csv" , sep = '')

# read data
ce<-data.table::fread(file_ce, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=",", na.strings="NULL")
cl<-data.table::fread(file_cl, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=",", na.strings="NULL")

# QCA: duplicates (eliminates if existing)
dim(cl); cl<-unique(cl); dim(cl)
dim(ce); ce<-unique(ce); dim(ce)

# ====================== 
# create directory structure
# ====================== 

if (!dir.exists(paste("RegionalOverviews/data_RDBES/002_prepared/", time_tag ,"2021/RCG_NA", sep=""))){
  dir.create(paste("RegionalOverviews/data_RDBES/002_prepared/", time_tag ,"/RCG_NA", sep=""),recursive=TRUE, showWarnings=FALSE)
}
if (!dir.exists(paste("RegionalOverviews/data_RDBES/002_prepared/", time_tag ,"/RCG_BA", sep=""))){
  dir.create(paste("RegionalOverviews/data_RDBES/002_prepared/", time_tag ,"/RCG_BA", sep=""),recursive=TRUE, showWarnings=FALSE)
}
if (!dir.exists(paste("RegionalOverviews/data_RDBES/002_prepared/", time_tag ,"/RCG_NSEA", sep=""))){
  dir.create(paste("RegionalOverviews/data_RDBES/002_prepared/", time_tag ,"/RCG_NSEA", sep=""),recursive=TRUE, showWarnings=FALSE)
}

################################################################################################################################################################
################################################################################################################################################################
#
#                                 COUNTRY SPECIFIC CORRECTIONS
#
################################################################################################################################################################
################################################################################################################################################################
# this should be fulfilled after generating the first versions of the overviews
# when some issues to be fixed are noticed 


################################################################################################################################################################
################################################################################################################################################################
#
#                                 BASIC CHECKS <------ TO BE DONE
#
################################################################################################################################################################
################################################################################################################################################################

# ======================
# Tweak on areas/region/ harbour/...
# ====================== 
# is it ISSG responsible for correction of the data?

# <----------------------------------------------------------------------------- to be done


################################################################################################################################################################
################################################################################################################################################################
#
#                                 FORMATS VARIABLES
#
################################################################################################################################################################
################################################################################################################################################################

# formats CL 
cl[,CLlandingLocation:=iconv(CLlandingLocation, from="UTF-8", to="")]
cl[,CLlandingLocation:=toupper(CLlandingLocation)]
cl[,CLscientificWeight:=as.numeric(CLscientificWeight)]

# formats CE 
ce[,CElandingLocation:=iconv(CElandingLocation, from="UTF-8", to="")]
ce[,CElandingLocation:=toupper(CElandingLocation)]


################################################################################################################################################################
################################################################################################################################################################
#
#                                 CREATE NEW VARIABLES
#
################################################################################################################################################################
################################################################################################################################################################

# ======================
# CL
# ======================

# OfficialLandingCatchWeight_1000ton
cl[,CLscientificWeight_ton := CLscientificWeight/1000]
cl[,CLscientificWeight_1000ton := CLscientificWeight/1000000]

# fleet segment (FlagCountry_Loa)
cl[,FlagCountry_Loa:=paste(CLvesselFlagCountry, CLvesselLengthCategory, sep="_")]

# HarbourCountry (ISO3) and HarbourCountry2 (ISO2)
cl[,HarbourCountry2:=substring(CLlandingLocation,1,2)]
cl[,HarbourCountry:=aux_countries$ISO3Code[match(HarbourCountry2, aux_countries$ISO2Code)]]

# HarbourCountry (ISO3) and HarbourCountry2 (ISO2)
cl[is.na(HarbourCountry) & !CLlandingCountry %in% c('*HS','WK1'),HarbourCountry:=CLlandingCountry]
cl[is.na(HarbourCountry) & !CLlandingCountry %in% c('*HS','WK1'),HarbourCountry2:=aux_countries$ISO2Code[match(HarbourCountry, aux_countries$ISO3Code)]]

# QCA: should yield TRUE otherwise debug on cl and cl_rcg
nrow(cl[is.na(HarbourCountry) & !is.na(HarbourCountry2),]) == 0

# 'BEN' (Benim - 'BJ' according to UNLOCODE lists), 
# 'ESH' (Western Sahara - 'EH' according to UNLOCODE lists) 
# are not included in the file 'aux_countries' - 
# - changed with code lines:'GM' ('GMB' - Gambia) and 'GW' ('GNB' - Guine Bissau)	

cl[HarbourCountry2 %in% c("GM"), HarbourCountry := "GMB"]
cl[HarbourCountry2 %in% c("GW"), HarbourCountry := "GNB"]
cl[HarbourCountry2 %in% c("EH"), HarbourCountry := "ESH"]
cl[HarbourCountry2 %in% c("BJ"), HarbourCountry := "BEN"]

# QCA: should yield TRUE otherwise debug on cl and cl_rcg
nrow(cl[is.na(HarbourCountry) & !is.na(HarbourCountry2),]) == 0

# ======================
# CE 
# ======================

# KWDays_thousands
ce[,CEscientifickWDaysAtSea_1000x := CEscientifickWDaysAtSea/1000]				
# GTDays_thousands
ce[,CEgTDaysAtSea_1000x := CEgTDaysAtSea/1000]

# fleet segment (FlagCountry_Loa)	
ce[,FlagCountry_Loa:=paste(CEvesselFlagCountry, CEvesselLengthCategory, sep="_")]

# HarbourCountry (ISO3) and HarbourCountry2 (ISO2)			
ce[,HarbourCountry2:=substring(CElandingLocation,1,2)]
ce[,HarbourCountry:=aux_countries$ISO3Code[match(HarbourCountry2, aux_countries$ISO2Code)]]

# HarbourCountry (ISO3) and HarbourCountry2 (ISO2)
ce[is.na(HarbourCountry) & !CElandingCountry %in% c('*HS','WK1'),HarbourCountry:=CElandingCountry]
ce[is.na(HarbourCountry) & !CElandingCountry %in% c('*HS','WK1'),HarbourCountry2:=aux_countries$ISO2Code[match(HarbourCountry, aux_countries$ISO3Code)]]

# QCA: should yield TRUE otherwise debug on ce and ce_rcg
nrow(ce[is.na(HarbourCountry) & !is.na(HarbourCountry2),]) == 0

# 'BEN' (Benim - 'BJ' according to UNLOCODE lists), 
# 'ESH' (Western Sahara - 'EH' according to UNLOCODE lists) 
# are not included in the file 'aux_countries' - 
# - changed with code lines:'GM' ('GMB' - Gambia) and 'GW' ('GNB' - Guine Bissau)	

ce[HarbourCountry2 %in% c("GM"), HarbourCountry := "GMB"]
ce[HarbourCountry2 %in% c("GW"), HarbourCountry := "GNB"]
ce[HarbourCountry2 %in% c("EH"), HarbourCountry := "ESH"]
ce[HarbourCountry2 %in% c("BJ"), HarbourCountry := "BEN"]

# QCA: should yield TRUE otherwise debug on ce and ce_rcg
nrow(ce[is.na(HarbourCountry) & !is.na(HarbourCountry2),]) == 0


################################################################################################################################################################
################################################################################################################################################################
#
#                                 ASSIGN RCG
#
################################################################################################################################################################
################################################################################################################################################################

# ========================
# subsets data and RCG specific preparations
# ========================	

# RCM Baltic: Baltic Sea (ICES areas III b-d)
if(target_region=="RCG_BA") 
{
  print(paste(".subsetting",target_region))
  
  cl_rcg <- cl[ ( grepl('27.3.b',CLarea) | grepl('27.3.c',CLarea) | grepl('27.3.d',CLarea) ) & 
                  CLyear >= year_start & CLyear <= year_end]

  ce_rcg <- ce[ ( grepl('27.3.b',CEarea) | grepl('27.3.c',CEarea) | grepl('27.3.d',CEarea) ) & 
                  CEyear >= year_start & CEyear <= year_end]
}

# RCM NS&EA: the  North  Sea  (ICES  areas  IIIa,  IV  and  VIId),  the  Eastern  Arctic  (ICES  areas  I  and  II),  the  ICES  divisions Va, XII & XIV and the NAFO areas.
if(target_region=="RCG_NSEA") 
{
  print(paste(".subsetting",target_region))
  
  cl_rcg <- cl[ ( grepl('27.1',CLarea) | 
                    grepl('27.2',CLarea) | 
                    grepl('27.3.a',CLarea) | 
                    grepl('27.4',CLarea) | 
                    grepl('27.5.a',CLarea) |
                    grepl('27.7.d',CLarea) | 
                    grepl('27.12',CLarea) | 
                    grepl('27.14',CLarea) | 
                    grepl('21.',CLarea) 
  ) & 
    CLyear >= year_start & CLyear <= year_end]
  
  ce_rcg <- ce[ ( grepl('27.1',CEarea) | 
                    grepl('27.2',CEarea) | 
                    grepl('27.3.a',CEarea) | 
                    grepl('27.4',CEarea) | 
                    grepl('27.5.a',CEarea) |
                    grepl('27.7.d',CEarea) | 
                    grepl('27.12',CEarea) | 
                    grepl('27.14',CEarea) | 
                    grepl('21.',CEarea)   
  ) & 
    CEyear >= year_start & CEyear <= year_end]
}


# RCM NA: the North Atlantic (ICES areas V-X, excluding Va and VIId)
if(target_region=="RCG_NA") 
{
  print(paste(".subsetting",target_region))
  
  cl_rcg <- cl[ ( grepl('27.5',CLarea) | 
                    grepl('27.6',CLarea) | 
                    grepl('27.7',CLarea) | 
                    grepl('27.8',CLarea) | 
                    grepl('27.9',CLarea) |
                    grepl('27.10',CLarea) 
  ) & 
    !grepl('27.5.a', CLarea) & 
    !grepl('27.7.d', CLarea) & 
    CLyear >= year_start & CLyear <= year_end]
  
  ce_rcg <- ce[ ( grepl('27.5',CEarea) | 
                    grepl('27.6',CEarea) | 
                    grepl('27.7',CEarea) | 
                    grepl('27.8',CEarea) | 
                    grepl('27.9',CEarea) |
                    grepl('27.10',CEarea) 
  ) & 
    !grepl('27.5.a', CEarea) & 
    !grepl('27.7.d', CEarea) & 
    CEyear >= year_start & CEyear <= year_end]
}

################################################################################
################################################################################
#
#                                 AREA MAP
#
################################################################################
################################################################################


################################################################################
################################################################################
#
#                                 ISSCAAP
#
################################################################################
################################################################################


################################################################################
################################################################################
#
#                                 CATCH GROUP
#
################################################################################
################################################################################


################################################################################
################################################################################
#
#                                 FACTORIZATION
#
################################################################################
################################################################################


################################################################################
################################################################################
#
#                                 STOCKS
#
################################################################################
################################################################################


################################################################################
################################################################################
#
#                                 SAVE DATA
#
################################################################################
################################################################################




