# based on the 001_read_and_prepare_data_rdb_2009_2021_CL_CE.r
# script prepares datasets for further analysis
setwd("Path to RCGs local repo")
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

## ========================
## Set params
## ======================== 

target_region <- "RCG_NSEA" # RCG_BA, RCG_NA, RCG_NSEA
year_start <- 2021
year_end <- 2021
time_tag<-format(Sys.time(), "%Y%m%d")

## ====================== 
## Create directory structure
## ====================== 

dir_output_all<-paste("RegionalOverviews/data_RDBES/002_prepared/", time_tag, sep="")
dir_output_rcg<-paste("RegionalOverviews/data_RDBES/002_prepared/", time_tag ,"/",target_region, sep="")


if (!dir.exists(dir_output_all)){
  dir.create(dir_output_all,recursive=TRUE, showWarnings=FALSE)
}

if (!dir.exists(dir_output_rcg)){
  dir.create(dir_output_rcg,recursive=TRUE, showWarnings=FALSE)
}

## ========================
## Downloads data from sharepoint
## ======================== 
## Here we obtain raw RDBES data. 
#  The preferable choice is to use a function downloading the data from the SharePoint. Alternatively, data are to be manually downloaded. 
source("RegionalOverviews/funs/func_download_data_from_sharepoint.r")
download_data_from_sharepoint(
 sharepoint_address = "Path to directory on SharePoint",
 filename_vector = paste0(target_region, ".zip"), 
 dir_download_browser = "//storage-lk.slu.se/home$/erqu0001/Downloads", # Directory where browser downloads, e.g. on eros machine
 dir_download_target = "Path to directory where data should be stored",  
 unzip=TRUE
 )

# ========================
# reads in data
# ========================

# reads aux_countries dataset
aux_countries<-read.table("RegionalOverviews//data//aux_countries.txt", sep=",", header=T, colClasses="character", na.strings = "")

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

# filter out the proper years
cl<- cl[CLyear >= year_start & CLyear <= year_end]
ce<- ce[CEyear >= year_start & CEyear <= year_end]

################################################################################################################################################################
################################################################################################################################################################
#
#                                 COUNTRY SPECIFIC CORRECTIONS <-------------------------- TO BE DONE
#
################################################################################################################################################################
################################################################################################################################################################
# this should be fulfilled after generating the first versions of the overviews
# when some issues to be fixed are noticed 

# should this part be in this script or a separate one not to mess here every year?


################################################################################################################################################################
################################################################################################################################################################
#
#                                 BASIC CHECKS <---------------------------------- TO BE DONE
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

# QCA: should yield TRUE otherwise debug on cl
nrow(cl[is.na(HarbourCountry) & !is.na(HarbourCountry2),]) == 0

# 'BEN' (Benim - 'BJ' according to UNLOCODE lists), 
# 'ESH' (Western Sahara - 'EH' according to UNLOCODE lists) 
# are not included in the file 'aux_countries' - 
# - changed with code lines:'GM' ('GMB' - Gambia) and 'GW' ('GNB' - Guine Bissau)	

cl[HarbourCountry2 == c("GM"), HarbourCountry := "GMB"]
cl[HarbourCountry2 == c("GW"), HarbourCountry := "GNB"]
cl[HarbourCountry2 == c("EH"), HarbourCountry := "ESH"]
cl[HarbourCountry2 == c("BJ"), HarbourCountry := "BEN"]
cl[HarbourCountry2 == c("PA"), HarbourCountry := "PAN"]
cl[HarbourCountry2 == c("CV"), HarbourCountry := "CPV"]

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

# 'BEN' (Benim - 'BJ' according to UNLOCODE lists), 
# 'ESH' (Western Sahara - 'EH' according to UNLOCODE lists) 
# are not included in the file 'aux_countries' - 
# - changed with code lines:'GM' ('GMB' - Gambia) and 'GW' ('GNB' - Guine Bissau)	

ce[HarbourCountry2 == c("GM"), HarbourCountry := "GMB"]
ce[HarbourCountry2 == c("GW"), HarbourCountry := "GNB"]
ce[HarbourCountry2 == c("EH"), HarbourCountry := "ESH"]
ce[HarbourCountry2 == c("BJ"), HarbourCountry := "BEN"]
ce[HarbourCountry2 == c("PA"), HarbourCountry := "PAN"]
ce[HarbourCountry2 == c("CV"), HarbourCountry := "CPV"]

# QCA: should yield TRUE otherwise debug on ce
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
  
  cl_rcg <- cl[ ( grepl('27.3.b',CLarea) | grepl('27.3.c',CLarea) | grepl('27.3.d',CLarea) )]

  ce_rcg <- ce[ ( grepl('27.3.b',CEarea) | grepl('27.3.c',CEarea) | grepl('27.3.d',CEarea) )]
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
  )]
  
  ce_rcg <- ce[ ( grepl('27.1',CEarea) | 
                    grepl('27.2',CEarea) | 
                    grepl('27.3.a',CEarea) | 
                    grepl('27.4',CEarea) | 
                    grepl('27.5.a',CEarea) |
                    grepl('27.7.d',CEarea) | 
                    grepl('27.12',CEarea) | 
                    grepl('27.14',CEarea) | 
                    grepl('21.',CEarea)   
  )]
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
    !grepl('27.7.d', CLarea)]
  
  ce_rcg <- ce[ ( grepl('27.5',CEarea) | 
                    grepl('27.6',CEarea) | 
                    grepl('27.7',CEarea) | 
                    grepl('27.8',CEarea) | 
                    grepl('27.9',CEarea) |
                    grepl('27.10',CEarea) 
  ) & 
    !grepl('27.5.a', CEarea) & 
    !grepl('27.7.d', CEarea)]
}

################################################################################
################################################################################
#
#                                 AREA MAP <----------------------------------------------------------- to do
#
################################################################################
################################################################################


################################################################################
################################################################################
#
#                                 ISSCAAP <------------------------------------------------------------ not needed anymore?
#
################################################################################
################################################################################


################################################################################
################################################################################
#
#                                 CATCH GROUP <------------------------------------------------------------ WORK ON IT
#
################################################################################
################################################################################

# to assign Catch group to the RDBES data, we're using a table produced by ISSG metiers
# https://github.com/ices-eg/RCGs/blob/master/Metiers/Reference_lists/Metier%20Subgroup%20Species%202020%20cleaned%20version.xlsx

MetierGroupSpecies = openxlsx::read.xlsx('Metiers/Reference_lists/Metier Subgroup Species 2020 cleaned version.xlsx')

# For now, we merge the dataset using FAO code, but this variable is optional <-------------------------------- IMPORTANT!
# So additional table would be needed: WORMS - FAO (where to find it? any official source?)
# then merge with MetierGroupSpecies by FAO key and we get for each row Catch group
# another option? WORMS - ISSCAAP? TaxoCode ? ...?

cl_rcg[,CatchGroup:=MetierGroupSpecies$`Grouping.2.(TO.BE.USED)`[match(cl_rcg$CLspeciesFaoCode, MetierGroupSpecies$FAOcode)]]
# QCA: should yield TRUE otherwise debug
nrow(cl_rcg[is.na(CatchGroup),]) == 0
cl_rcg[is.na(CatchGroup),]

# IF NEEDED -> Full name of the catch group might be taken from this table
# icesVocab::getCodeList("TargetSpecies")

################################################################################
################################################################################
#
#                                 FACTORIZATION <----------------------------------- is it still needed?
#
################################################################################
################################################################################


################################################################################
################################################################################
#
#                                 STOCK <---------------------------------------------- to be done
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

file_info_cl<-file.info(file_cl)
file_info_ce<-file.info(file_ce)

save(cl_rcg, file_info_cl, file = paste(dir_output_rcg, paste("//RDBES",target_region,"CL", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
save(ce_rcg, file_info_ce, file = paste(dir_output_rcg, paste("//RDBES",target_region,"CE", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
save(cl, file_info_cl, file = paste(dir_output_all, paste("//RDBES","All_Regions","CL", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
save(ce, file_info_ce, file = paste(dir_output_all, paste("//RDBES","All_Regions","CE", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))	