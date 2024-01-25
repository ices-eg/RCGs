# based on the 001_read_and_prepare_data_rdb_2009_2021_CL_CE.r

# script prepares datasets for further analysis

# 2024-01-11 first ver of this script is created, for the RDBES 2021 CL and CE data
# ...

rm(list=ls())
library(data.table)
gc()
getwd() #display the directory

# ====================== 
# Set Prep Options 
# ======================  

target_region <- "RCG_NSEA" # RCG_BA, RCG_NA, RCG_NSEA
year_start <- 2021
year_end <- 2021
time_tag<-format(Sys.time(), "%Y%m%d")

# ========================
# downloads data from sharepoint
# ======================== 

# either download the data manually, or use the function below (some adjustment might be needed)
# source("funs/func_download_data_from_sharepoint.r")

# is it needed?
# reads aux_countries dataset
# aux_countries<-read.table(".\\data\\aux_countries.txt", sep=",", header=T, colClasses="character", na.strings = "")

# ========================
# reads in data
# ========================

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

# ======================
# Tweak on areas/region 
# ====================== 
# is it ISSG responsible for correction of the data?

# CL and CE 

# areas
sort(unique(ce$CEarea))
sort(unique(cl$CLarea))
# QCA: check for ambiguous records (e.g., 27.7; 27.3)
# Area 27.7
ce[CEarea=="27.7",.N,c("CEyear","CEvesselFlagCountry","CEarea")]
cl[CLarea=="27.7",.N,c("CLyear","CLvesselFlagCountry","CLarea")]
# corrections below:
# ...

#	Area 27.3
ce[CEarea=="27.3",.N,c("CEyear","CEvesselFlagCountry","CEarea")]
cl[CLarea=="27.3",.N,c("CLyear","CLvesselFlagCountry","CLarea")]
# cerrections below:
#... 

# issue in Area
cl[CLarea=="27.3.d.28" & cl$CLvesselFlagCountry=="SWE"]

# issue in Harbour
sort(unique(ce$CElandingLocation))
sort(unique(cl$CLlandingLocation))

cl[CLlandingLocation=="POL-1303"]
ce[CElandingLocation=="POL-1303"]
# corrections below: 
# cl[CLlandingLocation=="POL-1303", Harbour:="RUPNY"]

# also may be worth noting/correcting this
cl[CLlandingLocation=="*HS-*HS"]
ce[CElandingLocation=="*HS-*HS"]			


# ========================
# subsets data and RCG specific preparations
# ========================	
 
# # to do -> check if all the rows that should be assigned to the target_region, are assigned properly


# RCM Baltic: Baltic Sea (ICES areas III b-d)
if(target_region=="RCG_BA") 
{
  print(paste(".subsetting",target_region))
  
  cl_rcg <- cl[ ( grepl('27.3.b',CLarea) | grepl('27.3.c',CLarea) | grepl('27.3.d',CLarea) ) & 
                  CLyear >= year_start & CLyear <= year_end]

  # to do -> check if all the rows that should be assigned to the RCG_BA, are assigned properly

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
  
  # to do -> check if all the rows that should be assigned to the RCG_NSEA, are assigned properly
  
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
  
  # to do -> check if all the rows that should be assigned to the RCG_NSEA, are assigned properly
  
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
