###################################################################
# Annual_overview_RDBES 
###################################################################
#
# By specifying the user´s selection and sourcing relevant scripts,
# this script produces the annual RDBES overview.  
# 
# The main structure of the overview is contained in 
# 
# "annual_overview_RDBES_template.Rmd"
# 
# But the following script should be considered the main source for 
# the RDBES annual overview and should be used to create the
# "annual_overview_RDBES.R" report. 
# 
###################################################################
# Authors: 
# - Marta Suska [first draft]
# - Eros Quesada
# - Kasia Krakówka
# 
# Dev. notes: 
#
# - 20240130: Created  
# - 20240207: Formatted, disassembled in scripts to be sourced. 
# - 20240220: Added fleet register
#
###################################################################

## Custom the overview
# Make your selection 
yearSelected = 2021
regionSelected = 'NSEA'# One of: 'BA', 'NA', 'NSEA'
downloadDataFromSP = 1 # One of: 1 (download from Share Point prepared data) or 0 (do not download and use data prepared locally - using "001_read_and_prepare_data_RDBES_CL_CE.R")
dataprepDate = 20240129 # Date on which data where prepared. If prepared data are downloaded from ICES SP, then this is the date used for the data folder name on the ICES SP. 

## Set wd 
# setwd("//storage-lk.slu.se/home$/erqu0001/Desktop/HLab_GH/Public_Eros/RCGs") # eros machine
setwd("Path to RCGs local repo")

## Load libraries
source("RegionalOverviews/overviews_reports_RDBES/fisheries_overviews_RDBES/scripts/loadLibraries.R")

## Load functions 
source("RegionalOverviews/overviews_reports_RDBES/fisheries_overviews_RDBES/scripts/loadFunctions.R")

# Parameters are defined based on user selection
source("RegionalOverviews/overviews_reports_RDBES/fisheries_overviews_RDBES/scripts/parametersDefinition.R")

## Download prepared data from SP accordingly, if required. 
# If the download is selected (downloadDataFromSP == 1), then prepared data are downloaded from Share Point. 
source("RegionalOverviews/overviews_reports_RDBES/fisheries_overviews_RDBES/scripts/downloadPreparedData.R")

## Load prepared data
source("RegionalOverviews/overviews_reports_RDBES/fisheries_overviews_RDBES/scripts/loadData.R")
source("RegionalOverviews/overviews_reports_RDBES/fisheries_overviews_RDBES/scripts/loadFleetRegister.R")
## Source the .rmd file producing the overview
rmdReport <- file.path("RegionalOverviews/overviews_reports_RDBES/fisheries_overviews_RDBES/annual_overview_RDBES_template.Rmd")
rmarkdown::render(
  rmdReport,
  params = params,
  output_file = paste0('results/AnnualOverview_', params$year ,'_', params$region, '.html'), # reports saved into results folder
  envir = new.env(parent = globalenv()),
  encoding = 'UTF-8'
)