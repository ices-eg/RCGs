###################################################################
# downloadPreparedData 
###################################################################
#
# This script downloads from the Sharepoint RDBES CL and CE data 
# in the prepared form.
#  
# These data are downloaded in:
# ~/RCGs/RegionalOverviews/data_RDBES/002_prepared/DATE/REGION
#
# The execution of the download is governed by "downloadDataFromSP"
# in the main script "annual_overview_RDBES". 
#
###################################################################
# Authors: 
# - Eros Quesada [first draft]
# 
# Dev. notes: 
# 
# - 20240207: Created 
#
###################################################################


# Print start message
if(downloadDataFromSP == 1){
  cat("[4]    Data download")
  cat("\n")
} 

# Empty warnings from previous code
assign("last.warning", NULL, envir = baseenv()) # Credits: https://stackoverflow.com/questions/5725106/r-how-to-clear-all-warnings

## Download data 
# Depending on the downloadDataFromSP selection, data are downloaded or not. 
if(downloadDataFromSP == 1){
  cat("       The option downloadDataFromSP was selected, thus data will be downloaded from ICES Share Point.")
  cat("\n")
  cat("\n")
  cat(paste("      ", magenta("!"), "Please note that the downloaded data originate from a folder labelled with date:", dataprepDate)) # Ideally we should be able to download latest prepared data each time. To do this, the function has to be able to list files at the SP url. 
  cat("\n")
  cat("       You can change this modifying the script: RCGs/RegionalOverviews/overviews_reports_RDBES/fisheries_overviews_RDBES/annual_overview_RDBES.R") # Ideally we should be able to download latest prepared data each time. To do this, the function has to be able to list files at the SP url. 
  cat("\n")

  # First, make sure destination folder is available [from "001_read_and_prepare_data_RDBES_CL_CE.R"]
  dir_output_all <- paste("RegionalOverviews/data_RDBES/002_prepared/", dataprepDate, sep="")
  dir_output_rcg <- paste("RegionalOverviews/data_RDBES/002_prepared/", dataprepDate, sep="")

  if (!dir.exists(dir_output_all)){
    dir.create(dir_output_all,recursive=TRUE, showWarnings=FALSE)
  }

  if (!dir.exists(dir_output_rcg)){
    dir.create(dir_output_rcg,recursive=TRUE, showWarnings=FALSE)
  }

  # Secondly, download the data. 

  download_data_from_sharepoint(
    sharepoint_address = "Path to SharePoint",
    filename_vector = paste0('RCG_',regionSelected, ".zip"), 
    dir_download_browser = "Path to directory where browser downloads",
    dir_download_target = "Path to directory where data should be stored",  
    unzip=TRUE
  )

  # e.g. 
  #download_data_from_sharepoint(
  #  sharepoint_address = "https://community.ices.dk/ExternalSites/datacollection/Regional%20coordination%20meetings%202017/RCGIntersessionalWork/2022%20Meeting%20Documents/06.%20Data/Prepared_Data/RDBES_data/20240129",
  #  filename_vector = paste0(target_region, ".zip"), 
  #  dir_download_browser = "//storage-lk.slu.se/home$/erqu0001/Downloads", # Directory where browser downloads e.g. eros machine
  #  dir_download_target = dir_output_rcg,  
  #  unzip=TRUE
  #)

  # Finally we can delete the folder used for downloading the zipped file. 
  unlink(paste(dir_output_rcg, paste0(regionSelected, ".zip"), sep = "/"))

} else {

  cat("The option downloadDataFromSP was not selected, thus locally prepared data will be used.")
  cat("/n")

}

# Print end message
if(is_empty(warnings())){
  cat("\n")
  cat(green('       \u2713'), paste0(" - Completed")) 
  cat("\n")
  cat("\n")
} 
