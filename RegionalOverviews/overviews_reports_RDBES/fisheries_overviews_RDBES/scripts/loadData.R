###################################################################
# Load data 
###################################################################
#
# This script is used to load CL and CE data in the prepared form.
#  
# These data originate from raw RDBES data treated with the script 
# "001_read_and_prepare_data_RDBES_CL_CE". The preparation is either
# performed locally or data already prepared are downloaded prior 
# to the overview generation. 
#
###################################################################
# Authors: 
# - Marta Suska [first draft]
# - Eros Quesada
# 
# Dev. notes: 
#
# - 20240130: Created  
# - 20240207: Formatted 
#
###################################################################

if(downloadDataFromSP == 1){
  cat("[5]    Load data")
  cat("\n")
} else{
  cat("[4]    Load data")
  cat("\n")
}

# Empty warnings from previous code
assign("last.warning", NULL, envir = baseenv()) # Credits: https://stackoverflow.com/questions/5725106/r-how-to-clear-all-warnings

## Load data 
# Load CL data
load(
  paste(params$data_dir, '/', params$CLfileName,'.Rdata', sep = "")
); cl = cl_rcg # shorter name 

# Load CE data
load(
  paste(params$data_dir, '/' ,params$CEfileName,'.Rdata', sep = "")
); ce = ce_rcg # shorter name 

# put some necessary data prep part below
######################
# FILTER the data out
######################
cl <- cl[CLyear %in% params$year]
ce <- ce[CEyear %in% params$year]

# Print end message
if(is_empty(warnings())){
  cat("\n")
  cat(green('       \u2713'), paste0(" - Completed")) 
  cat("\n")
  cat("\n")
} 