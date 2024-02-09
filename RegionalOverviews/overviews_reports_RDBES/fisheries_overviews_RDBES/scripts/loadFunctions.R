###################################################################
# loadFunctions
###################################################################
#
# This script loads the functions needed for the generation of the 
# annual RDBES overview. 
# 
###################################################################
# Authors: 
# - Eros Quesada [first draft] 
# 
# Dev. notes: 
# 
# - 20240207: Created based on annual_overview_RDBES.R
#
###################################################################

# Print start message
cat("[2]    Loading functions")

# Empty warnings from previous code
assign("last.warning", NULL, envir = baseenv()) # Credits: https://stackoverflow.com/questions/5725106/r-how-to-clear-all-warnings

## Specify the folder hosting the functions used needed for the generation of the annual RDBES overview. 
fun_RDBES_dir <- "RegionalOverviews/funs_RDBES"

## Load functions
for(fun in list.files(fun_RDBES_dir)){
    source(paste(fun_RDBES_dir, fun, sep = "/"))
}

# Print end message
if(is_empty(warnings())){
  cat("\n")
  cat(green('       \u2713'), paste0(" - Completed")) 
  cat("\n")
  cat("\n")
} 