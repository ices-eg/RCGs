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
cat("[1]    Loading functions")

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