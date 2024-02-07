###################################################################
# loadLibraries
###################################################################
#
# This script loads the libraries needed for the generation of the 
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
cat("[2]    Loading libraries")

# Empty warnings from previous code
assign("last.warning", NULL, envir = baseenv()) # Credits: https://stackoverflow.com/questions/5725106/r-how-to-clear-all-warnings

## Load libraries
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(car))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(kableExtra))
suppressPackageStartupMessages(library(rnaturalearth))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(crayon))

# Print end message
if(is_empty(warnings())){
  cat("\n")
  cat(green('       \u2713'), paste0(" - Completed")) 
  cat("\n")
  cat("\n")
} 
