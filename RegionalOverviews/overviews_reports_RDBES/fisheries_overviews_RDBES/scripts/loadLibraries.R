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
# - Kasia Krakówka
# 
# Dev. notes: 
# 
# - 20240207: Created based on annual_overview_RDBES.R
# - 20240222: Added flextable
#
###################################################################

# Print start message
cat("[1]    Loading libraries")

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
suppressPackageStartupMessages(library(flextable))

# Print end message
if(is_empty(warnings())){
  cat("\n")
  cat(green('       \u2713'), paste0(" - Completed")) 
  cat("\n")
  cat("\n")
} 
