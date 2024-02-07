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