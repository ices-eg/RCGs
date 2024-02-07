###################################################################
# parametersDefinition 
###################################################################
#
# This script simply gather user selections (i.e. values 
# specified by the user in annual_overview_RDBES script) and uses 
# them to generate the list of parameters used ultimately to 
# generate the overviews. 
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

## Download data 
params <- list(
  year = yearSelected,
  region = regionSelected, 
  logo_path = file.path("../../overviews_shiny/www/logo RCG BALTIC.PNG"), # move it to the rmd part <----------------- to do 
  data_dir = paste0(getwd(), 'RegionalOverviews/data_RDBES/002_prepared/', downloadDataFromSPDate, '/RCG_', regionSelected),
  CLfileName = paste0('RDBES_RCG_', regionSelected, '_CL_2021_2021_prepared_', downloadDataFromSPDate),
  CEfileName = paste0('RDBES_RCG_', regionSelected, '_CE_2021_2021_prepared_', downloadDataFromSPDate),
  RDBES_download_date = '01/01/2000'
)

# Print parameters. 
cat("The overview will be generated using the following parameters:")
cat("\n")
cat("\n")
do.call(rbind, params) %>% 
  data.frame() %>% 
  tibble::rownames_to_column(var = "Parameter") %>% 
  dplyr::rename("Value" = 2)
