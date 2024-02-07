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

# Print start message
cat("[3]    Parameters definition")

# Empty warnings from previous code
assign("last.warning", NULL, envir = baseenv()) # Credits: https://stackoverflow.com/questions/5725106/r-how-to-clear-all-warnings

## Download data 
params <- list(
  year = yearSelected,
  region = regionSelected, 
  logo_path = file.path("../../overviews_shiny/www/logo RCG BALTIC.PNG"), # move it to the rmd part <----------------- to do 
  data_dir = paste0(getwd(), '/RegionalOverviews/data_RDBES/002_prepared/', dataprepDate, "/", regionSelected, '/RCG_', regionSelected),
  CLfileName = paste0('RDBES_RCG_', regionSelected, '_CL_2021_2021_prepared_', dataprepDate),
  CEfileName = paste0('RDBES_RCG_', regionSelected, '_CE_2021_2021_prepared_', dataprepDate),
  RDBES_download_date = '01/01/2000'
)

# Print parameters. 
cat("\n")
cat("       The overview will be generated using the following parameters:")
cat("\n")
cat("\n")
writeLines(
    paste0(
      "             ", 
      capture.output(print(data.frame(
        do.call(rbind, params) %>% 
          data.frame() %>% 
          tibble::rownames_to_column(var = "Parameter") %>% 
          dplyr::rename("Value" = 2)
        )
      )
    )
  )
)
cat("\n")

# Print end message
if(is_empty(warnings())){
  cat("\n")
  cat(green('       \u2713'), paste0(" - Completed")) 
  cat("\n")
  cat("\n")
} 