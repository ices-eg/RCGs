# annual_overview_RDBES_template.Rmd - contains main structure of the overview, but to create a report, please use annual_overview_RDBES.R - 
#   - it sources the .rmd file and additionally in annual_overview_RDBES.R you can define parameters to be used in the report

params <- list(
  year = 2021,
  region = 'Baltic',
  logo_path = file.path("../../overviews_shiny/www/logo RCG BALTIC.PNG"),
  data_dir = '../../data_RDBES/002_prepared/20240129/RCG_BA', 
  CLfileName = 'RDBES_RCG_BA_CL_2021_2021_prepared_20240129',
  CEfileName = 'RDBES_RCG_BA_CE_2021_2021_prepared_20240129',
  RDBES_download_date = '01/01/2000'
  
)
rmdReport <- file.path("RegionalOverviews/overviews_reports_RDBES/fisheries_overviews_RDBES/annual_overview_RDBES_template.Rmd")

rmarkdown::render(
  rmdReport,
  params = params,
  output_file = paste0('AnnualOverview_', params$year ,'_', params$region, '.html'),
  envir = new.env(parent = globalenv()),
  encoding = 'UTF-8'
)
