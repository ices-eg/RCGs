# annual_overview_RDBES_template.Rmd - contains main structure of the overview, but to create a report, please use annual_overview_RDBES.R - 
#   - it sources the .rmd file and additionally in annual_overview_RDBES.R you can define parameters to be used in the report

params <- list(
  year = 2023,
  region = 'Baltic',
  logo_path = file.path("../../overviews_shiny/www/logo RCG BALTIC.PNG"),
  data_dir = 'your path here', 
  RDBES_download_date = '01/01/2000'
  
)

rmdReport <- file.path("RegionalOverviews/overviews_reports/fisheries_overviews_RDBES/annual_overview_RDBES_template.Rmd")

rmarkdown::render(
  rmdReport,
  params = params,
  envir = new.env(parent = globalenv()),
  encoding = 'UTF-8'
)
