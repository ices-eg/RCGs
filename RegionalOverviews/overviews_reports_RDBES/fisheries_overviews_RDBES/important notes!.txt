divide code into smaller parts and then source it in the main .rmd - easier to debug
smaller parts of the code can be put inside scripts folder 

annual_overview_RDBES_template.Rmd - contains main structure of the overview, but to create a report, please use annual_overview_RDBES.R - 
- it sources the .rmd file and additionally in annual_overview_RDBES.R you can define parameters to be used in the report

params:
in the annual_overview_RDBES_template.Rmd there are some default params
in annual_overview_RDBES.R there are the actual parameters which will be used in the report. They will overwrite parameters defined in the rmd.

If for any reason, for example to test your code, debug it etc you need to run the rmd file line by line, chunk by chunk, then you need to specify the parameters defined at the beginning of the rmd file. 

when some part of the code is valid only for a specific region, e.g. shapefile with rectangles for BA, you don't have to use if()... . Instead you can add eval=eval_region_BA to the chunk, ane this chunk will be used only for Baltic overview