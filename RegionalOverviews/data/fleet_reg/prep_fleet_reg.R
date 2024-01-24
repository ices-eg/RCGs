#The script prepare files to be used in the overviews.

#change the list of MS if needed
MS_country<-c("BEL","DEU" ,"DNK","ESP","EST","FIN","FRA","IRL","LTU","LVA",
              "NLD","POL","PRT","SWE")
#change year
year<-2023

#check the separator used in input files (',' or ';')
separator<-";"

#check the date format in Event Start and Event End (e.g. "%Y-%m-%d", "%d/%m/%Y", other)
format_start_date<-"%d/%m/%Y"
format_end_date<-"%d/%m/%Y"

#run the function which change the date format 
source("D:/RCGs/RegionalOverviews/funs_RDBES/func_prep_fleet_register.R")
prep_fleet_register(MS_country,
                    year,
                    separator,
                    format_start_date,
                    format_end_date)
