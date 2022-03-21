library(data.table)
library(tidyverse)

file_cl <- "..\\RCG LDF Data\\RDB_LDF_CL_2014_2019_prepared_202007141648.Rdata"
file_ce <- "..\\RCG LDF Data\\RDB_LDF_CE_2014_2019_prepared_202007141648.Rdata" 

load(file_cl)
load(file_ce)


cl %>% 
  filter(Year == 2019) %>% 
  group_by(SubPolygon, Catch_group, FlagCountry) %>% 
  summarise(sum(LandingWeight_ton))->table31

table31 %>% 
  group_by(SubPolygon, Catch_group) %>% 
  mutate(paste(FlagCountry))

ce %>% 
  filter(Year %in% c(2017, 2018, 2019)) %>% 
  filter(Area == '34'& SubPolygon!='Canaries'  & SubPolygon!='Madeira' | (Area == '34' & is.na(SubPolygon))) %>% #
  group_by(Year, FishingActivityCategoryEuropeanLvl6, FlagCountry) %>% 
  summarise(DaysAtSea = sum(DaysAtSea)) %>% 
  pivot_wider(names_from = Year, values_from = DaysAtSea)

cl %>% 
  filter(Year %in% c(2014:2019)) %>% 
  filter((Area == '34'& SubPolygon!='Canaries'  & SubPolygon!='Madeira' | (Area == '34' & is.na(SubPolygon)))) %>% 
  group_by(Year, FishingActivityCategoryEuropeanLvl6, FlagCountry) %>% 
  summarise(LandingWeight_ton = sum(LandingWeight_ton)) %>% 
  pivot_wider(names_from = FlagCountry, values_from = LandingWeight_ton)
  

# 3.4
cl %>% 
  filter(Year %in% c(2014:2019)) %>% 
  filter(Area == '34'& SubPolygon!='Canaries'  & SubPolygon!='Madeira'  | (Area == '34' & is.na(SubPolygon)) ) %>% 
  group_by(Year) %>% 
  summarise(LandingWeight_ton = sum(OfficialLandingCatchWeight, na.rm = TRUE)) 

cl %>% 
  filter(Year %in% c(2017:2019)) %>% 
  filter(Area == '34'& SubPolygon!='Canaries'  & SubPolygon!='Madeira'  | (Area == '34' & is.na(SubPolygon)) ) %>% 
  group_by(FishingActivityCategoryEuropeanLvl6) %>% 
  summarise(suma = sum(LandingWeight_ton)) %>% 
  ungroup() %>% 
  mutate(suma/sum(suma)*100)
  

