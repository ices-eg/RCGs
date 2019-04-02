# Marta Suska
# NMFRI
# msuska@mir.gdynia.pl

# Use the dataset prepared in '001_read_and_prepare_data_rdb_2009_2018.R'
load("D:/WG/RCG/IntersessionalWork/Subgroup on Regional Overviews/TestData/RDB_RCG_NA_CL_2009_2017_prepared_201904020909.Rdata")
cl_2017 = cl_rcg %>% filter(Year == 2017)

# Prepare the dataset with coordinates
Harbours_Codes = read_csv('C:/Users/msuska/Desktop/RCG/2018/Data/Harbours_Codes.csv') # file from -> RCG sharepoint->Data _> Data group scripts and data -> data files

Harbours_Codes %>% 
  mutate(Harbour = Hcode) %>% 
  select(Harbour, lat, lon)-> Harbours

##################################################################################################################################################
##################################################################################################################################################

# group_func(cl_2017, var = OfficialLandingCatchWeight,  groupBy=quos(Harbour, HarbourDesc), func = sum, type_of_threshold = 'percent',value_of_threshold = 100)

# pointsMap_func(cl_2017, var = OfficialLandingCatchWeight,  groupBy=quos(Harbour, Year), func = sum, type_of_threshold = 'percent',value_of_threshold = 90,
#                points_coord = Harbours, plot_labels = TRUE, time = Year,saveResults = FALSE, outputPath = 'D:/WG/RCG/IntersessionalWork/Github/RCGs/RegionalOverviews')
# # 
# pointsMap_func(cl_2017, var = OfficialLandingValue,  groupBy=quos(Harbour, HarbourDesc, Year), func = sum, type_of_threshold = 'top_n',value_of_threshold = 10,
#                points_coord = Harbours, plot_labels = FALSE, time = Year, saveResults = FALSE, outputPath = 'D:/WG/RCG/IntersessionalWork/Github/RCGs/RegionalOverviews')

