# Working version

# To do before running the func:
# prepare the dataset
# filter out appropriate Region - watch out, RCG NA written as NA in the dataset (how to change that into a string?)
# filter out appropriate year

# eg
# data loading
CL = read_csv('D:/WG/RCG/IntersessionalWork/Subgroup on Regional Overviews/TestData/CL Landing 2009-2017.csv')
CL_2014_NA = CL %>% filter(is.na(Region), Year==2014)

# where to get the harbours with coordinates from?
# RCG datacall attachement  - no coordinates, encoding errors
# UNLOCODE from RCMfunctions - encoding errors, less rows
# File prepared by HKN during RCG 2018 - less rows
Harbours_Codes = read_csv('C:/Users/msuska/Desktop/RCG/2018/Data/Harbours_Codes.csv') # file from -> RCG sharepoint->Data _> Data group scripts and data -> data files

Harbours_Codes %>% 
  mutate(Harbour = Hcode) %>% 
  select(Harbour, lat, lon)-> Harbours

##################################################################################################################################################
##################################################################################################################################################

#group_func(CL_2014_NA, var = OfficialLandingCatchWeight,  groupBy=quos(Harbour, HarbourDesc), func = sum, type_of_threshold = 'percent',value_of_threshold = 100)

# pointsMap_func(CL_2014_NA, var = OfficialLandingCatchWeight,  groupBy=quos(Harbour, Year), func = sum, type_of_threshold = 'percent',value_of_threshold = 90,
#                points_coord = Harbours, plot_labels = TRUE, time = Year,saveResults = FALSE, outputPath = 'D:/WG/RCG/IntersessionalWork/Github/RCGs/RegionalOverviews')
# # 
# pointsMap_func(CL_2014_NA, var = OfficialLandingValue,  groupBy=quos(Harbour, HarbourDesc, Year), func = sum, type_of_threshold = 'top_n',value_of_threshold = 10,
#                points_coord = Harbours, plot_labels = FALSE, time = Year, saveResults = FALSE, outputPath = 'D:/WG/RCG/IntersessionalWork/Github/RCGs/RegionalOverviews')

##################################################################################################################################################
##################################################################################################################################################

# on sharepoint
pointsMap_func(CL_2014_NA, var = OfficialLandingCatchWeight,  groupBy=quos(Harbour, Year), func = sum, type_of_threshold = 'percent',value_of_threshold = 90,
               points_coord = Harbours, plot_labels = FALSE, time = Year, saveResults = FALSE, outputPath = 'D:/WG/RCG/IntersessionalWork/Github/RCGs/RegionalOverviews')

