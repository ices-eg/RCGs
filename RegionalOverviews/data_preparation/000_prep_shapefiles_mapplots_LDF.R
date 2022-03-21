# ==========================
# Prepare and validate RCG shapefiles (to be used with mapplots::draw.shape and func_heatmap_ices_rect_one_var)
# ==========================

# RCG subgroup work on Regional Fisheries and Sampling Overview
# Nuno, Lucia, Sven, Marta, Gwladys, Hans, Henrik, Kirsten, Perttu, Alastair, Liz, Emilie, Joël
# 2019

# RCM NA: the North Atlantic (ICES areas V-X, excluding Va and VIId)
# RCM NS&EA: the  North  Sea  (ICES  areas  IIIa,  IV  and  VIId),  the  Eastern  Arctic  (ICES  areas  I  and  II),  the  ICES  divisions Va, XII & XIV and the NAFO areas.	
# RCM Baltic: Baltic Sea (ICES areas III b-d)



library(raster)
library(rgdal)

# ========================
# prepare shapefile FAO Areas
# ========================

rm(list=ls())	

sp1<-rgdal::readOGR(dsn="shapefiles/FAO_areas", layer="FAO_AREAS_NOCOASTLINE")


# LDF

#aux<-sp1@data$F_SUBAREA	
# sp2<-sp1[!is.na(aux) & (grepl(aux, pat="34") | 
#                           grepl (aux, pat="87") | 
#                           grepl (aux, pat="41") | 
#                           grepl (aux, pat="47") ) ,]	
#aux<-sp2@data$F_SUBAREA	
#aux2<-sp2@data$F_DIVISION
#aux3<-sp2@data$F_SUBDIVIS # because the resolution according to the datacall should be division. So subdivision is not needed unless we want to present subdivisisons on maps
#aux4 <- sp2@data$F_AREA
#sp2<-sp2[(!is.na(aux2) | (is.na(aux2) & aux == '34.2')) & is.na(aux3) ,] #   | (is.na(aux2) & is.na(aux))
#shapefile(sp2, 'shapefiles\\RCG_LDF_FAOareas.shp', overwrite=TRUE)

# correct in area 34
aux<-sp1@data$F_SUBAREA	
aux1 <-sp1@data$F_AREA	
sp2<-sp1[(!is.na(aux)|(is.na(aux) & aux1=='34')) & (grepl(aux1, pat="34") | 
                                         grepl (aux, pat="87") | 
                                         grepl (aux, pat="41") | 
                                         grepl (aux, pat="47") ) ,]	
aux<-sp2@data$F_SUBAREA	
aux2<-sp2@data$F_DIVISION
aux3<-sp2@data$F_SUBDIVIS # because the resolution according to the datacall should be division. So subdivision is not needed unless we want to present subdivisisons on maps
aux4 <- sp2@data$F_AREA
sp2<-sp2[((!is.na(aux2) & aux4 !='34')| (is.na(aux) & aux4 == '34')) & is.na(aux3) ,] #   | (is.na(aux2) & is.na(aux))
shapefile(sp2, 'shapefiles\\RCG_LDF_FAOareas34.shp', overwrite=TRUE)

# ========================
# manual validation
# ========================
# 
# rm(list=ls())
# 
# # shapefile fao areas
# sp2<-list("shp" = shapefiles::read.shp("shapefiles\\RCG_LDF_FAOareas.shp"),
#           "shx" = shapefiles::read.shx("shapefiles\\RCG_LDF_FAOareas.shx"), 
#           "dbf"= shapefiles::read.dbf("shapefiles\\RCG_LDF_FAOareas.dbf"))
# load("D:/WG/RCG/IntersessionalWork/Github/RCGs/RegionalOverviews/data/002_prepared/RCG_LDF/RDB_LDF_CL_2009_2018_prepared_201906261127.Rdata")
# load("D:/WG/RCG/IntersessionalWork/Github/RCGs/RegionalOverviews/data/002_prepared/RCG_LDF/RDB_LDF_CE_2009_2018_prepared_201906261127.Rdata")
# 
# uniq_areas<-unique(unique(cl$AreaMap), unique(ce$AreaMap))
# uniq_areas[!uniq_areas %in% unique(c(as.character(sp2$dbf$dbf$F_DIVISION),as.character(sp2$dbf$dbf$F_SUBAREA),as.character(sp2$dbf$dbf$F_SUBDIVIS)))]	

rm(list=ls())

# shapefile fao areas
sp2<-list("shp" = shapefiles::read.shp("shapefiles\\RCG_LDF_FAOareas34.shp"),
          "shx" = shapefiles::read.shx("shapefiles\\RCG_LDF_FAOareas34.shx"), 
          "dbf"= shapefiles::read.dbf("shapefiles\\RCG_LDF_FAOareas34.dbf"))
load("D:/WG/RCG/IntersessionalWork/Github/RCGs/RegionalOverviews/data/002_prepared/RCG_LDF/RDB_LDF_CL_2009_2018_prepared_201906262109.Rdata")
load("D:/WG/RCG/IntersessionalWork/Github/RCGs/RegionalOverviews/data/002_prepared/RCG_LDF/RDB_LDF_CE_2009_2018_prepared_201906262109.Rdata")

uniq_areas<-unique(unique(cl$AreaMap), unique(ce$AreaMap))
uniq_areas[!uniq_areas %in% unique(c(as.character(sp2$dbf$dbf$F_DIVISION),as.character(sp2$dbf$dbf$F_AREA),as.character(sp2$dbf$dbf$F_SUBAREA),as.character(sp2$dbf$dbf$F_SUBDIVIS)))]	











# # 34.1.2 + other
# # for now it was only here a change
# # make a column fg in the data, which is the same as area with the exception for area 34, as here it will be 34.1 and 34.2
# 
# # REMARK there is no 34.1.2 in the data
# 
# # We have to divide into 34.2 and 34.1 (including 34)
# library(raster)
# library(rgdal)
# 
# # ========================
# # prepare shapefile FAO Areas
# # ========================
# 
# rm(list=ls())	
# 
# sp1  = sf::st_read(
#   "shapefiles/FAO_areas/FAO_AREAS_NOCOASTLINE.shp"
# ) %>% filter(F_LEVEL=='DIVISION')
# 
# sp1 %>% 
#   filter(
#     grepl(F_AREA, pat = '34') | 
#       grepl (F_AREA, pat="87") | 
#       grepl (F_AREA, pat="41") | 
#       grepl (F_AREA, pat="47") 
#   )%>% 
#   mutate(ID = ifelse(grepl(F_CODE, pat = '34.1.2'),1, 
#                      ifelse(!grepl(F_CODE, pat = '34'),ID, 2 )),
#                     FG = ifelse(grepl(F_CODE, pat = '34.1.2'),'34.1.2',
#                                 ifelse(!grepl(F_CODE, pat = '34'),as.character(F_CODE), '34.xxx' ))) %>%
#   group_by(FG) %>% summarise(ID = mean(ID))-> sp1




############################################################# ONLY MAJOR AREAS



library(raster)
library(rgdal)

# ========================
# prepare shapefile FAO Areas
# ========================

rm(list=ls())	

sp1<-rgdal::readOGR(dsn="shapefiles/FAO_areas", layer="FAO_AREAS_NOCOASTLINE")


# LDF


# correct in area 34
aux<-sp1@data$F_AREA	
aux1 <-sp1@data$F_SUBAREA	
sp2<-sp1[is.na(aux1) & (grepl(aux, pat="34") | 
                                                      grepl (aux, pat="87") | 
                                                      grepl (aux, pat="41") | 
                                                      grepl (aux, pat="47") ) ,]	

shapefile(sp2, 'shapefiles\\RCG_LDF_FAOareasMajor.shp', overwrite=TRUE)



