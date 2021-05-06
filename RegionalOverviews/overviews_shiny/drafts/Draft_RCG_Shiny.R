# 
# # ICES rectagles
# shp.data <- readOGR("shp/RCG_NA_ICESrect.shp")
# #shp.data <- readOGR("shp/ICES_spatial_facility/ICES_rectangles/ICES_Statistical_Rectangles_Eco.shp")
# 
# shp_NA<- spTransform(shp.dataa, CRS("+proj=longlat +datum=WGS84 +no_defs"))
# 
# # map
# leaflet ()%>%addPolygons(data = shp.NA)%>%addTiles()
# 
# # Mask 
# library(raster)
# spain<-readRDS("C:/use/Maria/2019/MPDH 2019/draft/gadm36_ESP_0_sp.rds")
# 
# plot(shp_NA)
# masked <- mask(x = spain, mask = shp.NA)
