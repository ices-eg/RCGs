# library(mapdata)
# 
# cl_rcg %>% 
#   filter(Year==2019) %>% 
#   mutate(Gear = str_sub(FishingActivityLvl6, end = 3)) %>% 
#   group_by(StatisticalRectangle, Year, Quarter) %>% 
#   summarise(LandingsT=sum(LandingWeight_ton, na.rm=TRUE)) %>% 
#   ungroup()->mapcl
# 
# 
# left_join(sl, hh) %>%
#   filter(Region=='BS') %>% 
#   mutate(Gear = str_sub(FishingActivityCategoryEuropeanLvl6 , end = 3),
#           idsamp=paste(CS_TripId, StationNo),
#           Quarter=ceiling(as.numeric(substr(StartDate,6,7))/3)) %>%
#     group_by(x=PosStartLonDec ,y=PosStartLatDec, CatchCategory, Quarter )%>%
#     summarise(wsamp=sum(Weight ,na.rm=T),
#               nbsamp=n_distinct(idsamp))-> mapcs
# 
# 
#   #draw a map
#   mapcl$lon<-mapcl$lat<-mapcl$lonc<-mapcl$latc<-NA
#   for(i in 1:nrow(mapcl)){
#     if(!is.na(mapcl$StatisticalRectangle[i]) & nchar(mapcl$StatisticalRectangle[i])==4){
#       mapcl$lon[i]<-ices.rect(mapcl$StatisticalRectangle[i])$lon
#       mapcl$lat[i]<-ices.rect(mapcl$StatisticalRectangle[i])$lat
# 
#     }
#   }
#   rangex<-c(min(mapcl$lon,na.rm=T)-.1,max(mapcl$lon,na.rm=T)+.1)
#   rangey<-c(min(mapcl$lat,na.rm=T)-.1,max(mapcl$lat,na.rm=T)+.1)
#   #poly map
#   map0<-ggplot(mapcl)+theme_bw()+
#     geom_raster(data=mapcl,aes(x=lon,y=lat,fill=LandingsT ),stat="identity",alpha=.75)+ 
#     geom_point(data=mapcs,aes(x=x,y=y,size=wsamp),alpha=.8,shape=1)+ #CatchCategory
#     borders("worldHires",xlim=rangex,ylim=rangey,fill="light grey",colour="light grey")+
#     coord_quickmap(xlim=range(mapcl$lon,na.rm=T),ylim=range(mapcl$lat,na.rm=T))+
#     scale_fill_distiller(palette='Spectral',name="Landings\n(kg)")+
#     facet_wrap(~Quarter,ncol=2)+xlab("")+ylab("")
#   
#   map0
#   
#   output$SvL_map <-renderPlot({
#     map0
#   })
#     
# # selct Area,  Year
# # Total landings and sampling position and landing sampled weights by quarter and statistical rectangle.
