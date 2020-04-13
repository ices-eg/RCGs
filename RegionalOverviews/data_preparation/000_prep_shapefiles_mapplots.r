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
	# prepare shapefile ICESrect
	# ========================

	rm(list=ls())	
	
	sp1<-readOGR(dsn="shapefiles\\ICES_spatial_facility\\ICES_StatRec_mapto_ICES_Areas",layer="StatRec_map_Areas_Full_20170124")

	# RCG BA
		# to develop
	
	# RCG NSEA	
		# to develop
	
	# RCG NA
	
	aux<-sp1@data$AreasList
	sp2<-sp1[!is.na(aux) & (grepl(aux, pat="5.") | 
								grepl (aux, pat="6.") | 
									grepl (aux, pat="7.") | 
										grepl (aux, pat="8.") | 
											grepl (aux, pat="9.")	 | 
												grepl (aux, pat="10.") ) & 
															aux!="5.a.1" & aux!="5.a.2" &  aux!="7.d" & !grepl(aux, pat="3.d.2") & !grepl(aux, pat="4.c, 7.d"),]	
		
	unique(aux[!is.na(aux) & (grepl(aux, pat="5.") | 
								grepl (aux, pat="6.") | 
									grepl (aux, pat="7.") | 
										grepl (aux, pat="8.") | 
											grepl (aux, pat="9.")	 | 
												grepl (aux, pat="10.") ) & 
															aux!="5.a.1" & aux!="5.a.2" &  aux!="7.d" & !grepl(aux, pat="3.d.2") & !grepl(aux, pat="4.c, 7.d")])
	
	
	shapefile(sp2, 'shapefiles\\RCG_NA_ICESrect.shp', overwrite=TRUE)

	
	# ========================
	# prepare shapefile FAO Areas
	# ========================

	rm(list=ls())	
	
	sp1<-rgdal::readOGR(dsn="shapefiles/FAO_areas", layer="FAO_AREAS_NOCOASTLINE")

	# NA
	
	aux<-sp1@data$F_SUBAREA	
	sp2<-sp1[!is.na(aux) & (grepl(aux, pat="27.5") | 
								grepl (aux, pat="27.6") | 
									grepl (aux, pat="27.7") | 
										grepl (aux, pat="27.8") | 
											grepl (aux, pat="27.9")	 | 
												grepl (aux, pat="27.10") ),]	
	aux<-sp2@data$F_DIVISION
	sp2<-sp2[!is.na(aux) & !grepl (aux, pat="27.5.a") &  !grepl (aux, pat="27.7.d"),]

	shapefile(sp2, 'shapefiles\\RCG_NA_FAOareas.shp', overwrite=TRUE)
				
	
	# ========================
	# manual validation
	# ========================

	rm(list=ls())
	
	# read data
	
	load("RDB_RCG_NA_CL_2009_2017_prepared_201904020909.Rdata")
	load("RDB_RCG_NA_CE_2009_2017_prepared_201904020909.Rdata")

	# shapefile rectangles
		sp2<-list("shp" = read.shp("shapefiles\\RCG_NA_ICESrect.shp"), "shx" = read.shx("shapefiles\\RCG_NA_ICESrect.shx"), "dbf"= read.dbf("shapefiles\\RCG_NA_ICESrect.dbf"))

		uniq_rect<-unique(unique(cl_rcg$StatisticalRectangle), unique(ce_rcg$StatisticalRectangle))
		uniq_rect[!uniq_rect %in% sp2$dbf$dbf$ICESNAME]	
		
		# check if it makes sense
		sp1@data[sp1@data$ICESNAME %in% uniq_rect[!uniq_rect %in% sp2$dbf$dbf$ICESNAME],]

	# shapefile fao areas
		sp2<-list("shp" = read.shp("shapefiles\\RCG_NA_FAOareas.shp"), "shx" = read.shx("shapefiles\\RCG_NA_FAOareas.shx"), "dbf"= read.dbf("shapefiles\\RCG_NA_FAOareas.dbf"))

		uniq_areas<-unique(unique(cl_rcg$Area), unique(ce_rcg$Area))
		uniq_areas[!uniq_areas %in% unique(c(as.character(sp2$dbf$dbf$F_DIVISION),as.character(sp2$dbf$dbf$F_SUBAREA),as.character(sp2$dbf$dbf$F_SUBDIVIS)))]	
		
		# check if it makes sense
		sp1@data[sp1@data$ICESNAME %in% uniq_rect[!uniq_rect %in% sp2$dbf$dbf$ICESNAME],]
		
	
	# ========================
	# map
	# ========================
	
	library(mapplots)
	
	map_fao_areas<-list("shp" = read.shp("shapefiles\\RCG_NA_FAOareas.shp"), "shx" = read.shx("shapefiles\\RCG_NA_FAOareas.shx"), "dbf"= read.dbf("shapefiles\\RCG_NA_FAOareas.dbf"))
	map_ices_rect<-list("shp" = read.shp("shapefiles\\RCG_NA_ICESrect.shp"), "shx" = read.shx("shapefiles\\RCG_NA_ICESrect.shx"), "dbf"= read.dbf("shapefiles\\RCG_NA_ICESrect.dbf"))
	data_dir<-"shapefiles\\GSHHG\\gshhg-shp-2.3.7\\GSHHS_shp\\i"
	map_coast<-list("shp" = read.shp(paste(data_dir,"GSHHS_i_L1.shp", sep="/")), "shx" = read.shx(paste(data_dir,"GSHHS_i_L1.shx", sep="/")), "dbf"= read.dbf(paste(data_dir,"GSHHS_i_L1.dbf", sep="/")))
	data_dir<-"shapefiles\\GSHHG\\gshhg-shp-2.3.7\\WDBII_shp\\i"
	map_borders<-list("shp" = read.shp(paste(data_dir,"WDBII_border_i_L1.shp", sep="/")), "shx" = read.shx(paste(data_dir,"WDBII_border_i_L1.shx", sep="/")), "dbf"= read.dbf(paste(data_dir,"WDBII_border_i_L1.dbf", sep="/")))
	
	xlimite=c(-45.5,27.5)
	ylimite=c(32.5,65.5)
	basemap(xlim=xlimite, ylim=ylimite, main = "", bg="white", xaxs="i", yaxs="i")
	draw.shape(map_ices_rect, border="gray", col="transparent")
	draw.shape(map_fao_areas, border="black", col="transparent")
	draw.shape(map_coast, col="brown")
	draw.shape(map_borders, type="l",col="black")
	
	text(map_ices_rect$dbf$dbf[c("stat_x","stat_y")], labels= map_ices_rect$dbf$dbf$ICESNAME, cex=0.3, col="blue")
	
	
	