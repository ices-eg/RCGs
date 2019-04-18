# RCG subgroup work on Regional Fisheries and Sampling Overview
	# Nuno, Lucia, Sven, Marta, Gwladys, Hans, Henrik, Kirsten, Perttu, Alastair, Liz, Emilie, Joël
	# 2019
	
	
# script to produce graphs of Annual Overview



	# =======================	
	# section General
	# =======================
			
		# read data
		rm(list=ls())
		
		load("data\\002_prepared\\RCG_NA\\RDB_RCG_NA_CL_2009_2018_prepared_201904131853.Rdata")
		load("data\\002_prepared\\RCG_NA\\RDB_RCG_NA_CE_2009_2018_prepared_201904131853.Rdata")
		
		# read functions
		source("funs/func_barplot_var_by_one_var.r")	
		source("funs/func_barplot_var_by_two_var_stacked.r")
		
		# Subset Year
		
		head(cl_rcg)
		
		cl_rcg<-droplevels(cl_rcg[Year==2017,])
		ce_rcg<-droplevels(ce_rcg[Year==2017,])
				
		#Adds IDs [move to preparation]
		cl_rcg[,FlagCountry_Loa:=paste(FlagCountry, VesselLengthCategory, sep="_")]
		ce_rcg[,FlagCountry_Loa:=paste(FlagCountry, VesselLengthCategory, sep="_")]
		
		# Add to preparation [and convert 2-letter code to 3-letter code]
		colnames(ce_rcg)[colnames(ce_rcg)=="LandingCountry"]<-"LandingCountry2"
	
	#CL
		
		
		# read_graph_details
		graph_det_all <- read.table("graphical_parameters/RCG_NA/Annual_Overview/AnnualOverview_RCG_NA_CL_Graphical_details.txt", sep="\t", stringsAsFactors=FALSE, header=T)
		colour_table<-read.table("aux_colours.txt", header=T, sep="\t", colClasses="character", na.strings="", comment.char="")
				
		#graph_det_all<-graph_det_all[1:20,]
		
		for(group in unique(graph_det_all$Catch_group))
		{
		
		print(group)	
		
		# subsets group
			graph_det1<-graph_det_all[graph_det_all$Catch_group==group,]
			if(group!="NULL") cl_rcg_group<-cl_rcg[Catch_group==group] else cl_rcg_group<-cl_rcg
		
			for (Graph_group in unique(graph_det1$Graph_group))
			{
			print(paste("Graph group:",Graph_group))		
			graph_det<-graph_det1[graph_det1$Graph_group==Graph_group,]
			
			# detects group

			if(nrow(graph_det)>1) {windows(10,10); par(oma = eval(parse(text=graph_det$graph_par))$oma, mfrow=eval(parse(text=graph_det$graph_par))$mfrow)}
			if(nrow(graph_det)==1) {windows(10,5)} # oma parameter is set inside the barplot function
			
		# runs graphs
		for (i in 1:nrow(graph_det))
		{
		
		print(i)
		if(graph_det$Graph_type[i]==1)
			{
			res<-barplot_var_by_one_var(x = as.data.frame(cl_rcg_group), Var = graph_det$Var[i] , var1 = graph_det$var1[i], tapply_type = graph_det$tapply_type[i], type_of_threshold = graph_det$type_of_threshold[i], value_of_threshold = graph_det$value_of_threshold[i], sorted=graph_det$sorted[i], graph_par = eval(parse(text=graph_det$graph_par[i])))
			savePlot(filename = paste(graph_det$png_dir[i], paste(graph_det$png_name[i],".png", sep=""), sep="/"), type="png")
			write.table(res, file = paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".txt", sep=""), sep="\t", dec=".", row.names = TRUE)
			write.xlsx(res, file = paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".xlsx", sep=""), sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE)
			dev.off()
			}
		if(graph_det$Graph_type[i]==2)
			{
			res<-barplot_var_by_two_var_stacked(x = as.data.frame(cl_rcg_group), Var = graph_det$Var[i] , var1 = graph_det$var1[i], var2 = graph_det$var2[i], tapply_type = graph_det$tapply_type[i], proportion = graph_det$proportion[i], type_of_threshold = graph_det$type_of_threshold[i], value_of_threshold = graph_det$value_of_threshold[i], sorted=graph_det$sorted[i], graph_par = eval(parse(text=graph_det$graph_par[i])), legend_par = graph_det$legend_par)
			savePlot(filename = paste(graph_det$png_dir[i], paste(graph_det$png_name[i],".png", sep=""), sep="/"), type="png")
			write.table(res, file = paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".txt", sep=""), sep="\t", dec=".", row.names = TRUE)
			write.xlsx(res, file = paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".xlsx", sep=""), sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE)
			dev.off()
			}			
		}
		}
		#graphics.off()
		}
		
		
	# maps
		library(tidyverse)
		
		# Load shapefiles and Harbour Lists
		########################################################################################################################################################################
		# Prepare the dataset with coordinates <-----------------------  WORK on this part
		Harbours_Codes = read_csv('C:/Users/msuska/Desktop/RCG/2018/Data/Harbours_Codes.csv') # file from -> RCG sharepoint->Data _> Data group scripts and data -> data files
		
		Harbours_Codes %>% 
		  mutate(Harbour = Hcode) %>% 
		  select(Harbour, lat, lon)-> Harbours
		
		# load shapefile
		shp  = sf::st_read(
		  "shapefiles/FAO_areas/FAO_AREAS_NOCOASTLINE.shp"
		)
		shp %>%
		  filter((!is.na(F_DIVISION) &
		            is.na(F_SUBDIVIS)) |
		           (F_SUBDIVIS == '27.5.b.1' &
		              is.na(F_SUBUNIT)) |
		           (F_SUBDIVIS == '27.9.b.2' & is.na(F_SUBUNIT)) # to avoid duplicates, another solution?
		  ) %>%
		  mutate(Area = F_CODE) -> shp
		# For plotting FishingGrounds
		cl_rcg %>% group_by(FishingGround) %>% distinct( Area)->FishingGround
		shp %>% left_join(FishingGround) %>% group_by(FishingGround) %>% summarise(ID = mean(ID))-> FAOshpFG
		FAOshpFG = cbind(FAOshpFG,  sf::st_coordinates(sf::st_centroid(FAOshpFG$geometry))) %>% mutate(lon = X, lat = Y)
		
		# For plotting Areas
		# add centroids - to put areas labels there, and to put piecharts there, creates new columns to the dataset named X, Y
		FAOshp = cbind(shp,  sf::st_coordinates(sf::st_centroid(shp$geometry))) %>% mutate(lon = X, lat = Y)
		
		StatRectshp  = sf::st_read(
		  "shapefiles/ICES_spatial_facility/ICES_rectangles/ICES_Statistical_Rectangles_Eco.shp" 
		)
		StatRectshp %>% mutate(StatisticalRectangle = ICESNAME)-> StatRectshp
		StatRectshp = cbind(StatRectshp,  sf::st_coordinates(sf::st_centroid(StatRectshp$geometry))) %>% mutate(lon = X, lat = Y)
		
		
		# adjust color palette to the ggplot maps
		aux_colours_ggplot = c(colour_table$colour4)
		names(aux_colours_ggplot) = c(colour_table$Country)
		
		options(scipen=10000) # to remove scientific notation from the legend
		########################################################################################################################################################################
		source("funs/pointsMap_func.R")
		source("funs/choroplethMap_func.R")
		source("funs/scatterpieMap_func.R")
		
		# read_graph_details
		graph_det_all <- read.table("graphical_parameters/RCG_NA/Annual_Overview/AnnualOverview_RCG_NA_CL_Graphical_details_maps.txt", sep="\t", stringsAsFactors=FALSE, header=T)

		for(group in unique(graph_det_all$Catch_group))
		{
		  
		  print(group)	
		  
		  # subsets group
		  graph_det<-graph_det_all[graph_det_all$Catch_group==group,]
		  if(group!="NULL") cl_rcg_group<-cl_rcg[Catch_group==group] else cl_rcg_group<-cl_rcg

		    # runs graphs
		    for (i in 1:nrow(graph_det))
		    {
		      
		      print(i)
		      if(graph_det$Graph_type[i]==3)
		      {
		        res = pointsMap_func(cl_rcg_group, var = graph_det$var[i],  groupBy=graph_det$groupBy[i], facet = graph_det$facet[i],
		                             func = graph_det$func[i], type_of_threshold = graph_det$type_of_threshold[i], value_of_threshold =  graph_det$value_of_threshold[i],
		                             points_coord =  eval(parse(text = graph_det$points_coord[i])), plot_labels = graph_det$plot_labels[i], saveResults = FALSE,
		                             Catch_group = graph_det$Catch_group[i], 
		                             newVarName = graph_det$newVarName[i],
		                             addExtraShp = graph_det$addExtraShp[i],
		                             extraShp = eval(parse(text = graph_det$extraShp[i])))
		        res[[2]]
		        ggsave(paste(graph_det$png_dir[i], paste(graph_det$png_name[i], ".tiff", sep = ""), sep="/"), units="in", width=10, height=10, dpi=300, compression = 'lzw')
		        write.table(res[[1]], file =  paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".txt", sep=""), sep = '\t', dec = '.')
		      }
		      if(graph_det$Graph_type[i]==4)
		      {
		        res = choroplethMap_func(cl_rcg_group, var = graph_det$var[i],  groupBy=graph_det$groupBy[i], facet = graph_det$facet[i],
		                                 func = graph_det$func[i], type_of_threshold = graph_det$type_of_threshold[i], value_of_threshold =  graph_det$value_of_threshold[i],
		                                 points_coord =  eval(parse(text = graph_det$points_coord[i])), plot_labels = graph_det$plot_labels[i], saveResults = FALSE,
		                                 Catch_group = graph_det$Catch_group[i], 
		                                 newVarName = graph_det$newVarName[i],
		                                 addExtraShp = graph_det$addExtraShp[i],
		                                 extraShp = eval(parse(text = graph_det$extraShp[i])))
		        res[[2]]
		        ggsave(paste(graph_det$png_dir[i], paste(graph_det$png_name[i], ".tiff", sep = ""), sep="/"), units="in", width=10, height=10, dpi=300, compression = 'lzw')
		        write.table(res[[1]], file =  paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".txt", sep=""), sep = '\t', dec = '.')

		      }		
		      if(graph_det$Graph_type[i]==5)
		      {
		        res = scatterpieMap_func(cl_rcg_group, var = graph_det$var[i],  groupBy=graph_det$groupBy[i], groupBy2 = graph_det$groupBy2[i] , facet = graph_det$facet[i],
		                                 func = graph_det$func[i], type_of_threshold = graph_det$type_of_threshold[i], value_of_threshold =  graph_det$value_of_threshold[i],
		                                 points_coord =  eval(parse(text = graph_det$points_coord[i])), plot_labels = graph_det$plot_labels[i], saveResults = FALSE,
		                                 Catch_group = graph_det$Catch_group[i], 
		                                 newVarName = graph_det$newVarName[i],
		                                 addExtraShp = graph_det$addExtraShp[i],
		                                 extraShp = eval(parse(text = graph_det$extraShp[i])), color_palette = aux_colours_ggplot)
		        res[[2]]
		        ggsave(paste(graph_det$png_dir[i], paste(graph_det$png_name[i], ".tiff", sep = ""), sep="/"), units="in", width=10, height=10, dpi=300, compression = 'lzw')
		        write.table(res[[1]], file =  paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".txt", sep=""), sep = '\t', dec = '.')
		        
		      }	
		    }
		  }
		
	# river flow	
		# add here	
		
	
	#CE graphs generic

		# read_graph_details
		graph_det_all <- read.table("graphical_parameters/RCG_NA/Annual_Overview/AnnualOverview_RCG_NA_CE_Graphical_details.txt", sep="\t", stringsAsFactors=FALSE, header=T)
		source("funs/func_barplot_var_by_one_var.r")			
		source("funs/func_barplot_var_by_two_var_stacked.r")		
		
		
		for(Catch_group in unique(graph_det_all$Catch_group))
		{
		
		print(paste("Catch group:",Catch_group))	
		# subsets Catch_group
			graph_det1<-graph_det_all[graph_det_all$Catch_group==Catch_group,]
			if(Catch_group!="NULL") ce_rcg_group<-ce_rcg[Catch_group==Catch_group] else ce_rcg_group<-ce_rcg
		
		for (Graph_group in unique(graph_det1$Graph_group))
		{
		print(paste("Graph group:",Graph_group))		
		graph_det<-graph_det1[graph_det1$Graph_group==Graph_group,]
		
		# detects group
		if(nrow(graph_det)>1) {windows(10,10); par(oma = eval(parse(text=graph_det$graph_par))$oma, mfrow=eval(parse(text=graph_det$graph_par))$mfrow)}
		if(nrow(graph_det)==1) {windows(10,5)} # oma parameter is set inside the barplot function

		
		# runs graphs
		for (i in 1:nrow(graph_det))
		{
		print(i)
		if(graph_det$Graph_type[i]==1)
			{
			res<-barplot_var_by_one_var(x = as.data.frame(ce_rcg_group), Var = graph_det$Var[i] , var1 = graph_det$var1[i], tapply_type = graph_det$tapply_type[i], type_of_threshold = graph_det$type_of_threshold[i], value_of_threshold = graph_det$value_of_threshold[i], sorted=graph_det$sorted[i], graph_par = eval(parse(text=graph_det$graph_par[i])), grouped = graph_det$Grouped[i])
			savePlot(filename = paste(graph_det$png_dir[i], paste(graph_det$png_name[i],".png", sep=""), sep="/"), type="png")
			write.table(res, file = paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".txt", sep=""), sep="\t", dec=".", row.names = TRUE)
			write.xlsx(res, file = paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".xlsx", sep=""), sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE)
			#dev.off()
			}
		if(graph_det$Graph_type[i]==2)
			{
			res<-barplot_var_by_two_var_stacked(x = as.data.frame(ce_rcg_group), Var = graph_det$Var[i] , var1 = graph_det$var1[i], var2 = graph_det$var2[i], tapply_type = graph_det$tapply_type[i], proportion = graph_det$proportion[i], type_of_threshold = graph_det$type_of_threshold[i], value_of_threshold = graph_det$value_of_threshold[i], sorted=graph_det$sorted[i], graph_par = eval(parse(text=graph_det$graph_par[i])), legend_par = graph_det$legend_par)
			savePlot(filename = paste(graph_det$png_dir[i], paste(graph_det$png_name[i],".png", sep=""), sep="/"), type="png")
			write.table(res, file = paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".txt", sep=""), sep="\t", dec=".", row.names = TRUE)
			write.xlsx(res, file = paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".xlsx", sep=""), sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE)
			#dev.off()
			}			
		}
		}
		#graphics.off()
		}
		
	
	# CE graphs under and over 10 [grouped by size]
		# 2 independent graph_det files with the characteristics and names of u10 and 10o vessels
	
		
		# read_graph_details
		source("funs/func_barplot_var_by_one_var.r")			
		source("funs/func_barplot_var_by_two_var_stacked.r")		
		
		# activate the option you need
		graph_det_all <- read.table("graphical_parameters/RCG_NA/Annual_Overview/AnnualOverview_RCG_NA_CE_Graphical_details_u10.txt", sep="\t", stringsAsFactors=FALSE, header=T)
		ce_rcg2<-ce_rcg[VesselLengthCategory=="<10",]
		graph_det_all <- read.table("graphical_parameters/RCG_NA/Annual_Overview/AnnualOverview_RCG_NA_CE_Graphical_details_10o.txt", sep="\t", stringsAsFactors=FALSE, header=T)
		ce_rcg2<-ce_rcg[!VesselLengthCategory=="<10",]		
		
		for(Catch_group in unique(graph_det_all$Catch_group))
		{
		
		print(paste("Catch group:",Catch_group))	
		# subsets Catch_group
			graph_det1<-graph_det_all[graph_det_all$Catch_group==Catch_group,]
			if(Catch_group!="NULL") ce_rcg_group<-ce_rcg2[Catch_group==Catch_group] else ce_rcg_group<-ce_rcg2
		
		for (Graph_group in unique(graph_det1$Graph_group))
		{
		print(paste("Graph group:",Graph_group))		
		graph_det<-graph_det1[graph_det1$Graph_group==Graph_group,]
		
		# detects group
		if(nrow(graph_det)>1) {windows(7,10); par(oma = eval(parse(text=graph_det$graph_par))$oma, mfrow=eval(parse(text=graph_det$graph_par))$mfrow)}
		if(nrow(graph_det)==1) {windows(10,5)} # oma parameter is set inside the barplot function

		# runs graphs
		for (i in 1:nrow(graph_det))
		{
		print(i)
		if(graph_det$Graph_type[i]==1)
			{
			res<-barplot_var_by_one_var(x = as.data.frame(ce_rcg_group), Var = graph_det$Var[i] , var1 = graph_det$var1[i], tapply_type = graph_det$tapply_type[i], type_of_threshold = graph_det$type_of_threshold[i], value_of_threshold = graph_det$value_of_threshold[i], sorted=graph_det$sorted[i], graph_par = eval(parse(text=graph_det$graph_par[i])), grouped = graph_det$Grouped[i])
			savePlot(filename = paste(graph_det$png_dir[i], paste(graph_det$png_name[i],".png", sep=""), sep="/"), type="png")
			write.table(res, file = paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".txt", sep=""), sep="\t", dec=".", row.names = TRUE)
			write.xlsx(res, file = paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".xlsx", sep=""), sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE)
			#dev.off()
			}
		if(graph_det$Graph_type[i]==2)
			{
			res<-barplot_var_by_two_var_stacked(x = as.data.frame(ce_rcg_group), Var = graph_det$Var[i] , var1 = graph_det$var1[i], var2 = graph_det$var2[i], tapply_type = graph_det$tapply_type[i], proportion = graph_det$proportion[i], type_of_threshold = graph_det$type_of_threshold[i], value_of_threshold = graph_det$value_of_threshold[i], sorted=graph_det$sorted[i], graph_par = eval(parse(text=graph_det$graph_par[i])), legend_par = graph_det$legend_par, grouped = graph_det$Grouped[i])
			savePlot(filename = paste(graph_det$png_dir[i], paste(graph_det$png_name[i],".png", sep=""), sep="/"), type="png")
			write.table(res, file = paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".txt", sep=""), sep="\t", dec=".", row.names = TRUE)
			write.xlsx(res, file = paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".xlsx", sep=""), sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE)
			#dev.off()
			}			
		}
		}
		graphics.off()
		}		
		
		########################################################################################################################################################################
		# maps
		
		source("funs/pointsMap_func.R")
		source("funs/choroplethMap_func.R")
		source("funs/scatterpieMap_func.R")
		
	# run this part twice, once for <10 and once for >10
	# ce_rcg_vl<-ce_rcg[VesselLengthCategory=="<10",]
	#  titleAdd ='under 10m'
	ce_rcg_vl<-ce_rcg[!VesselLengthCategory=="<10" & !is.na(VesselLengthCategory),]
	titleAdd ='10m and over'
	
	graph_det_all <- read.table("graphical_parameters/RCG_NA/Annual_Overview/AnnualOverview_RCG_NA_CE_Graphical_details_maps.txt", sep="\t", stringsAsFactors=FALSE, header=T)
	
		for(group in unique(graph_det_all$Catch_group))
		{

		  print(group)

		  # subsets group
		  graph_det<-graph_det_all[graph_det_all$Catch_group==group,]
		  if(group!="NULL") ce_rcg_vl_group<-ce_rcg_vl[Catch_group==group] else ce_rcg_vl_group<-ce_rcg_vl

		  # runs graphs
		  for (i in 1:nrow(graph_det))
		  {
		    print(i)
		    if(graph_det$Graph_type[i]==3)
		    {
		      res = pointsMap_func(ce_rcg_vl_group, var = graph_det$var[i],  groupBy=graph_det$groupBy[i], facet = graph_det$facet[i],
		                           func = graph_det$func[i], type_of_threshold = graph_det$type_of_threshold[i], value_of_threshold =  graph_det$value_of_threshold[i],
		                           points_coord =  eval(parse(text = graph_det$points_coord[i])), plot_labels = graph_det$plot_labels[i], saveResults = FALSE,
		                           Catch_group = graph_det$Catch_group[i], 
		                           newVarName = graph_det$newVarName[i],
		                           addExtraShp = graph_det$addExtraShp[i],
		                           extraShp = eval(parse(text = graph_det$extraShp[i])),
		                           addToTitle = titleAdd)
		      res[[2]]
		      ggsave(paste(graph_det$png_dir[i], paste(graph_det$png_name[i], '_', titleAdd, ".tiff", sep = ""), sep="/"), units="in", width=10, height=10, dpi=300, compression = 'lzw')
		      write.table(res[[1]], file =  paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"), '_', titleAdd,".txt", sep=""), sep = '\t', dec = '.')

		    }
		    if(graph_det$Graph_type[i]==4)
		    {
		      res = choroplethMap_func(ce_rcg_vl_group, var = graph_det$var[i],  groupBy=graph_det$groupBy[i], facet = graph_det$facet[i],
		                               func = graph_det$func[i], type_of_threshold = graph_det$type_of_threshold[i], value_of_threshold =  graph_det$value_of_threshold[i],
		                               points_coord =  eval(parse(text = graph_det$points_coord[i])), plot_labels = graph_det$plot_labels[i], saveResults = FALSE,
		                               Catch_group = graph_det$Catch_group[i], 
		                               newVarName = graph_det$newVarName[i],
		                               addExtraShp = graph_det$addExtraShp[i],
		                               extraShp = eval(parse(text = graph_det$extraShp[i])),
		                               addToTitle = titleAdd)
		      res[[2]]
		      ggsave(paste(graph_det$png_dir[i], paste(graph_det$png_name[i], '_', titleAdd, ".tiff", sep = ""), sep="/"), units="in", width=10, height=10, dpi=300, compression = 'lzw')
		      write.table(res[[1]], file =  paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),'_', titleAdd,".txt", sep=""), sep = '\t', dec = '.')
		      
		    }
		    if(graph_det$Graph_type[i]==5)
		    {
		      res = scatterpieMap_func(ce_rcg_vl_group, var = graph_det$var[i],  groupBy=graph_det$groupBy[i], groupBy2 = graph_det$groupBy2[i] , facet = graph_det$facet[i],
		                               func = graph_det$func[i], type_of_threshold = graph_det$type_of_threshold[i], value_of_threshold =  graph_det$value_of_threshold[i],
		                               points_coord =  eval(parse(text = graph_det$points_coord[i])), plot_labels = graph_det$plot_labels[i], saveResults = FALSE,
		                               Catch_group = graph_det$Catch_group[i], 
		                               newVarName = graph_det$newVarName[i],
		                               addExtraShp = graph_det$addExtraShp[i],
		                               extraShp = eval(parse(text = graph_det$extraShp[i])),
		                               addToTitle = titleAdd, color_palette = aux_colours_ggplot)
		      res[[2]]
		      ggsave(paste(graph_det$png_dir[i], paste(graph_det$png_name[i], '_', titleAdd, ".tiff", sep = ""), sep="/"), units="in", width=10, height=10, dpi=300, compression = 'lzw')
		      write.table(res[[1]], file =  paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),'_', titleAdd, ".txt", sep=""), sep = '\t', dec = '.')
		      
		    }
		  }

		}
		 
	# river flow	
		# add here			
		
		
