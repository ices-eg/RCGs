# RCG subgroup work on Regional Fisheries and Sampling Overview
	# Nuno, Lucia, Sven, Marta, Gwladys, Hans, Henrik, Kirsten, Perttu, Alastair, Liz, Emilie, Joël
	# 2019
	
	
# script to produce graphs of Annual Overview



	# =======================	
	# section General
	# =======================
			
		source("funs/func_barplot_var_by_one_var.r")	
		source("funs/func_barplot_var_by_two_var_stacked.r")
				
		
		library(data.table)
		library(xlsx)
		
		# read data
		rm(list=ls())
		
		
		load("RDB_RCG_NA_CL_2009_2017_prepared_201904020909.Rdata")
		load("RDB_RCG_NA_CE_2009_2017_prepared_201904020909.Rdata")
		
		head(cl_rcg)
		
		cl_rcg<-droplevels(cl_rcg[Year==2017,])
		ce_rcg<-droplevels(ce_rcg[Year==2017,])
				
		#Other IDs
		cl_rcg[,FlagCountry_Loa:=paste(FlagCountry, VesselLengthCategory, sep="_")]
		ce_rcg[,FlagCountry_Loa:=paste(FlagCountry, VesselLengthCategory, sep="_")]

	
	#CL
		
		
		source("funs/func_barplot_var_by_one_var.r")			
		
		# read_graph_details
		graph_det_all <- read.table("RCG_NA_CL_Graphical_details1.txt", sep="\t", stringsAsFactors=FALSE, header=T)
		
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
		if(graph_det$Graph_type[i]==1)
			{
			res<-barplot_var_by_one_var(x = as.data.frame(cl_rcg_group), Var = graph_det$Var[i] , var1 = graph_det$var1[i], tapply_type = graph_det$tapply_type[i], type_of_threshold = graph_det$type_of_threshold[i], value_of_threshold = graph_det$value_of_threshold[i], sorted=graph_det$sorted[i], graph_par = eval(parse(text=graph_det$graph_par[i])))
			savePlot(filename = paste(graph_det$png_dir[i], paste(graph_det$png_name[i],".png", sep=""), sep="/"), type="png")
			write.table(res, file = paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".txt", sep=""), sep="\t", dec=".", row.names = TRUE)
			write.xlsx(res, file = paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".xlsx", sep=""), sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE)
			dev.off()
			}
		}
		graphics.off()
		}
		


		source("funs/func_barplot_var_by_two_var_stacked.r")			
		graph_det_all <- read.table("RCG_NA_CL_Graphical_details2.txt", sep="\t", stringsAsFactors=FALSE, header=T)
	
		group="small pelagic"
		i=4
	
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
		if(graph_det$Graph_type[i]==2)
			{
			res<-barplot_var_by_two_var_stacked(x = as.data.frame(cl_rcg_group), Var = graph_det$Var[i] , var1 = graph_det$var1[i], var2 = graph_det$var2[i], tapply_type = graph_det$tapply_type[i], proportion = graph_det$proportion[i], type_of_threshold = graph_det$type_of_threshold[i], value_of_threshold = graph_det$value_of_threshold[i], sorted=graph_det$sorted[i], graph_par = eval(parse(text=graph_det$graph_par[i])), legend_par = graph_det$legend_par)
			savePlot(filename = paste(graph_det$png_dir[i], paste(graph_det$png_name[i],".png", sep=""), sep="/"), type="png")
			write.table(res, file = paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".txt", sep=""), sep="\t", dec=".", row.names = TRUE)
			write.xlsx(res, file = paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".xlsx", sep=""), sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE)
			dev.off()
			}
		}
		graphics.off()
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
		  'D:/WG/RCG/IntersessionalWork/Subgroup on Regional Overviews/TestData/shp/FAO_AREAS_NOCOASTLINE.shp' # shp uploaded on sharepoint by Hans
		)
		shp %>%
		  filter((!is.na(F_DIVISION) &
		            is.na(F_SUBDIVIS)) |
		           (F_SUBDIVIS == '27.5.b.1' &
		              is.na(F_SUBUNIT)) |
		           (F_SUBDIVIS == '27.9.b.2' & is.na(F_SUBUNIT)) # to avoid duplicates, another solution?
		  ) %>%
		  mutate(Area = F_CODE) -> shp
		# add centroids - to put areas labels there, and to put piecharts there, creates new columns to the dataset named X, Y
		FAOshp = cbind(shp,  sf::st_coordinates(sf::st_centroid(shp$geometry))) %>% mutate(lon = X, lat = Y)
		
		StatRectshp  = sf::st_read(
		  'D:/maps/shapefile/ICES_Statistical_Rectangles_Eco/ICES_Statistical_Rectangles_Eco.shp' 
		)
		StatRectshp %>% mutate(StatisticalRectangle = ICESNAME)-> StatRectshp
		StatRectshp = cbind(StatRectshp,  sf::st_coordinates(sf::st_centroid(StatRectshp$geometry))) %>% mutate(lon = X, lat = Y)
		
		
		options(scipen=10000) # to remove scientific notation from the legend
		########################################################################################################################################################################
		
		#pointsMaps
		source("funs/pointsMap_func.R")
		
		graph_det_all <- read.table("RCG_NA_CL_Graphical_details3.txt", sep="\t", stringsAsFactors=FALSE, header=T)
		
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
		      res = pointsMap_func(cl_rcg_group, var = as.symbol(graph_det$var[i]),  groupBy=str_split(graph_det$groupBy[i], '_')[[1]],
		                           func = as.symbol(graph_det$func[i]), type_of_threshold = graph_det$type_of_threshold[i], value_of_threshold =  graph_det$value_of_threshold[i],
		                     points_coord = Harbours, plot_labels = as.symbol(graph_det$plot_labels[i]), time = as.symbol(graph_det$time[i]), saveResults = FALSE,
		                     Catch_group = graph_det$Catch_group[i])
		      res[[2]]
		      ggsave(paste(graph_det$png_dir[i], paste(graph_det$png_name[i], ".tiff", sep = ""), sep="/"), units="in", width=15, height=10, dpi=300, compression = 'lzw')
		      write.table(res[[1]], file =  paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".txt", sep=""), sep = '\t', dec = '.')
		      
		    }
		  }
		  
		}
		
		# choroplethMap
		source("funs/choroplethMap_func.R")
		
		graph_det_all <- read.table("RCG_NA_CL_Graphical_details4.txt", sep="\t", stringsAsFactors=FALSE, header=T)
		
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
		    if(graph_det$Graph_type[i]==4)
		    {
		      res = choroplethMap_func(cl_rcg_group, var = as.symbol(graph_det$var[i]),  groupBy=str_split(graph_det$groupBy[i], '_')[[1]],
		                               func = as.symbol(graph_det$func[i]), type_of_threshold = graph_det$type_of_threshold[i], value_of_threshold =  graph_det$value_of_threshold[i],
		                               points_coord = eval(parse(text = graph_det$points_coord[i])),
		                               plot_labels = as.symbol(graph_det$plot_labels[i]), time = as.symbol(graph_det$time[i]), saveResults = FALSE,
		                               Catch_group = graph_det$Catch_group[i])
		      res[[2]]
		      ggsave(paste(graph_det$png_dir[i], paste(graph_det$png_name[i], ".tiff", sep = ""), sep="/"), units="in", width=15, height=10, dpi=300, compression = 'lzw')
		      write.table(res[[1]], file =  paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".txt", sep=""), sep = '\t', dec = '.')
		      
		    }
		  }
		  
		}
		
		# scatterPieMap_func
		source("funs/scatterpieMap_func.R")
		
		graph_det_all <- read.table("RCG_NA_CL_Graphical_details5.txt", sep="\t", stringsAsFactors=FALSE, header=T)
		
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
		    if(graph_det$Graph_type[i]==5)
		    {
		      res = scatterpieMap_func(cl_rcg_group, var = as.symbol(graph_det$var[i]),  groupBy=str_split(graph_det$groupBy[i], '_')[[1]], plotBy = graph_det$plotBy[i],
		                               func = as.symbol(graph_det$func[i]), type_of_threshold = graph_det$type_of_threshold[i], value_of_threshold =  graph_det$value_of_threshold[i],
		                               points_coord =eval(parse(text = graph_det$points_coord[i])),# FAOshp,
		                               plot_labels = as.symbol(graph_det$plot_labels[i]), time = as.symbol(graph_det$time[i]), saveResults = FALSE,
		                               Catch_group = graph_det$Catch_group[i])
		      res[[2]]
		      ggsave(paste(graph_det$png_dir[i], paste(graph_det$png_name[i], ".tiff", sep = ""), sep="/"), units="in", width=15, height=10, dpi=300, compression = 'lzw')
		      write.table(res[[1]], file =  paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".txt", sep=""), sep = '\t', dec = '.')
		      
		    }
		  }
		  
		}
		
	# river flow	
		# add here	
		
	
	#CE
	
	
		source("funs/func_barplot_var_by_one_var.r")			
		
		# read_graph_details
		graph_det_all <- read.table("RCG_NA_CE_Graphical_details.txt", sep="\t", stringsAsFactors=FALSE, header=T)
		
		for(group in unique(graph_det_all$Catch_group))
		{
		
		print(group)	
		
		# subsets group
			graph_det<-graph_det_all[graph_det_all$Catch_group==group,]
			if(group!="NULL") ce_rcg_group<-ce_rcg[Catch_group==group] else ce_rcg_group<-ce_rcg
		
		# runs graphs
		for (i in 1:nrow(graph_det))
		{
		print(i)
		if(graph_det$Graph_type[i]==1)
			{
			res<-barplot_var_by_one_var(x = as.data.frame(ce_rcg_group), Var = graph_det$Var[i] , var1 = graph_det$var1[i], tapply_type = graph_det$tapply_type[i], type_of_threshold = graph_det$type_of_threshold[i], value_of_threshold = graph_det$value_of_threshold[i], sorted=graph_det$sorted[i], graph_par = eval(parse(text=graph_det$graph_par[i])))
			savePlot(filename = paste(graph_det$png_dir[i], paste(graph_det$png_name[i],".png", sep=""), sep="/"), type="png")
			write.table(res, file = paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".txt", sep=""), sep="\t", dec=".", row.names = TRUE)
			write.xlsx(res, file = paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".xlsx", sep=""), sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE)
			#dev.off()
			}
		}
		#graphics.off()
		}
		
		
	# maps
		# add here

	# river flow	
		# add here			
		
		
