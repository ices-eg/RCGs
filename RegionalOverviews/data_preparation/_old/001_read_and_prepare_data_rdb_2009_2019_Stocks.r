# RCG subgroup work on Regional Fisheries and Sampling Overview
	# Nuno, Lucia, Sven, Marta, Gwladys, Hans, Henrik, Kirsten, Perttu, Alastair, Liz, Emilie, Joël, Karolina, Katarzyna, Federico, Maria, 
	# 2019-2020
	
	# script prepares datasets for further analysis

	# 2020-04-17: created from 2009_2018 version
	

	# mac.nea
	 	rm(list=ls())
		gc()
		library(data.table)
		system.time(load("data/002_prepared/2020/RDB_All_Regions_CE_2009_2019_prepared_202004191807.Rdata"))
		system.time(load("data/002_prepared/2020/RDB_All_Regions_CL_2009_2019_prepared_202004191807.Rdata"))
		system.time(load("data/002_prepared/2020/RDB_All_Regions_CS_2009_2019_prepared_202004191807.Rdata"))

		# mac.27.nea: Mackerel (Scomber scombrus) in subareas 1-8 and 14 and division 9.a (the Northeast Atlantic and adjacent waters)
			cl_stock<-cl[Species == "Scomber scombrus",]	
			ce_stock<-ce
					
			sl_stock<-sl[Species == "Scomber scombrus",]
			hl_stock<-hl[CS_SpeciesListId %in% sl_stock$CS_SpeciesListId,]
			hh_stock<-hh[CS_StationId %in% unique(sl_stock$CS_StationId),]
			tr_stock<-tr[CS_TripId %in% unique(hh_stock$CS_TripId),]
			ca_stock<-ca[Species == "Scomber scombrus",]

		time_tag<-format(Sys.time(), "%Y%m%d%H%M")	
		year_start <- 2009	
		year_end <- 2019	
		stock_code <- "mac.nea"		
		output_dir<-paste("data/002_prepared/2020/Stocks/", stock_code, sep=""); dir.create(output_dir)
		save(cl_stock, ce_stock, tr_stock, hh_stock, sl_stock, hl_stock ,ca_stock, file_info_cs, file = paste(output_dir, paste("/RDB","All_Regions","CLCECS", stock_code, year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
