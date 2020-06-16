# RCG subgroup work on Regional Fisheries and Sampling Overview
	# Nuno, Lucia, Sven, Marta, Gwladys, Hans, Henrik, Kirsten, Perttu, Alastair, Liz, Emilie, Joël, Karolina, Katarzyna, Federico, Maria, 
	# 2019-2020
	
# script prepares datasets for further analysis

# note on CS subsets
	# two types of objects are produced: hh_rcg_all and hh_rcg
	# object rcg_all includes all hauls, etc of trips that registered >=1 haul in RCG areas [i.e., may be pan-regional]
	# object rcg includes only data from hauls RCG area
		# example:
			# if a long trip fished in BA and NSEA
				# all its data will be in rcg_all from BA and NSEA
				# only the data from hauls in BA will be in rcg (BA), only the data from hauls in NA will be in rcg (NA). If you merge the two you get the full trip present in rcg_all.

	# 2020-04-17: createdfrom 2009_2019 version
	# 2020-04-19: added a quality check
	# 2020-04-25: added a tweak on Sweden (27.3.d.28->27.3.d.28.2)
	# 2020-04-25: started adding stockNew var [not yet implemented]
	# 2020-05-04: added restrict_to_SSF_data [requested by SSF subgroup]
	# 2020-06-07: updated ASFIS_WoRMS_updt
	# 2020-06-16: updated Sepiidae ISSCAAP
	# 2020-06-16: fixes to species Catch_group "small-pelagic" (Perciformed, Sphyraena, Pomatomus, etc)
	# 2020-06-16: fixes to species Scomber japonicus in PRT records
	# 2020-06-16: fixed variable name "stock" to "Stock"
	# 2020-06-16: fixes to species Scomber japonicus in ESP records

# ========================
# downloads data from sharepoint
# ======================== 
 
source("funs/func_download_data_from_sharepoint.r")
 
# downloads rdb data from sharepoint 
	
	#sharepoint_address <- "ADD_HERE_website_address"
	#download_data_from_sharepoint (sharepoint_address, filename_vector = c("CL Landing 2009-2018.zip","CE Effort 2009-2018.zip"), dir_download_browser = "ADD_HERE_download_folder_adress", dir_download_target = getwd(), unzip=TRUE)


# ========================
# reads in data
# ========================

 rm(list=ls())
 library(data.table)
 gc()

# read file names
 	# file_cl <- "data/001_original/2020/20200417/CL Landing.zip"
	# file_ce <- "data/001_original/2020/20200417/CE Effort.zip" 
	# unzip("data/001_original/2020/20200417/CL Landing.zip",exdir="data/001_original/2020/20200417")
	# unzip("data/001_original/2020/20200417/CE Effort.zip",exdir="data/001_original/2020/20200417")
	file_cl <- "data/001_original/2020/20200610/CL Landing.csv"
	file_ce <- "data/001_original/2020/20200610/CE Effort.csv" 
 
 
# read data
	#cl<-fread(file_cl, stringsAsFactors=FALSE, verbose=FALSE, TRUE, sep=";", na.strings="NULL", nrows=9797287)
	ce<-fread(file_ce, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=",", na.strings="NULL")
	cl<-fread(file_cl, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=",", na.strings="NULL")
	#ce<-fread(file_ce, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=";", na.strings="NULL", nrows = 1066183)

# reads aux_countries dataset
	aux_countries<-read.table("aux_countries.txt", sep=",", header=T, colClasses="character", na.strings = "")

# QCA: duplicates (eliminates if existing)
	dim(cl); cl<-unique(cl); dim(cl)
	dim(ce); ce<-unique(ce); dim(ce)
 
# ====================== 
# create directory structure
# ====================== 

dir.create(paste("data/002_prepared/2020/RCG_NA", sep=""),recursive=TRUE, showWarnings=FALSE)
dir.create(paste("data/002_prepared/2020/RCG_BA", sep=""),recursive=TRUE, showWarnings=FALSE)
dir.create(paste("data/002_prepared/2020/RCG_NSEA", sep=""),recursive=TRUE, showWarnings=FALSE)
 
	
# ====================== 
# Set Prep Options 
# ======================  

restrict_to_SSF_data <- FALSE
target_region <- "RCG_NA" # "RCG_NA", "RCG_BA"; RCG_NSEA
year_start <- 2009
year_end <- 2019
time_tag<-format(Sys.time(), "%Y%m%d")
#time_tag<-202006160931
dir_output_rcg<-paste("data/002_prepared/2020/",time_tag,"/",target_region,sep=""); dir.create(dir_output_rcg, recursive=T)
dir_output_all<-paste("data/002_prepared/2020",time_tag, sep="/"); dir.create(dir_output_all, recursive=T)


# ======================
# Tweak on areas/region 
# ====================== 
 
# CL and CE 
 
 # QCA: check for ambiguous records (e.g., 27.7; 27.3)
	# Area 27.7
		ce[Area=="27.7",.N,c("Year","FlagCountry","Region","FishingGround","Area")]
		ce[Area=="27.7",.N,c("Year","FlagCountry","Region","FishingGround","Area","StatisticalRectangle")]
		# corrects
		#ce[Area=="27.7" & FlagCountry=="IRL","FishingGround"]<-NA
		ce[Area=="27.7" & FlagCountry=="IRL","StatisticalRectangle"]<-NA
	#	Area 27.3
		ce[Area=="27.3",.N,c("Year","FlagCountry","Region","FishingGround","Area")]
		# records from LTU "OTM_SPF_32-69_0_0" are likely NSEA
			ce[Area=="27.3" & FlagCountry=="LTU","Region"]<-"NSEA"
			ce[Area=="27.3" & FlagCountry=="LTU","FishingGround"]<-"3a"
			ce[Area=="27.3" & FlagCountry=="LTU","Area"]<-"27.3.a"
 			ce[Area=="27.3",.N, c("FlagCountry","Year","Region","FishingGround","Area")]
 			ce[Area=="27.3.a" & FlagCountry=="LTU",.N, c("FlagCountry","Year","Region","FishingGround","Area")]

	# issue in Harbour
		# fixes - ask MS to correction
		cl[Harbour=="POL-1303",Harbour:="RUPNY"]
		ce[Harbour=="POL-1303",Harbour:="RUPNY"]
			# also may be worth noting/correcting this
			cl[Harbour=="*HS-*HS",]
			ce[Harbour=="*HS-*HS",]			
	
	# issue in Area
		cl[Area=="27.3.d.28" & cl$FlagCountry=="SWE","Area"]<-"27.3.d.28.1"
			
# ========================
# subsets data and RCG specific preparations
# ========================	


# RCM Baltic: Baltic Sea (ICES areas III b-d)
 if(target_region=="RCG_BA") 
		{
		print(paste(".subsetting",target_region))
		
		target_areas <- c('27.3.b.23','27.3.c.22','27.3.d.24','27.3.d.25','27.3.d.26','27.3.d.27','27.3.d.28','27.3.d.28.1','27.3.d.28.2','27.3.d.29','27.3.d.30','27.3.d.31','27.3.d.32')	

		cl_rcg <- cl[Area %in% target_areas & Year>=year_start & Year<=year_end,]
			# QCA: should yield 0
			nrow(table(cl[Region=="BS" &  !Area %in% target_areas,"Area"]))
			nrow(table(cl[!Region=="BS" &  Area %in% target_areas,"Area"]))
			# QCA: should yield BS 
			#table(cl_rcg$Region)
				# corrects
				#cl_rcg[!Region=="BS",FishingGround:=NA,]	
				#cl_rcg[!Region=="BS",Region:="BS",]		
		
		ce_rcg <- ce[Area %in% target_areas & Year>=year_start & Year<=year_end,]
			# QCA: should yield 0
				# ATT: a few records 27.3 and 27.7 in BA?! 
				nrow(table(ce[Region=="BS" &  !Area %in% target_areas,"Area"]))
				nrow(table(ce[!Region=="BS" &  Area %in% target_areas,"Area"]))
			# QCA: should yield BS 
				#table(ce_rcg$Region)
				# corrects
					#ce_rcg[!Region=="BS",FishingGround:=NA,]				
					#ce_rcg[!Region=="BS",Region:="BS",]	
		
		}

# RCM NS&EA: the  North  Sea  (ICES  areas  IIIa,  IV  and  VIId),  the  Eastern  Arctic  (ICES  areas  I  and  II),  the  ICES  divisions Va, XII & XIV and the NAFO areas.
 if(target_region=="RCG_NSEA") 
		{
		print(paste(".subsetting",target_region))
		
		target_areas_nsea <- c('27.1','27.2','27.2.a','27.2.a.1','27.2.a.2','27.2.b','27.2.b.2','27.3.a','27.3.a.20','27.3.a.21','27.4','27.4.a','27.4.b','27.4.c','27.5.a','27.7.d','27.12','27.14','27.14.a','27.14.b','27.14.b.1','27.14.b.2')
		
		cl_rcg <-cl[ (Area %in% target_areas_nsea | grepl(Area, pat="21.") ) & Year>=year_start & Year<=year_end,]
		
			# QCA: should yield 0
				# ATT: a few records 41, 51 and 57 in NSEA [these have not been included in cl_rcg] 
				table(cl[Region=="NSEA" & !(Area %in% target_areas_nsea | grepl(Area, pat="21.") ),"Area"])
				table(cl[!Region=="NSEA" & (Area %in% target_areas_nsea | grepl(Area, pat="21.") ),"Area"])
			# QCA: should yield NSEA
				# ATT: a few records (21 - NAFO) not NSEA?!			
				table(cl_rcg$Region, useNA="al")
				table(cl_rcg[!Region=="NSEA",Area])
					# corrects				
					#cl_rcg[!Region=="NSEA" & (Area %in% target_areas_nsea | grepl(Area, pat="21.") ),Region:="NSEA",]
					#table(cl_rcg$Region)
			
		ce_rcg <- ce[ (Area %in% target_areas_nsea | grepl(Area, pat="21.") ) & Year>=year_start & Year<=year_end,]
			# QCA: should yield 0
				# ATT: a few records 41, 51 and 57 in NSEA [these have not been included in ce_rcg] 
				table(ce[Region=="NSEA" & !(Area %in% target_areas_nsea | grepl(Area, pat="21.") ),"Area"])	
				table(ce[!Region=="NSEA" & (Area %in% target_areas_nsea | grepl(Area, pat="21.") ),"Area"])	
			# QCA: should yield NSEA
				# ATT: a few records (21 - NAFO) not NSEA?!			
				table(ce_rcg$Region, useNA="al")
				table(ce_rcg[!Region=="NSEA",Area])		
					# corrects	
					#ce_rcg[!Region=="NSEA" & grepl(Area, pat="21."),Region:="NSEA",]
					#table(ce_rcg$Region)
	
	
	
		}	
		
# RCM NA: the North Atlantic (ICES areas V-X, excluding Va and VIId)
 if(target_region=="RCG_NA") 
		{
		
		print(paste(".subsetting",target_region))
		
		# cl
		cl_rcg<-cl[(grepl(Area, pat="27.5") | 
							grepl (Area, pat="27.6") | 
								grepl (Area, pat="27.7") | 
									grepl (Area, pat="27.8") | 
										grepl (Area, pat="27.9")	 | 
											grepl (Area, pat="27.10") ) &  
												!grepl (Area, pat="27.5.a") &  !grepl (Area, pat="27.7.d") & Year>=year_start & Year<=year_end,]
			
			# QCA: should make sense
				print(cl[Region=="NA",.N, "Area"][order(Area)])		
			# QCA: should yield 0 lines
				print(cl_rcg[!Region=="NA",.N,"Region"])
				
	
		# ce			
		ce_rcg<-ce[(grepl(Area, pat="27.5") | 
							grepl (Area, pat="27.6") | 
								grepl (Area, pat="27.7") | 
									grepl (Area, pat="27.8") | 
										grepl (Area, pat="27.9")	 | 
											grepl (Area, pat="27.10") ) &  
												!grepl (Area, pat="27.5.a") &  !grepl (Area, pat="27.7.d") & Year>=year_start & Year<=year_end,]
			
			# QCA: should make sense
				print(ce[Region=="NA",.N,"Area"][order(Area)])		
			# QCA: should yield N0
				print(ce_rcg[!Region=="NA",.N,Area])
			
	}	

# ========================
# formats variables
# ======================== 
 
	# formats CL 
	cl[,HarbourDesc:=iconv(HarbourDesc, from="UTF-8", to="")]
	cl[,HarbourDesc:=toupper(HarbourDesc)]
	cl[,OfficialLandingCatchWeight:=as.numeric(OfficialLandingCatchWeight)]
	cl_rcg[,HarbourDesc:=iconv(HarbourDesc, from="UTF-8", to="")]
	cl_rcg[,HarbourDesc:=toupper(HarbourDesc)]
	cl_rcg[,OfficialLandingCatchWeight:=as.numeric(OfficialLandingCatchWeight)]
		# QCA: should yield 0
			sum(is.na(cl$OfficialLandingCatchWeight))
	
	# formats CE 
	ce[,HarbourDesc:=iconv(HarbourDesc, from="UTF-8", to="")]
	ce[,HarbourDesc:=toupper(HarbourDesc)]
	ce_rcg[,HarbourDesc:=iconv(HarbourDesc, from="UTF-8", to="")]
	ce_rcg[,HarbourDesc:=toupper(HarbourDesc)]

	# renames stock->Stock [move to extraction - email sent to henrik 2020-06-16]
	colnames(cl)[colnames(cl)=="stock"]<-"Stock"
		
# ========================	
# Creates additional variables
# ========================	

	# CL

		# OfficialLandingCatchWeight_1000ton
			cl[,OfficialLandingCatchWeight_ton := OfficialLandingCatchWeight/1000]
			cl[,OfficialLandingCatchWeight_1000ton := OfficialLandingCatchWeight/1000000]
			cl_rcg[,OfficialLandingCatchWeight_ton := OfficialLandingCatchWeight/1000]
			cl_rcg[,OfficialLandingCatchWeight_1000ton := OfficialLandingCatchWeight/1000000]
		# fleet segment (FlagCountry_Loa)
			cl[,FlagCountry_Loa:=paste(FlagCountry, VesselLengthCategory, sep="_")]
			cl_rcg[,FlagCountry_Loa:=paste(FlagCountry, VesselLengthCategory, sep="_")]
		# HarbourCountry (ISO3) and HarbourCountry2 (ISO2)
			cl[,HarbourCountry2:=substring(Harbour,1,2)]
			cl_rcg[,HarbourCountry2:=substring(Harbour,1,2)]
			cl[,HarbourCountry:=aux_countries$ISO3Code[match(HarbourCountry2, aux_countries$ISO2Code)]]
			cl_rcg[,HarbourCountry:=aux_countries$ISO3Code[match(HarbourCountry2, aux_countries$ISO2Code)]]
				# QCA: should yield TRUE otherwise debug on cl and cl_rcg
				nrow(cl[is.na(HarbourCountry) & !is.na(HarbourCountry2),]) == 0
		
	# CE 
		
		# KWDays_thousands
			ce[,KWDays_1000x := KWDays/1000]				
			ce_rcg[,KWDays_1000x := KWDays/1000]				
		# GTDays_thousands
			ce[,GTDays_1000x := GTDays/1000]		
			ce_rcg[,GTDays_1000x := GTDays/1000]		
		# fleet segment (FlagCountry_Loa)	
			ce[,FlagCountry_Loa:=paste(FlagCountry, VesselLengthCategory, sep="_")]
			ce_rcg[,FlagCountry_Loa:=paste(FlagCountry, VesselLengthCategory, sep="_")]
		# HarbourCountry (ISO3) and HarbourCountry2 (ISO2)			
			ce[,HarbourCountry2:=substring(Harbour,1,2)]
			ce_rcg[,HarbourCountry2:=substring(Harbour,1,2)]
			ce[,HarbourCountry:=aux_countries$ISO3Code[match(HarbourCountry2, aux_countries$ISO2Code)]]
			ce_rcg[,HarbourCountry:=aux_countries$ISO3Code[match(HarbourCountry2, aux_countries$ISO2Code)]]
				# QCA: should yield TRUE otherwise debug on ce and ce_rcg
				nrow(ce[is.na(HarbourCountry) & !is.na(HarbourCountry2),]) == 0

		
		
	# AreaMap
		cl_rcg[,AreaMap:=Area,]
		ce_rcg[,AreaMap:=Area,]
		
		if(target_region=="RCG_BA") 
			{		
			cl_rcg[AreaMap %in% c("27.3.d.28.1", "27.3.d.28.2"), AreaMap := "27.3.d.28"]
			ce_rcg[AreaMap %in% c("27.3.d.28.1", "27.3.d.28.2"), AreaMap := "27.3.d.28"]
			}
		if(target_region=="RCG_NSEA") 
			{		
			cl_rcg[AreaMap %in% c("21.1"), AreaMap := "NA"] # div required (minority of records)					
			ce_rcg[AreaMap %in% c("21.1"), AreaMap := "NA"] # div required (minority of records)		
			
			cl_rcg[AreaMap %in% c("21.3"), AreaMap := "NA"] # div required (minority of records)				
			ce_rcg[AreaMap %in% c("21.3"), AreaMap := "NA"] # div required (minority of records)		
			
			cl_rcg[AreaMap %in% c("27.2"), AreaMap := "NA"]	 # div required	(minority of records)				
			ce_rcg[AreaMap %in% c("27.2"), AreaMap := "NA"]	 # div required	(minority of records)	
			
			cl_rcg[AreaMap %in% c("27.3.a"), AreaMap := "NA"] # subdiv required	(minority of records)		
			ce_rcg[AreaMap %in% c("27.3.a"), AreaMap := "NA"] # subdiv required	(minority of records)

			cl_rcg[AreaMap %in% c("27.4"), AreaMap := "NA"]	 # div required	(minority of records)	
			ce_rcg[AreaMap %in% c("27.4"), AreaMap := "NA"]	 # div required	(minority of records)	
			
			cl_rcg[AreaMap %in% c("27.14"), AreaMap := "NA"] # div required	(some records)				
			ce_rcg[AreaMap %in% c("27.14"), AreaMap := "NA"] # div required	(some records)		
			
			cl_rcg[AreaMap %in% c("27.2.a.1", "27.2.a.2"), AreaMap := "27.2.a"]
			ce_rcg[AreaMap %in% c("27.2.a.1", "27.2.a.2"), AreaMap := "27.2.a"]
			
			cl_rcg[AreaMap %in% c("27.2.b.2"), AreaMap := "27.2.b"]			
			ce_rcg[AreaMap %in% c("27.2.b.2"), AreaMap := "27.2.b"]			
			
			cl_rcg[AreaMap %in% c("27.14.b.1", "27.14.b.2"), AreaMap := "27.14.b"]
			ce_rcg[AreaMap %in% c("27.14.b.1", "27.14.b.2"), AreaMap := "27.14.b"]
			}
		
		if(target_region=="RCG_NA") 
			{		
			cl_rcg[AreaMap %in% c("27.5.b.1","27.5.b.2"), AreaMap := "27.5.b"]
			ce_rcg[AreaMap %in% c("27.5.b.1","27.5.b.2"), AreaMap := "27.5.b"]
			
			cl_rcg[AreaMap %in% c("27.9.b.1", "27.9.b.2"), AreaMap := "27.9.b"]
			ce_rcg[AreaMap %in% c("27.9.b.1", "27.9.b.2"), AreaMap := "27.9.b"]						
			
			cl_rcg[AreaMap %in% c('27.6.a.n','27.6.a.s'), AreaMap := "27.6.a"]
			ce_rcg[AreaMap %in% c('27.6.a.n','27.6.a.s'), AreaMap := "27.6.a"]

			cl_rcg[AreaMap %in% c("27.10"), AreaMap := "NA"]	# div required	(minority of records)			
			ce_rcg[AreaMap %in% c("27.10"), AreaMap := "NA"]	# div required	(minority of records)	

			cl_rcg[AreaMap %in% c("27.6"), AreaMap := "NA"]		# div required	(minority of records)			
			ce_rcg[AreaMap %in% c("27.6"), AreaMap := "NA"]		# div required	(minority of records)				
			
			cl_rcg[AreaMap %in% c("27.7"), AreaMap := "NA"]		# div required	(minority of records)		
			ce_rcg[AreaMap %in% c("27.7"), AreaMap := "NA"]		# div required	(minority of records)	
			
			}			
	# QCA: visual
		cl_rcg[, list(N=.N,ton1000 = round(sum(OfficialLandingCatchWeight_1000ton),1)),list(AreaMap,Area)][order(AreaMap)]
		cl_rcg[, list(N=.N,ton1000 = round(sum(OfficialLandingCatchWeight_1000ton),1)),list(AreaMap,Area, FlagCountry, Year)][order(AreaMap)][AreaMap=="NA",]
		ce_rcg[, list(N=.N,TripsNumber = sum(TripsNumber)),list(AreaMap,Area)][order(AreaMap)]
		ce_rcg[, list(N=.N,TripsNumber = sum(TripsNumber)),list(AreaMap,Area, FlagCountry, Year)][order(AreaMap)][AreaMap=="NA",]
	
# ========================	
# Creates and tweaks ISSCAAP codes
# ========================		

	# wishlist: add an update to valid Aphia and Aphia SciNames	

	
	#require(fishPiCodes) # Ask Alastair Pout (MSS) if you don't have package
	#data(ASFIS_WoRMS)

	# 2019-03-27: email sent to fishpi2 wp2+3 participants on permissions to use the package
	# 2019-04-02: so far no answer - to allow review of code an updated version of fishPiCodes::ASFIS_WoRMS table (ASFIS_WoRMS_updt.csv) will be kept on the data sharepoint of the RCG subgroup (acessible only to Subgroup members)
	
	ASFIS_WoRMS_updt <- read.table (file="ASFIS_WoRMS_updt_20200607.csv", header=T, sep=";", stringsAsFactors=FALSE) # on the data sharepoint of the RCG subgroup

		# =====================
		# CL [note: at the moment this is only being run on cl_rcg, not on the larger cl]
		# =====================		
				
		cl_rcg[,ISSCAAP:=ASFIS_WoRMS_updt$ISSCAAP[match(cl_rcg$SpeciesAphiaID, ASFIS_WoRMS_updt$AphiaID_accepted)]]
		
			# QCA should yield zero, if not more tweaks are needed
			sum(is.na(cl_rcg$Species))
			
			# QCA should yield zero, if not more tweaks are needed
			sum(is.na(cl_rcg$ISSCAAP))
			
			cl_rcg[Species == "Cottus gobio",ISSCAAP:=13] # Miscellaneous freshwater fishes
			cl_rcg[Species == "Gymnocephalus cernua",ISSCAAP:=13] # Miscellaneous freshwater fishes
			cl_rcg[Species == "Salvelinus alpinus",ISSCAAP:=23] # Salmons, trouts, smelts
			cl_rcg[Species == "Salmo trutta fario",ISSCAAP:=23] # Salmons, trouts, smelts
			cl_rcg[Species == "Gasterosteidae",ISSCAAP:=25] # Miscellaneous diadromous fishes
			cl_rcg[Species == "Gaidropsarus guttatus",ISSCAAP:=32] # Cods, hakes, haddocks
			cl_rcg[Species == "Mullus barbatus",ISSCAAP:=33] # Miscellaneous coastal fishes
			cl_rcg[Species == "Liparis liparis",ISSCAAP:=33] #  Miscellaneous coastal fishes
			cl_rcg[Species == "Auxis rochei",ISSCAAP:=36] # Tunas, bonitos, billfishes
			cl_rcg[Species == "Auxis thazard",ISSCAAP:=36] # Tunas, bonitos, billfishes
			cl_rcg[Species == "Scombrinae Rafinesque",ISSCAAP:=36] # Tunas, bonitos, billfishes
			cl_rcg[Species == "Scomberesox saurus",ISSCAAP:=37] # Miscellaneous pelagic fishes
			cl_rcg[Species == "Selachii",ISSCAAP:=38] # Sharks, rays, chimaeras
			cl_rcg[Species == "Squatina",ISSCAAP:=38] # Sharks, rays, chimaeras
			cl_rcg[Species == "Rajella lintea",ISSCAAP:=38] # Sharks, rays, chimaeras
			cl_rcg[Species == "Pisces",ISSCAAP:=39] # Marine fishes not identified
			cl_rcg[Species == "Macropodia",ISSCAAP:=42] # Crabs, sea-spiders
			cl_rcg[Species == "Dardanus arrosor",ISSCAAP:=44] # King crabs, squat-lobsters
			cl_rcg[Species == "Pasiphaea",ISSCAAP:=45] # Shrimps, prawns
			cl_rcg[Species == "Pasiphaeidae",ISSCAAP:=45] # Shrimps, prawns
			cl_rcg[Species == "Dendrobranchiata",ISSCAAP:=45] # Shrimps, prawns
			cl_rcg[Species == "Crangon",ISSCAAP:=45] # Shrimps, prawns
			cl_rcg[Species == "Decapodiformes",ISSCAAP:=47] # Miscellaneous marine crustaceans
			cl_rcg[Species == "Megabalanus azoricus",ISSCAAP:=47] # Miscellaneous marine crustaceans
			cl_rcg[Species == "Gibbula",ISSCAAP:=52] # Abalones, winkles, conchs
			cl_rcg[Species == "Venerupis philippinarum",ISSCAAP:=56] # Clams, cockles, arkshells
			cl_rcg[Species == "Arcopagia crassa",ISSCAAP:=56] # Clams, cockles, arkshells
			cl_rcg[Species == "Loligo forbesii",ISSCAAP:=57] # Squids, cuttlefishes, octopuses
			cl_rcg[Species == "Sepiidae",ISSCAAP:=57] # Squids, cuttlefishes, octopuses
			cl_rcg[Species == "Sepiida",ISSCAAP:=57] # Squids, cuttlefishes, octopuses
			cl_rcg[Species == "Echinidae",ISSCAAP:=76] # Sea-urchins and other echinoderms
			cl_rcg[Species == "Laminaria",ISSCAAP:=91] # Brown seaweeds
			cl_rcg[Species == "",ISSCAAP:=91] # Brown seaweeds
			
			# QCA should be zero
			sum(is.na(cl_rcg$ISSCAAP))
				# code for debugging:
					# unique(cl_rcg[is.na(cl_rcg$ISSCAAP),"Species"]$Species)
			
		
		# Ammodytes and Norway Pout appear classified as "33" - better assign to "37"
			cl_rcg[grepl(Species, pat= "Ammodytes"),ISSCAAP:=37] # Miscellaneous pelagic fishes
			cl_rcg[grepl(Species, pat= "Ammodytidae"),ISSCAAP:=37]  # Miscellaneous pelagic fishes
			cl_rcg[grepl(Species, pat= "Trisopterus esmarkii"),ISSCAAP:=37] # Miscellaneous pelagic fishes
		
		# Actinopterygii 39
			cl_rcg[grepl(Species, pat= "Actinopterygii"),ISSCAAP:=39]
		# Perciformes passed to 39 
			cl_rcg[grepl(Species, pat= "Perciformes"),ISSCAAP:=39]

		# Adds RCG Catch_group
			aux_spp_categ<-read.table("001_Inputs_Species_Categ/Table_Species_Categ.txt", header=T, sep="\t")
			cl_rcg[,Catch_group:=aux_spp_categ$RCM_NSEA_categ[match(cl_rcg$ISSCAAP,aux_spp_categ$ISSCAAP)]]
			

			# QCA should be zero			
			sum(is.na(cl_rcg$Catch_group))
			
			# some additional tweaks
			cl_rcg[grepl(Species, pat="Trachurus"),Species:="Trachurus spp."]
			cl_rcg[grepl(Species, pat="Lepidorhombus"),Species:="Lepidorhombus spp."]
			cl_rcg[grepl(Species, pat="Lophi"),Species:="Lophiidae"]
			
			cl_rcg[Species == "Myxine glutinosa",Catch_group:="other"]
			cl_rcg[grepl(Species, pat= "Scombridae"),Catch_group:="other"]
			cl_rcg[Species == "Lichia amia",Catch_group:="large pelagic"]
			cl_rcg[Species == "Acipenser sturio",Catch_group:="diadromous"]
			cl_rcg[Species == "Coryphaena hippurus",Catch_group:="large pelagic"]
			cl_rcg[Species == "Pomatomus saltatrix",Catch_group:="large pelagic"]
			cl_rcg[Species == "Seriola lalandi",Catch_group:="large pelagic"]
			cl_rcg[Species == "Seriola dumerili",Catch_group:="large pelagic"]
			cl_rcg[Species == "Brama brama",Catch_group:="large pelagic"]
			cl_rcg[Species == "Sphyraena viridensis",Catch_group:="large pelagic"]
			cl_rcg[Species == "Sphyraena sphyraena",Catch_group:="large pelagic"]
			cl_rcg[Species == "Sphyraena",Catch_group:="large pelagic"]
			cl_rcg[Species == "Trachipterus arcticus",Catch_group:="large pelagic"]
			cl_rcg[Species == "Lampris guttatus",Catch_group:="large pelagic"]
			cl_rcg[Species == "Mola mola",Catch_group:="large pelagic"]
			cl_rcg[Species == "Mola",Catch_group:="large pelagic"]
			cl_rcg[Species == "Naucrates ductor",Catch_group:="demersal"]
		
			
			# give it a check (see if it makes sense)
			 # check demersal
				head(cl_rcg[Catch_group == "demersal",list(Kg=sum(OfficialLandingCatchWeight), KgLastYear=sum(OfficialLandingCatchWeight[Year==max(Year)])),list(Species)] [order(-Kg),],20)
			# check flatfish
				head(cl_rcg[Catch_group == "flatfish",list(Kg=sum(OfficialLandingCatchWeight), KgLastYear=sum(OfficialLandingCatchWeight[Year==max(Year)])),list(Species)] [order(-Kg),],20)
			# check small pelagic
				head(cl_rcg[Catch_group == "small pelagic",list(Kg=sum(OfficialLandingCatchWeight), KgLastYear=sum(OfficialLandingCatchWeight[Year==max(Year)])),list(Species)] [order(-Kg),],20)
			# check large pelagic
				head(cl_rcg[Catch_group == "large pelagic",list(Kg=sum(OfficialLandingCatchWeight), KgLastYear=sum(OfficialLandingCatchWeight[Year==max(Year)])),list(Species)] [order(-Kg),],20)
			# check molluscs
				head(cl_rcg[Catch_group == "molluscs",list(Kg=sum(OfficialLandingCatchWeight), KgLastYear=sum(OfficialLandingCatchWeight[Year==max(Year)])),list(Species)] [order(-Kg),],20)
			# check crustaceans
				head(cl_rcg[Catch_group == "crustaceans",list(Kg=sum(OfficialLandingCatchWeight), KgLastYear=sum(OfficialLandingCatchWeight[Year==max(Year)])),list(Species)] [order(-Kg),],20)
			# check	elasmobranchs
				head(cl_rcg[Catch_group == "elasmobranchs",list(Kg=sum(OfficialLandingCatchWeight), KgLastYear=sum(OfficialLandingCatchWeight[Year==max(Year)])),list(Species)] [order(-Kg),],20)
			# check	diadromous
				head(cl_rcg[Catch_group == "diadromous",list(Kg=sum(OfficialLandingCatchWeight), KgLastYear=sum(OfficialLandingCatchWeight[Year==max(Year)])),list(Species)] [order(-Kg),],20)
			# check	incidental by-catch
				head(cl_rcg[Catch_group == "incidental by-catch",list(Kg=sum(OfficialLandingCatchWeight), KgLastYear=sum(OfficialLandingCatchWeight[Year==max(Year)])),list(Species)] [order(-Kg),],20)			
			# check	other
				head(cl_rcg[Catch_group == "other",list(Kg=sum(OfficialLandingCatchWeight), KgLastYear=sum(OfficialLandingCatchWeight[Year==max(Year)])),list(Species)] [order(-Kg),],20)

		
# ========================
# factorization [establishes the order in unsorted bar graphs]
# ========================
	
	cl[,FlagCountry:=factor(FlagCountry, levels=sort(unique(FlagCountry))),]
	cl[,LandingCountry:=factor(LandingCountry, levels=sort(unique(LandingCountry))),]
	cl[,FishingActivityCategoryEuropeanLvl5:=factor(FishingActivityCategoryEuropeanLvl5, levels=sort(unique(FishingActivityCategoryEuropeanLvl5))),]
	cl[,FishingActivityCategoryEuropeanLvl6:=factor(FishingActivityCategoryEuropeanLvl6, levels=sort(unique(FishingActivityCategoryEuropeanLvl6))),]
	cl[,Harbour:=factor(Harbour, levels=sort(unique(Harbour))),]
	cl[,Species:=factor(Species, levels=sort(unique(Species))),]
	cl[,VesselLengthCategory:=factor(VesselLengthCategory, levels=c("<10","10-<12","12-<18","18-<24","24-<40",">40"))]
	
	cl_rcg[,FlagCountry:=factor(FlagCountry, levels=sort(unique(FlagCountry))),]
	cl_rcg[,LandingCountry:=factor(LandingCountry, levels=sort(unique(LandingCountry))),]
	cl_rcg[,FishingActivityCategoryEuropeanLvl5:=factor(FishingActivityCategoryEuropeanLvl5, levels=sort(unique(FishingActivityCategoryEuropeanLvl5))),]
	cl_rcg[,FishingActivityCategoryEuropeanLvl6:=factor(FishingActivityCategoryEuropeanLvl6, levels=sort(unique(FishingActivityCategoryEuropeanLvl6))),]
	cl_rcg[,Harbour:=factor(Harbour, levels=sort(unique(Harbour))),]
	cl_rcg[,Species:=factor(Species, levels=sort(unique(Species))),]
	cl_rcg[,VesselLengthCategory:=factor(VesselLengthCategory, levels=c("<10","10-<12","12-<18","18-<24","24-<40",">40"))]

	ce[,FlagCountry:=factor(FlagCountry, levels=sort(unique(FlagCountry))),]
	ce[,FishingActivityCategoryEuropeanLvl5:=factor(FishingActivityCategoryEuropeanLvl5, levels=sort(unique(FishingActivityCategoryEuropeanLvl5))),]
	ce[,FishingActivityCategoryEuropeanLvl6:=factor(FishingActivityCategoryEuropeanLvl6, levels=sort(unique(FishingActivityCategoryEuropeanLvl6))),]
	ce[,Harbour:=factor(Harbour, levels=sort(unique(Harbour))),]
	ce[,VesselLengthCategory:=factor(VesselLengthCategory, levels=c("<10","10-<12","12-<18","18-<24","24-<40",">40"))]
	ce[,DaysAtSea:=as.numeric(DaysAtSea)]
	ce[,KWDays:=as.numeric(KWDays)]
	ce[,GTDays:=as.numeric(GTDays)]
	ce[,KWDays_1000x:=as.numeric(KWDays_1000x)]
	ce[,GTDays_1000x:=as.numeric(GTDays_1000x)]

	
	ce_rcg[,FlagCountry:=factor(FlagCountry, levels=sort(unique(FlagCountry))),]
	ce_rcg[,FishingActivityCategoryEuropeanLvl5:=factor(FishingActivityCategoryEuropeanLvl5, levels=sort(unique(FishingActivityCategoryEuropeanLvl5))),]
	ce_rcg[,FishingActivityCategoryEuropeanLvl6:=factor(FishingActivityCategoryEuropeanLvl6, levels=sort(unique(FishingActivityCategoryEuropeanLvl6))),]
	ce_rcg[,Harbour:=factor(Harbour, levels=sort(unique(Harbour))),]
	ce_rcg[,VesselLengthCategory:=factor(VesselLengthCategory, levels=c("<10","10-<12","12-<18","18-<24","24-<40",">40"))]
	ce_rcg[,DaysAtSea:=as.numeric(DaysAtSea)]
	ce_rcg[,KWDays:=as.numeric(KWDays)]
	ce_rcg[,GTDays:=as.numeric(GTDays)]
	ce_rcg[,KWDays_1000x:=as.numeric(KWDays_1000x)]
	ce_rcg[,GTDays_1000x:=as.numeric(GTDays_1000x)]

if (restrict_to_SSF_data==TRUE)
	{
	cl<-cl[VesselLengthCategory %in% c("<10","10-<12"),]
	cl<-cl[,VesselLengthCategory:=factor(VesselLengthCategory, levels=c("<10","10-<12"))]
	cl_rcg<-cl_rcg[VesselLengthCategory %in% c("<10","10-<12"),]
	cl_rcg[,VesselLengthCategory:=factor(VesselLengthCategory, levels=c("<10","10-<12"))]
	ce<-ce[VesselLengthCategory %in% c("<10","10-<12"),]
	ce[,VesselLengthCategory:=factor(VesselLengthCategory, levels=c("<10","10-<12"))]
	ce_rcg<-ce_rcg[VesselLengthCategory %in% c("<10","10-<12"),]
	ce_rcg[,VesselLengthCategory:=factor(VesselLengthCategory, levels=c("<10","10-<12"))]
	}


	# region specific	
	if(target_region=="RCG_BA"){
	target_FishingGround<-c("22-24","25-32")
		print(cl_rcg[!FishingGround %in% c(NA,target_FishingGround),.N, "FlagCountry"])
		print(ce_rcg[!FishingGround %in% c(NA,target_FishingGround),.N, "FlagCountry"])	
		cl_rcg[,FishingGround:=factor(FishingGround, levels=target_FishingGround),]
		ce_rcg[,FishingGround:=factor(FishingGround, levels=target_FishingGround),]	

		target_Areas<-c('27.3.b.23','27.3.c.22','27.3.d.24','27.3.d.25','27.3.d.26','27.3.d.27','27.3.d.28','27.3.d.28.1','27.3.d.28.2','27.3.d.29','27.3.d.30','27.3.d.31','27.3.d.32')
		print(cl_rcg[!Area %in% target_Areas,.N, c("FlagCountry","Area")])
		print(ce_rcg[!Area %in% target_Areas,.N, c("FlagCountry","Area")])
		cl_rcg[,Area:=factor(Area, levels=target_Areas),]
		ce_rcg[,Area:=factor(Area, levels=target_Areas),]
				
		# AreaMap
		target_AreaMap<-c('27.3.b.23','27.3.c.22','27.3.d.24','27.3.d.25','27.3.d.26','27.3.d.27','27.3.d.28', '27.3.d.29','27.3.d.30','27.3.d.31','27.3.d.32')
		cl_rcg[,AreaMap:=factor(AreaMap, levels=target_AreaMap),]
		ce_rcg[,AreaMap:=factor(AreaMap, levels=target_AreaMap),]

	}
	if(target_region=="RCG_NSEA"){
		target_FishingGround<-c("1+2", "3a", "4+7d", "5a+12+14", "21.0-6")
		print(cl_rcg[!FishingGround %in% c(NA,target_FishingGround),.N, "FlagCountry"])
		print(ce_rcg[!FishingGround %in% c(NA,target_FishingGround),.N, "FlagCountry"])	
		cl_rcg[,FishingGround:=factor(FishingGround, levels=target_FishingGround),]
		ce_rcg[,FishingGround:=factor(FishingGround, levels=target_FishingGround),]	
	
		# Areas
		target_Areas<-c('27.1','27.2','27.2.a','27.2.a.1','27.2.a.2','27.2.b','27.2.b.2','27.3.a','27.3.a.20','27.3.a.21','27.4','27.4.a','27.4.b','27.4.c','27.5.a','27.7.d','27.12','27.14','27.14.a','27.14.b','27.14.b.1','27.14.b.2','21.0.A','21.0.B','21.1','21.1.A','21.1.B','21.1.C','21.1.D','21.1.E','21.1.F','21.2.H','21.2.J','21.3','21.3.K','21.3.L','21.3.M','21.3.N','21.3.O','21.6.G')
		print(cl_rcg[!Area %in% target_Areas,.N, c("FlagCountry","Area")])
		print(ce_rcg[!Area %in% target_Areas,.N, c("FlagCountry","Area")])
		cl_rcg[,Area:=factor(Area, levels=target_Areas),]
		ce_rcg[,Area:=factor(Area, levels=target_Areas),]

		# AreaMap
		target_AreaMap<-c('27.1', '27.2.a','27.2.b','27.3.a.20','27.3.a.21','27.4.a','27.4.b','27.4.c','27.5.a','27.7.d','27.12', '27.14.a','27.14.b', '21.0.A','21.0.B','21.1','21.1.A','21.1.B','21.1.C','21.1.D','21.1.E','21.1.F','21.2.H','21.2.J','21.3','21.3.K','21.3.L','21.3.M','21.3.N','21.3.O','21.6.G')
		cl_rcg[,AreaMap:=factor(AreaMap, levels=target_AreaMap),]
		ce_rcg[,AreaMap:=factor(AreaMap, levels=target_AreaMap),]		
		
	}
	
	if(target_region=="RCG_NA"){
		target_FishingGround<-c("5b","6","7a","7bcjk","7e","7fgh","8abde","8c+9","10")
		print(cl_rcg[!FishingGround %in% c(NA,target_FishingGround),.N, "FlagCountry"])
		print(ce_rcg[!FishingGround %in% c(NA,target_FishingGround),.N, "FlagCountry"])
		cl_rcg[,FishingGround:=factor(FishingGround, levels=target_FishingGround),]
		ce_rcg[,FishingGround:=factor(FishingGround, levels=target_FishingGround),]
		
		# Areas
		target_Areas<-c('27.5.b','27.5.b.1','27.5.b.2','27.6','27.6.a','27.6.a.n','27.6.a.s','27.6.b','27.7','27.7.a','27.7.b','27.7.c','27.7.e','27.7.f','27.7.g','27.7.h','27.7.j','27.7.k','27.8.a','27.8.b','27.8.c','27.8.d','27.8.e','27.9.a','27.9.b','27.9.b.1','27.9.b.2','27.10','27.10.a','27.10.b')
		print(cl_rcg[!Area %in% target_Areas,.N, c("FlagCountry","Area")])
		print(ce_rcg[!Area %in% target_Areas,.N, c("FlagCountry","Area")])
		cl_rcg[,Area:=factor(Area, levels=target_Areas),]
		ce_rcg[,Area:=factor(Area, levels=target_Areas),]
		

		# AreaMap
		target_AreaMap<-c('27.5.b', '27.6.a', '27.6.b', '27.7.a','27.7.b','27.7.c','27.7.e','27.7.f','27.7.g','27.7.h','27.7.j','27.7.k','27.8.a','27.8.b','27.8.c','27.8.d','27.8.e','27.9.a','27.9.b', '27.10.a','27.10.b','27.14.a','27.14.b')
		cl_rcg[,AreaMap:=factor(AreaMap, levels=target_AreaMap),]
		ce_rcg[,AreaMap:=factor(AreaMap, levels=target_AreaMap),]				
		
		}	

	# QCA: visual
		cl_rcg[, list(N=.N,ton1000 = round(sum(OfficialLandingCatchWeight_1000ton),1)),list(AreaMap,Area)][order(AreaMap)]
		cl_rcg[, list(N=.N,ton1000 = round(sum(OfficialLandingCatchWeight_1000ton),1)),list(AreaMap,Area, FlagCountry, Year)][order(AreaMap)][is.na(AreaMap),]
		ce_rcg[, list(N=.N,TripsNumber = sum(TripsNumber)),list(AreaMap,Area)][order(AreaMap)]
		ce_rcg[, list(N=.N,TripsNumber = sum(TripsNumber)),list(AreaMap,Area, FlagCountry, Year)][order(AreaMap)][is.na(AreaMap),]
		
# ================	
# data update: IRL	
# ================	
	
	# update: TripsNumber == 999 are TripsNumber = 0
	ce[TripsNumber==999 & FlagCountry=="IRL",TripsNumber:=0,]
	ce_rcg[TripsNumber==999 & FlagCountry=="IRL",TripsNumber:=0,]


# ================	
# data update: PRT [authorization email 2020-06-16]
# ================	
	# update "Scomber japonicus" is the old name of "Scomber colias"
	cl_rcg[Species=="Scomber japonicus" & FlagCountry=="PRT","SpeciesAphiaID"]<-151174
	cl_rcg[Species=="Scomber japonicus" & FlagCountry=="PRT","SpeciesDesc"]<-"Atlantic chub mackerel"
	cl_rcg[Species=="Scomber japonicus" & FlagCountry=="PRT","Species"]<-"Scomber colias"

	cl[Species=="Scomber japonicus" & FlagCountry=="PRT","SpeciesAphiaID"]<-151174
	cl[Species=="Scomber japonicus" & FlagCountry=="PRT","SpeciesDesc"]<-"Atlantic chub mackerel"
	cl[Species=="Scomber japonicus" & FlagCountry=="PRT","Species"]<-"Scomber colias"

	
# ================	
# data update: ESP [authorization email 2020-06-16]
# ================	
	# update "Scomber japonicus" is the old name of "Scomber colias"
	cl_rcg[Species=="Scomber japonicus" & FlagCountry=="ESP","SpeciesAphiaID"]<-151174
	cl_rcg[Species=="Scomber japonicus" & FlagCountry=="ESP","SpeciesDesc"]<-"Atlantic chub mackerel"
	cl_rcg[Species=="Scomber japonicus" & FlagCountry=="ESP","Species"]<-"Scomber colias"	
	
	cl[Species=="Scomber japonicus" & FlagCountry=="ESP","SpeciesAphiaID"]<-151174
	cl[Species=="Scomber japonicus" & FlagCountry=="ESP","SpeciesDesc"]<-"Atlantic chub mackerel"
	cl[Species=="Scomber japonicus" & FlagCountry=="ESP","Species"]<-"Scomber colias"	



# ================
# quick and dirty check
# ================	
	# country level
	
	test_ctry<-"ESP"
		
		cl_rcg[FlagCountry==test_ctry,sum(OfficialLandingCatchWeight),list(Year)]
		cl[FlagCountry==test_ctry,sum(OfficialLandingCatchWeight),list(Year)]

		ce_rcg[FlagCountry==test_ctry,sum(TripsNumber),list(Year)]
		ce[FlagCountry==test_ctry,sum(TripsNumber),list(Year)]
	
	# all countries: 2018-2019
		cl[Year %in% c(2018,2019),sum(OfficialLandingCatchWeight),list(FlagCountry, Year)] [order(FlagCountry, Year)]
		ce[Year %in% c(2018,2019),sum(TripsNumber),list(FlagCountry, Year)] [order(FlagCountry, Year)]
	
# ================
# creates a few additional variables with shorter names (convenient for titles of barplot and maps sake)
# ================		
	# note: duplication to be avoided in the future after group discussion
	 
	 cl[, LandingWeight_ton:=OfficialLandingCatchWeight_ton]    
	 cl_rcg[, LandingWeight_ton:=OfficialLandingCatchWeight_ton]    
	 
	 cl[, LandingWeight_1000ton:=OfficialLandingCatchWeight_1000ton]    
	 cl_rcg[, LandingWeight_1000ton:=OfficialLandingCatchWeight_1000ton]    
	 
	 cl[, FishingActivityLvl5:=FishingActivityCategoryEuropeanLvl5]    
	 cl_rcg[, FishingActivityLvl5:=FishingActivityCategoryEuropeanLvl5]    
	 
	 cl[, FishingActivityLvl6:=FishingActivityCategoryEuropeanLvl6]    
	 cl_rcg[, FishingActivityLvl6:=FishingActivityCategoryEuropeanLvl6]    
	 
	 ce[, FishingActivityLvl5:=FishingActivityCategoryEuropeanLvl5]    
	 ce_rcg[, FishingActivityLvl5:=FishingActivityCategoryEuropeanLvl5]    
	 
	 ce[, FishingActivityLvl6:=FishingActivityCategoryEuropeanLvl6]    
	 ce_rcg[, FishingActivityLvl6:=FishingActivityCategoryEuropeanLvl6]    

# ===================
# adds stock
# ===================

# reads aux_countries dataset
	# aux_stocks<-read.table("aux_stocks.txt", sep="\t", header=T, colClasses="character", encoding="UTF-8")
	
	# # prepares cl for matching
	# cl$ID<-NA
	# target_rows1<-cl$Species=="Nephrops norvegicus" & !cl$Area %in% c("27.3.a","27.3.a.20","27.3.a.21")
	# cl$ID[target_rows1]<-paste(cl$SpeciesAphiaID[target_rows1], cl$StatisticalRectangle[target_rows1])
	# target_rows2<-cl$Species=="Ammodytes" & !is.na(cl$StatisticalRectangle) & cl$StatisticalRectangle %in% aux_stocks$area[aux_stocks$Code %in% c("san.sa.1r","san.sa.2r","san.sa.3r","san.sa.4","san.sa.5r","san.sa.6", "san.sa.7r")]
	# cl$ID[target_rows2]<-paste(cl$SpeciesAphiaID[target_rows2], cl$StatisticalRectangle[target_rows2])
	# target_rows3<-cl$Species=="Pandalus borealis" & cl$Area %in% c("27.4","27.4.a")
	# cl$ID[target_rows3]<-paste(cl$SpeciesAphiaID[target_rows3], cl$StatisticalRectangle[target_rows3])
	
	# target_rows4<-cl$Species=="Gadus morhua" & cl$Area %in% c("27.1","27.2","27.1.b","27.2.a","27.2.a.1", "27.2.a.2","27.2.b","27.2.b.2") & !is.na(cl$StatisticalRectangle)
	# cl$ID[target_rows4]<-paste(cl$SpeciesAphiaID[target_rows4], cl$StatisticalRectangle[target_rows4])
	
	# cl$ID[!(target_rows1 | target_rows2 | target_rows3 | target_rows4)]<-paste(cl$SpeciesAphiaID[!(target_rows1 | target_rows2 | target_rows3 | target_rows4)], cl$Area[!(target_rows1 | target_rows2 | target_rows3 | target_rows4)])
		# # QCA: should yield 0
		# sum(is.na(cl$ID))
	
	# # prepares aux_stocks for matching
	# aux_stocks$ID<-paste(aux_stocks$AphiaID,aux_stocks$area)
	
	# cl[, stockNew:=aux_stocks$Code[match(cl$ID, aux_stocks$ID)]]   
	# table(cl$stockNew)

	# cl[!is.na(stockNew) & Year==2019,list(V1=sum(OfficialLandingCatchWeight)), list(FlagCountry, Species, stockNew)][order(-V1),]
	# cl[!is.na(stockNew) & Year==2019,list(V1=sum(OfficialLandingCatchWeight)), list(stockNew)][order(-V1),][grepl(stockNew, pat="cod.27.1-2"),]
	
	# cl[grepl(Area, pat="27.") & Year==2019,list(withStock=sum(OfficialLandingCatchWeight[!is.na(stockNew)]), withoutStock=sum(OfficialLandingCatchWeight[is.na(stockNew)]), propWithoutStock=sum(OfficialLandingCatchWeight[is.na(stockNew)])/sum(OfficialLandingCatchWeight)), list(Species)]
	# # species with stocks
	# target_species<-unique(cl$Species[!is.na(cl$stockNew)])
	# out<-cl[grepl(Area, pat="27.") & Year==2019 & Species %in% target_species, list(withStock=sum(OfficialLandingCatchWeight[!is.na(stockNew)]), withoutStock=sum(OfficialLandingCatchWeight[is.na(stockNew)]), propWithoutStock=round(sum(OfficialLandingCatchWeight[is.na(stockNew)])/sum(OfficialLandingCatchWeight)*100,1), areasWithoutStock=paste(sort(unique(Area[is.na(stockNew)])), collapse=","), stocksPresent=paste(sort(unique(stockNew[!is.na(stockNew)])), collapse=",")), list(Species)]
	# out1<-cl[grepl(Area, pat="27.") & Year==2019 & Species %in% target_species, list(withStock=sum(OfficialLandingCatchWeight[!is.na(stockNew)]), withoutStock=sum(OfficialLandingCatchWeight[is.na(stockNew)]), propWithoutStock=round(sum(OfficialLandingCatchWeight[is.na(stockNew)])/sum(OfficialLandingCatchWeight)*100,1), areasWithoutStock=paste(sort(unique(Area[is.na(stockNew)])), collapse=","), stocksPresent=paste(sort(unique(stockNew[!is.na(stockNew)])), collapse=",")), list(FlagCountry, Species)]
	
	# xlsx::write.xlsx(out, file="check_stocks_updt3.xlsx")
	# xlsx::write.xlsx(out1, file="check_stocks_updt3.xlsx", sheetName="Sheet2", append=T)
	
	
	# # some subsisting NAs may be due to absence of rectangles
	# cl[grepl(Area, pat="27.") & Year==2019,,]
	# cl[is.na(stockNew) & Year==2019 & FlagCountry=="ESP" & Species ==  "Gadus morhua",]
	
	# head(cl[grepl(Area, pat="27.") & is.na(stockNew) & Year==2019 & FlagCountry=="SWE",list(V1=sum(OfficialLandingCatchWeight)), list(FlagCountry, Species, Area, stockNew)][order(-V1),],10)
	# cl[grepl(Area, pat="27.") & is.na(stockNew) & Year==2019 & FlagCountry=="ESP",list(V1=sum(OfficialLandingCatchWeight)), list(FlagCountry, Species, Area, stockNew)][order(-V1),]

	
# ========================
# saves data
# ========================	

	file_info_cl<-file.info(file_cl)
	file_info_ce<-file.info(file_ce)


if (restrict_to_SSF_data==FALSE)
	{
	save(cl_rcg, file_info_cl, file = paste(dir_output_rcg, paste("\\RDB",target_region,"CL", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
	save(ce_rcg, file_info_ce, file = paste(dir_output_rcg, paste("\\RDB",target_region,"CE", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
	save(cl, file_info_cl, file = paste(dir_output_all, paste("\\RDB","All_Regions","CL", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
	save(ce, file_info_ce, file = paste(dir_output_all, paste("\\RDB","All_Regions","CE", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
	} else  {
	print(1)
	save(cl_rcg, file_info_cl, file = paste(dir_output_rcg, paste("\\RDB",target_region,"CL_SSF", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
	save(ce_rcg, file_info_ce, file = paste(dir_output_rcg, paste("\\RDB",target_region,"CE_SSF", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
	save(cl, file_info_cl, file = paste(dir_output_all, paste("\\RDB","All_Regions","CL_SSF", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
	save(ce, file_info_ce, file = paste(dir_output_all, paste("\\RDB","All_Regions","CE_SSF", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))	
	}


	
