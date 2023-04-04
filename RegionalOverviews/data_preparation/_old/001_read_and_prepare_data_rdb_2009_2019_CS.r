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

				
	# 2020-04-17: created from 2009_2018 version
	# 2020-04-17: added unzip option
	# 2020-04-17: updated a few catch groups

			
# ========================
# downloads data from sharepoint
# ======================== 
 
source("funs/func_download_data_from_sharepoint.r")
 
# downloads rdb data from sharepoint 
	
	#sharepoint_address <- "ADD_HERE_website_address"
	#download_data_from_sharepoint (sharepoint_address, filename_vector = c("CL Landing 2009-2018.zip","CE Effort 2009-2018.zip"), dir_download_browser = "ADD_HERE_download_folder_adress", dir_download_target = getwd(), unzip=TRUE)


# ===========================
# TIP TIP TIP: to prepare individual stocks you can jump all way to the end	
# ===========================	

# ========================
# reads in data
# ========================
 
	# wishlist - ask HKN to remove "last line" and standardize names of files
 
 rm(list=ls())
 library(data.table)
 gc()
  
 # read file names
 
	# file_tr <- "data/001_original/2020/20200417/TR Trip.zip"
	# file_hh <- "data/001_original/2020/20200417/HH Station.zip" 
	# file_sl <- "data/001_original/2020/20200417/SL SpeciesList.zip" 
	# file_hl <- "data/001_original/2020/20200417/HL Length.zip" 
	# file_ca <- "data/001_original/2020/20200417/CA SMAWL.zip" 

	# unzip(file_tr,exdir="data/001_original/2020/20200417")
	# unzip(file_hh,exdir="data/001_original/2020/20200417")
	# unzip(file_sl,exdir="data/001_original/2020/20200417")
	# unzip(file_hl,exdir="data/001_original/2020/20200417")
	# unzip(file_ca,exdir="data/001_original/2020/20200417")
 
 	file_tr <- "data/001_original/2020/20200417/TR Trip.csv"
	file_hh <- "data/001_original/2020/20200417/HH Station.csv" 
	file_sl <- "data/001_original/2020/20200417/SL SpeciesList.csv" 
	file_hl <- "data/001_original/2020/20200417/HL Length.csv" 
	file_ca <- "data/001_original/2020/20200417/CA SMAWL.csv" 
 
# read CS data
 tr<-fread(file_tr, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=",", na.strings="NULL", colClasses=c(Trip  = "character"))
 #tr<-fread(file_tr, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=";", na.strings="NULL", colClasses=c(Trip  = "character"), nrows=130242)
 hh<-fread(file_hh, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=",", na.strings="NULL", colClasses=c(Trip  = "character"))
 #hh<-fread(file_hh, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=";", na.strings="NULL", colClasses=c(Trip  = "character"), nrows=264268)
 sl<-fread(file_sl, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=",", na.strings="NULL", colClasses=c(Trip  = "character", SizeCategory = "character",CatchCategory="character"))
 #sl<-fread(file_sl, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=";", na.strings="NULL", colClasses=c(Trip  = "character"), nrows=1567407)
 hl<-fread(file_hl, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=",", na.strings="NULL", colClasses=c(Trip  = "character", SizeCategory = "character",CatchCategory="character"))
 #hl<-fread(file_hl, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=";", na.strings="NULL", colClasses=c(Trip  = "character", SizeCategory = "character"), nrows=9292149)
 ca<-fread(file_ca, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=",", na.strings="NULL", colClasses=c(Trip  = "character", SizeCategory = "character", OtolithSide = "character"))
 #ca<-fread(file_ca, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=";", na.strings="NULL", colClasses=c(Trip  = "character", SizeCategory = "character", OtolithSide = "character"), nrows=5416104)
 

	# QCA: duplicates
	dim(tr); tr<-unique(tr); dim(tr)
	dim(hh); hh<-unique(hh); dim(hh)
	dim(sl); sl<-unique(sl); dim(sl)
	dim(hl); hl<-unique(hl); dim(hl)
	dim(ca); ca<-unique(ca); dim(ca)
 
# ====================== 
# create directory structure
# ====================== 

dir.create(paste("data/002_prepared/2020/RCG_NA", sep=""),recursive=TRUE, showWarnings=FALSE)
dir.create(paste("data/002_prepared/2020/RCG_BA", sep=""),recursive=TRUE, showWarnings=FALSE)
dir.create(paste("data/002_prepared/2020/RCG_NSEA", sep=""),recursive=TRUE, showWarnings=FALSE) 
 
 
# ========================
# check and correct on reverse dependencies (should be moved to extraction)
# ======================== 		
	
	# check on hh [ones showing could be ca only]
	hh[!CS_TripId %in% tr$CS_TripId,.N,by=c("FlagCountry","Year")] 
	 
	# check on sl
		# 297 EST records from 2011 and 2012
			sl[!CS_StationId %in% hh$CS_StationId,.N,by=c("FlagCountry","Year")]
			# decision: delete
				sl<-sl[CS_StationId %in% hh$CS_StationId] 

	
	# check on hl
		# 5185 EST records from 2011 and 2012
			hl[!CS_SpeciesListId %in% sl$CS_SpeciesListId,.N,by=c("FlagCountry","Year")]
			# decision: delete
				hl<-hl[CS_SpeciesListId %in% sl$CS_SpeciesListId] 
	
	# check on ca
	ca[!CS_TripId %in% tr$CS_TripId,.N,by=c("FlagCountry","Year")] 



# ======================== 		
# adds a few convenient Ids (should be moved to extraction)
# ======================== 	
	
	#dim(sl); sl<-merge(sl, hh[,list(CS_StationId,CS_TripId)], by="CS_StationId", all.x=T); dim(sl)
	#dim(hl); hl<-merge(hl, sl[,list(CS_SpeciesListId,CS_StationId,CS_TripId)], by="CS_SpeciesListId", all.x=T); dim(hl)
	
# ======================== 		
# adds an additional convenient Id to ca
# ======================== 		
	
	# adds a column with probable hauls to ca [useful to identify positions of bio samples]
		# auxID is made with mandatory fields [note: length is not used - can be attempted but probably too much resolution and prone to error when samples brought back to lab]
		# pass 1
			# for SamplingType == S auxID includes StatisticalRectangle
			# for SamplingType != S StatisticalRectangle is not included
		# pass 2 (only in remaining NAs for SamplingType == S)
			# auxID a bit more crude, excluding StatisticalRectangle
			# not much improvement
			
		# note: might be worth testing inclusion of SamplingType in auxID, i.e., forcing non-S records of ca to fully correspond in terms of SamplingType
	
	aux<-sl
	dim(aux); aux<-merge(aux, hh[,list(CS_StationId,Area,StatisticalRectangle)], by="CS_StationId", all.x=T); dim(aux)
	
	# pass 1 [both SamplingType with the ID]
	
	# builds ID with mandatory variables
		dim(aux); aux[, auxID:=paste(CS_TripId, Area, SpeciesAphiaID, CatchCategory, LandingCategory, StatisticalRectangle),]; dim(aux)
		aux[!SamplingType=="S", auxID:=paste(CS_TripId, Area, SpeciesAphiaID, CatchCategory, LandingCategory),] 
		dim(ca); ca[, auxID:=paste(CS_TripId, Area, SpeciesAphiaID, CatchCategory, LandingCategory, StatisticalRectangle),]; dim(ca)
		ca[!SamplingType=="S", auxID:=paste(CS_TripId, Area, SpeciesAphiaID, CatchCategory, LandingCategory),]
	
		aux1<-unique(aux[,list(CS_StationId,auxID, CS_TripId)])
		aux2<-aux1[,list(CS_StationId_Probable=paste(CS_StationId, collapse=",")), by=list(auxID)]	

		# assignment	
		dim(ca); ca<-merge(ca, aux2, by="auxID", all.x=T); dim(ca)	
	
			# some results on assignment:
				# % allocated
				 sum(!is.na(ca$CS_StationId_Probable))/nrow(ca)*100
				# % allocated to single haul
				 sum(!is.na(ca$CS_StationId_Probable) & !grepl(ca$CS_StationId_Probable, pat=","))/nrow(ca)*100
			 
	# pass 2 [only SamplingType=="S"]
		dim(aux); aux[SamplingType=="S", auxID:=paste(CS_TripId, Area, SpeciesAphiaID, CatchCategory, LandingCategory),]; dim(aux)
		dim(ca); ca[SamplingType=="S", auxID:=paste(CS_TripId, Area, SpeciesAphiaID, CatchCategory, LandingCategory),]; dim(ca)
		
		aux1<-unique(aux[,list(SamplingType,CS_StationId,auxID, CS_TripId)])
		aux2<-aux1[,list(CS_StationId_Probable=paste(CS_StationId, collapse=",")), by=list(SamplingType,auxID)]	
		
		# assignment
		ca[SamplingType=="S" & is.na(CS_StationId_Probable),]$CS_StationId_Probable <- aux2[SamplingType=="S",]$CS_StationId_Probable[match(ca[SamplingType=="S" & is.na(CS_StationId_Probable),"auxID"]$auxID,aux2[SamplingType=="S","auxID"]$auxID)]
		
			# some results on assignment:
				# % allocated
				 sum(!is.na(ca$CS_StationId_Probable))/nrow(ca)*100
				# % allocated to single haul
				 sum(!is.na(ca$CS_StationId_Probable) & !grepl(ca$CS_StationId_Probable, pat=","))/nrow(ca)*100
	
	# deletes auxID
		ca[,auxID:=NULL]

	
# ====================== 
# Set Prep Options 
# ======================  
 
target_region <- "RCG_NA" # "RCG_BA", "RCG_NSEA", "RCG_NA"
year_start <- 2009
year_end <- 2019
dir_output_rcg<-paste("data/002_prepared/2020/",target_region,sep="")
dir_output_all<-"data/002_prepared/2020/"


# ======================
# Tweak on areas/region 
# ====================== 
 

# CS
	# visual check
	table(hh$Area, hh$Region, useNA="al")
		# 20190413
			# RCG_BA: ok
			# RCG_NSEA: ok
			# RCG_NA: ok
	table(ca$Area, ca$Region, useNA="al")
		# 20190413
			# RCG_BA: ok
			# RCG_NSEA: ok
			# RCG_NA: ok


# ========================
# subsets data and RCG specific preparations
# ========================	


# RCM Baltic: Baltic Sea (ICES areas III b-d)
 if(target_region=="RCG_BA") 
		{
		print(paste(".subsetting",target_region))
		
		target_areas <- c('27.3.b.23','27.3.c.22','27.3.d.24','27.3.d.25','27.3.d.26','27.3.d.27','27.3.d.28','27.3.d.28.1','27.3.d.28.2','27.3.d.29','27.3.d.30','27.3.d.31','27.3.d.32')	

		# cl_rcg <- cl[Area %in% target_areas & Year>=year_start & Year<=year_end,]
			# # QCA: should yield 0
			# table(cl[Region=="BS" &  !Area %in% target_areas,"Area"])
			# table(cl[!Region=="BS" &  Area %in% target_areas,"Area"])
			# # QCA: should yield BS 
			# #table(cl_rcg$Region)
				# # corrects
				# #cl_rcg[!Region=="BS",FishingGround:=NA,]	
				# #cl_rcg[!Region=="BS",Region:="BS",]		
		
		# ce_rcg <- ce[Area %in% target_areas & Year>=year_start & Year<=year_end,]
			# # QCA: should yield 0
				# # ATT: a few records 27.3 and 27.7 in BA?! 
				# table(ce[Region=="BS" &  !Area %in% target_areas,"Area"])	 
				# table(ce[!Region=="BS" &  Area %in% target_areas,"Area"])	 
			# # QCA: should yield BS 
				# #table(ce_rcg$Region)
				# # corrects
					# #ce_rcg[!Region=="BS",FishingGround:=NA,]				
					# #ce_rcg[!Region=="BS",Region:="BS",]	
		
		target_trips <- hh[Area %in% target_areas & Year>=year_start & Year<=year_end,unique(CS_TripId),]
		tr_rcg_all <- tr[CS_TripId %in% target_trips,]
		hh_rcg_all <- hh[CS_TripId %in% target_trips,]
		sl_rcg_all <- sl[CS_TripId %in% target_trips,]
		hl_rcg_all <- hl[CS_TripId %in% target_trips,]
		ca_rcg_all <- ca[CS_TripId %in% target_trips,]
		
		}

# RCM NS&EA: the  North  Sea  (ICES  areas  IIIa,  IV  and  VIId),  the  Eastern  Arctic  (ICES  areas  I  and  II),  the  ICES  divisions Va, XII & XIV and the NAFO areas.
 if(target_region=="RCG_NSEA") 
		{
		print(paste(".subsetting",target_region))
		
		target_areas_nsea <- c('27.1','27.2','27.2.a','27.2.a.1','27.2.a.2','27.2.b','27.2.b.2','27.3.a','27.3.a.20','27.3.a.21','27.4','27.4.a','27.4.b','27.4.c','27.5.a','27.7.d','27.12','27.14','27.14.a','27.14.b','27.14.b.1','27.14.b.2')
		
		# cl_rcg <-cl[ (Area %in% target_areas_nsea | grepl(Area, pat="21.") ) & Year>=year_start & Year<=year_end,]
		
			# # QCA: should yield 0
				# # ATT: a few records 41, 51 and 57 in NSEA [these have not been included in cl_rcg] 
				# table(cl[Region=="NSEA" & !(Area %in% target_areas_nsea | grepl(Area, pat="21.") ),"Area"])
				# table(cl[!Region=="NSEA" & (Area %in% target_areas_nsea | grepl(Area, pat="21.") ),"Area"])
			# # QCA: should yield NSEA
				# # ATT: a few records (21 - NAFO) not NSEA?!			
				# table(cl_rcg$Region)
				# table(cl_rcg[!Region=="NSEA",Area])
					# # corrects				
					# cl_rcg[!Region=="NSEA" & (Area %in% target_areas_nsea | grepl(Area, pat="21.") ),Region:="NSEA",]
					# table(cl_rcg$Region)
			
		# ce_rcg <- ce[ (Area %in% target_areas_nsea | grepl(Area, pat="21.") ) & Year>=year_start & Year<=year_end,]
			# # QCA: should yield 0
				# # ATT: a few records 41, 51 and 57 in NSEA [these have not been included in ce_rcg] 
				# table(ce[Region=="NSEA" & !(Area %in% target_areas_nsea | grepl(Area, pat="21.") ),"Area"])	
				# table(ce[!Region=="NSEA" & (Area %in% target_areas_nsea | grepl(Area, pat="21.") ),"Area"])	
			# # QCA: should yield NSEA
				# # ATT: a few records (21 - NAFO) not NSEA?!			
				# table(ce_rcg$Region)
				# table(ce_rcg[!Region=="NSEA",Area])		
					# # corrects	
					# ce_rcg[!Region=="NSEA" & grepl(Area, pat="21."),Region:="NSEA",]
					# table(ce_rcg$Region)
	
		
		
		target_trips <- hh[(Area %in% target_areas_nsea | grepl(Area, pat="21.") ) & Year>=year_start & Year<=year_end,unique(CS_TripId),]
		tr_rcg_all <- tr[CS_TripId %in% target_trips,]
		hh_rcg_all <- hh[CS_TripId %in% target_trips,]
		sl_rcg_all <- sl[CS_TripId %in% target_trips,]
		hl_rcg_all <- hl[CS_TripId %in% target_trips,]
		ca_rcg_all <- ca[CS_TripId %in% target_trips,]
					# corrects	
					hh_rcg_all[!Region=="NSEA" & grepl(Area, pat="21."),Region:="NSEA",]
					ca_rcg_all[!Region=="NSEA" & grepl(Area, pat="21."),Region:="NSEA",]
	
		}	
		
# RCM NA: the North Atlantic (ICES areas V-X, excluding Va and VIId)
 if(target_region=="RCG_NA") 
		{
		
		print(paste(".subsetting",target_region))
					
		# cs	
		target_trips<-hh[(grepl(Area, pat="27.5") | 
							grepl (Area, pat="27.6") | 
								grepl (Area, pat="27.7") | 
									grepl (Area, pat="27.8") | 
										grepl (Area, pat="27.9")	 | 
											grepl (Area, pat="27.10") ) &  
												!grepl (Area, pat="27.5.a") &  !grepl (Area, pat="27.7.d") & Year>=year_start & Year<=year_end,unique(CS_TripId)]
		
		tr_rcg_all <- tr[CS_TripId %in% target_trips,]
		hh_rcg_all <- hh[CS_TripId %in% target_trips,]
		sl_rcg_all <- sl[CS_TripId %in% target_trips,]
		hl_rcg_all <- hl[CS_TripId %in% target_trips,]
		
		target_trips<-ca[(grepl(Area, pat="27.5") | 
							grepl (Area, pat="27.6") | 
								grepl (Area, pat="27.7") | 
									grepl (Area, pat="27.8") | 
										grepl (Area, pat="27.9")	 | 
											grepl (Area, pat="27.10") ) &  
												!grepl (Area, pat="27.5.a") &  !grepl (Area, pat="27.7.d") & Year>=year_start & Year<=year_end,unique(CS_TripId)]		
		
		ca_rcg_all <- ca[CS_TripId %in% target_trips,]		
				
	}	

# ========================
# formats variables
# ======================== 
 

	# formats CS	
	tr[,HarbourDesc:=iconv(HarbourDesc, from="UTF-8", to="")]
	tr_rcg_all[,HarbourDesc:=iconv(HarbourDesc, from="UTF-8", to="")]

		
# ========================	
# Creates additional variables
# ========================	

	
	# CS
		# tr
		tr[!is.na(VesselLength) & VesselLength<10,VesselLengthCategory:="<10"]
		tr[!is.na(VesselLength) & VesselLength>=10 & VesselLength<12,VesselLengthCategory:="10-<12"]
		tr[!is.na(VesselLength) & VesselLength>=12 & VesselLength<18,VesselLengthCategory:="12-<18"]
		tr[!is.na(VesselLength) & VesselLength>=18 & VesselLength<24,VesselLengthCategory:="18-<24"]
		tr[!is.na(VesselLength) & VesselLength>=24 & VesselLength<40,VesselLengthCategory:="24-<40"]
		tr[!is.na(VesselLength) & VesselLength>=40 ,VesselLengthCategory:=">40"]
		tr_rcg_all[!is.na(VesselLength) & VesselLength<10,VesselLengthCategory:="<10"]
		tr_rcg_all[!is.na(VesselLength) & VesselLength>=10 & VesselLength<12,VesselLengthCategory:="10-<12"]
		tr_rcg_all[!is.na(VesselLength) & VesselLength>=12 & VesselLength<18,VesselLengthCategory:="12-<18"]
		tr_rcg_all[!is.na(VesselLength) & VesselLength>=18 & VesselLength<24,VesselLengthCategory:="18-<24"]
		tr_rcg_all[!is.na(VesselLength) & VesselLength>=24 & VesselLength<40,VesselLengthCategory:="24-<40"]
		tr_rcg_all[!is.na(VesselLength) & VesselLength>=40 ,VesselLengthCategory:=">40"]
	
		# sl
		sl[, Weight_kg := Weight/1000]
		sl[, Weight_ton := Weight/1000000]
		sl[, SubSampleWeight_kg := SubSampleWeight/1000]
		sl[, SubSampleWeight_ton := SubSampleWeight/1000000]
		sl_rcg_all[, Weight_kg := Weight/1000]
		sl_rcg_all[, Weight_ton := Weight/1000000]
		sl_rcg_all[, SubSampleWeight_kg := SubSampleWeight/1000]
		sl_rcg_all[, SubSampleWeight_ton := SubSampleWeight/1000000]

		# hl
		hl[, LengthClass_cm := LengthClass/10]
		hl[, NoAtLengthInSample_ThousandIndiv := NoAtLengthInSample/1000]
		hl[, NoAtLengthInSample_MillionIndiv := NoAtLengthInSample/1000000]
		hl_rcg_all[, LengthClass_cm := LengthClass/10]
		hl_rcg_all[, NoAtLengthInSample_ThousandIndiv := NoAtLengthInSample/1000]
		hl_rcg_all[, NoAtLengthInSample_MillionIndiv := NoAtLengthInSample/1000000]
		
		# ca
		ca[,Weight_kg := Weight/1000]
		ca[,LengthClass_cm := LengthClass/10]
		ca_rcg_all[,Weight_kg := Weight/1000]
		ca_rcg_all[,LengthClass_cm := LengthClass/10]


	# AreaMap
		hh_rcg_all[,AreaMap:=Area,]
		ca_rcg_all[,AreaMap:=Area,]
		
		if(target_region=="RCG_BA") 
			{		
			hh_rcg_all[AreaMap %in% c("27.3.d.28.1", "27.3.d.28.2"), AreaMap := "27.3.d.28"]
			ca_rcg_all[AreaMap %in% c("27.3.d.28.1", "27.3.d.28.2"), AreaMap := "27.3.d.28"]
			}
		if(target_region=="RCG_NSEA") 
			{		
			hh_rcg_all[AreaMap %in% c("21.1"), AreaMap := "NA"] # div required (minority of records)					
			ca_rcg_all[AreaMap %in% c("21.1"), AreaMap := "NA"] # div required (minority of records)		
			
			hh_rcg_all[AreaMap %in% c("21.3"), AreaMap := "NA"] # div required (minority of records)				
			ca_rcg_all[AreaMap %in% c("21.3"), AreaMap := "NA"] # div required (minority of records)		
			
			hh_rcg_all[AreaMap %in% c("27.2"), AreaMap := "NA"]	 # div required	(minority of records)				
			ca_rcg_all[AreaMap %in% c("27.2"), AreaMap := "NA"]	 # div required	(minority of records)	
			
			hh_rcg_all[AreaMap %in% c("27.3.a"), AreaMap := "NA"] # subdiv required	(minority of records)		
			ca_rcg_all[AreaMap %in% c("27.3.a"), AreaMap := "NA"] # subdiv required	(minority of records)

			hh_rcg_all[AreaMap %in% c("27.4"), AreaMap := "NA"]	 # div required	(minority of records)	
			ca_rcg_all[AreaMap %in% c("27.4"), AreaMap := "NA"]	 # div required	(minority of records)	
			
			hh_rcg_all[AreaMap %in% c("27.14"), AreaMap := "NA"] # div required	(some records)				
			ca_rcg_all[AreaMap %in% c("27.14"), AreaMap := "NA"] # div required	(some records)		
			
			hh_rcg_all[AreaMap %in% c("27.2.a.1", "27.2.a.2"), AreaMap := "27.2.a"]
			ca_rcg_all[AreaMap %in% c("27.2.a.1", "27.2.a.2"), AreaMap := "27.2.a"]
			
			hh_rcg_all[AreaMap %in% c("27.2.b.2"), AreaMap := "27.2.b"]			
			ca_rcg_all[AreaMap %in% c("27.2.b.2"), AreaMap := "27.2.b"]			
			
			hh_rcg_all[AreaMap %in% c("27.14.b.1", "27.14.b.2"), AreaMap := "27.14.b"]
			ca_rcg_all[AreaMap %in% c("27.14.b.1", "27.14.b.2"), AreaMap := "27.14.b"]
			}
		
		if(target_region=="RCG_NA") 
			{		
			hh_rcg_all[AreaMap %in% c("27.5.b.1","27.5.b.2"), AreaMap := "27.5.b"]
			ca_rcg_all[AreaMap %in% c("27.5.b.1","27.5.b.2"), AreaMap := "27.5.b"]
			
			hh_rcg_all[AreaMap %in% c("27.9.b.1", "27.9.b.2"), AreaMap := "27.9.b"]
			ca_rcg_all[AreaMap %in% c("27.9.b.1", "27.9.b.2"), AreaMap := "27.9.b"]						
			
			hh_rcg_all[AreaMap %in% c('27.6.a.n','27.6.a.s'), AreaMap := "27.6.a"]
			ca_rcg_all[AreaMap %in% c('27.6.a.n','27.6.a.s'), AreaMap := "27.6.a"]

			hh_rcg_all[AreaMap %in% c("27.10"), AreaMap := "NA"]	# div required	(minority of records)			
			ca_rcg_all[AreaMap %in% c("27.10"), AreaMap := "NA"]	# div required	(minority of records)	

			hh_rcg_all[AreaMap %in% c("27.6"), AreaMap := "NA"]		# div required	(minority of records)			
			ca_rcg_all[AreaMap %in% c("27.6"), AreaMap := "NA"]		# div required	(minority of records)				
			
			hh_rcg_all[AreaMap %in% c("27.7"), AreaMap := "NA"]		# div required	(minority of records)		
			ca_rcg_all[AreaMap %in% c("27.7"), AreaMap := "NA"]		# div required	(minority of records)	
			
			}			
	# QCA: visual
		hh_rcg_all[, list(N=.N),list(AreaMap,Area)][order(AreaMap)]
		hh_rcg_all[, list(N=.N),list(AreaMap,Area, FlagCountry, Year)][order(AreaMap)][AreaMap=="NA",]
		ca_rcg_all[, list(N=.N),list(AreaMap,Area)][order(AreaMap)]
		ca_rcg_all[, list(N=.N),list(AreaMap,Area, FlagCountry, Year)][order(AreaMap)][AreaMap=="NA",]
	

# ========================		
# adjusts 99u9 rectangles to NA
# ========================			
		
		hh_rcg_all[StatisticalRectangle=="99u9", StatisticalRectangle := NA]
		ca_rcg_all[StatisticalRectangle=="99u9", StatisticalRectangle := NA]

	
# ========================	
# Creates and tweaks ISSCAAP codes
# ========================		

	# wishlist: add an update to valid Aphia and Aphia SciNames	

	
	#require(fishPiCodes) # Ask Alastair Pout (MSS) if you don't have package
	#data(ASFIS_WoRMS)

	# 2019-03-27: email sent to fishpi2 wp2+3 participants on permissions to use the package
	# 2019-04-02: so far no answer - to allow review of code an updated version of fishPiCodes::ASFIS_WoRMS table (ASFIS_WoRMS_updt.csv) will be kept on the data sharepoint of the RCG subgroup (acessible only to Subgroup members)
	
	ASFIS_WoRMS_updt <- read.table (file="ASFIS_WoRMS_updt.csv", header=T, sep=";", stringsAsFactors=FALSE) # on the data sharepoint of the RCG subgroup


		# =====================
		# SL [note: at the moment this is only being run on sl_rcg_all, not on the larger sl]
		# =====================	
	
			sl_rcg_all[,ISSCAAP:=ASFIS_WoRMS_updt$ISSCAAP[match(sl_rcg_all$SpeciesAphiaID, ASFIS_WoRMS_updt$AphiaID_accepted)]]
	
			# QCA should yield zero, if not more tweaks are needed
			sum(is.na(sl_rcg_all$ISSCAAP))

			sort(unique(sl_rcg_all$Species[is.na(sl_rcg_all$ISSCAAP)]))

			sl_rcg_all[Species == "Gaidropsarus guttatus",ISSCAAP:=32] # Cods, hakes, haddocks
			sl_rcg_all[Species == "Mullus barbatus",ISSCAAP:=33] # Miscellaneous coastal fishes
			sl_rcg_all[Species == "Auxis rochei",ISSCAAP:=36] # Tunas, bonitos, billfishes
			sl_rcg_all[Species == "Auxis thazard",ISSCAAP:=36] # Tunas, bonitos, billfishes
			sl_rcg_all[Species == "Scombrinae Rafinesque",ISSCAAP:=36] # Tunas, bonitos, billfishes
			sl_rcg_all[Species == "Melanostomiinae",ISSCAAP:=37] # Miscellaneous pelagic fishes
			sl_rcg_all[Species == "Scomberesox saurus",ISSCAAP:=37] # Miscellaneous pelagic fishes
			sl_rcg_all[Species == "Selachii",ISSCAAP:=38] # Sharks, rays, chimaeras
			sl_rcg_all[Species == "Squatina",ISSCAAP:=38] # Sharks, rays, chimaeras
			sl_rcg_all[Species == "Pisces",ISSCAAP:=39] # Marine fishes not identified
			sl_rcg_all[Species == "Macropodia",ISSCAAP:=42] # Crabs, sea-spiders
			sl_rcg_all[Species == "Pasiphaea",ISSCAAP:=45] # Shrimps, prawns
			sl_rcg_all[Species == "Dendrobranchiata",ISSCAAP:=45] # Shrimps, prawns
			sl_rcg_all[Species == "Crangon",ISSCAAP:=45] # Shrimps, prawns
			sl_rcg_all[Species == "Decapodiformes",ISSCAAP:=47] # Miscellaneous marine crustaceans
			sl_rcg_all[Species == "Gibbula",ISSCAAP:=52] # Abalones, winkles, conchs
			sl_rcg_all[Species == "Venerupis philippinarum",ISSCAAP:=56] # Clams, cockles, arkshells
			sl_rcg_all[Species == "Arcopagia crassa",ISSCAAP:=56] # Clams, cockles, arkshells
			sl_rcg_all[Species == "Loligo forbesii",ISSCAAP:=57] # Squids, cuttlefishes, octopuses
			sl_rcg_all[Species == "Nudibranchia",ISSCAAP:=58] #Miscellaneous marine molluscs 
			sl_rcg_all[Species == "Salpa",ISSCAAP:=74] # Sea-squirts and other tunicates 
			sl_rcg_all[Species == "Echinidae",ISSCAAP:=76] # Sea-urchins and other echinoderms
			sl_rcg_all[Species == "Astrospartus mediterraneus",ISSCAAP:=76] # Sea-urchins and other echinoderms
			sl_rcg_all[Species == "Laminaria",ISSCAAP:=91] # Brown seaweeds
			
			sl_rcg_all[grepl(Species, pat = "Callionymus"),ISSCAAP:=33] # Miscellaneous coastal fishes
			sl_rcg_all[grepl(Species, pat = "Pomatoschistus"),ISSCAAP:=33] # Miscellaneous coastal fishes
			sl_rcg_all[grepl(Species, pat = "Dipturus"),ISSCAAP:=38] # Sharks, rays, chimaeras
			sl_rcg_all[grepl(Species, pat = "Bathynectes"),ISSCAAP:=42]
			sl_rcg_all[grepl(Species, pat = "Liocarcinus"),ISSCAAP:=42] # Crabs, sea-spiders
			sl_rcg_all[grepl(Species, pat = "Inachidae"),ISSCAAP:=42] # Crabs, sea-spiders
			sl_rcg_all[grepl(Species, pat = "Munida"),ISSCAAP:=44]
			sl_rcg_all[grepl(Species, pat = "Sergestes"),ISSCAAP:=45] # Shrimps, prawns
			sl_rcg_all[grepl(Species, pat = "Ophiura"),ISSCAAP:=76] # Sea-urchins and other echinoderms
			sl_rcg_all[grepl(Species, pat = "Pagurus"),ISSCAAP:= 52] # Abalones, winkles, conchs
			sl_rcg_all[grepl(Species, pat = "Astropecten"),ISSCAAP:= 76]  # Sea-urchins and other echinoderms
			sl_rcg_all[grepl(Species, pat = "Inachus"),ISSCAAP:=42]  # Crabs, sea-spiders
			
		
				# fast and dirty correspondence via a reference table of genus and ISSCAAP
					aux<-as.data.table(unique(ASFIS_WoRMS_updt[!is.na(ASFIS_WoRMS_updt$Genus) & !is.na(ASFIS_WoRMS_updt$ISSCAAP), c("Genus","ISSCAAP")]))
					aux1<-aux[,.N,"Genus"][N==1,]
					ref_table<-as.data.frame(aux[Genus %in% aux1$Genus,])
					
					a<-sort(unique(sl_rcg_all$Species[is.na(sl_rcg_all$ISSCAAP)]))
					vector_genus<-do.call("rbind", strsplit(a, " "))[,1]

					ref_table<-merge(data.frame(Species=a, Genus=vector_genus), ref_table, by="Genus", all.x=T)
			
					sl_rcg_all[is.na(ISSCAAP),]$ISSCAAP<-ref_table$ISSCAAP[match(sl_rcg_all[is.na(ISSCAAP),]$Species,ref_table$Species)]
			
				# and the few remaining (manually...)
					sl_rcg_all[Species == "Indostomus",ISSCAAP:= 13] # Miscellaneous freshwater fishes
					sl_rcg_all[Species == "Gasterosteidae",ISSCAAP:= 25] 
					sl_rcg_all[Species == "Dagetichthys lusitanicus",ISSCAAP:=31]  # Flounders, halibuts, soles
					sl_rcg_all[Species %in% c("Scorpaeniformes","Leptoclinus maculatus"), ISSCAAP:=34] # Miscellaneous demersal fishes
					sl_rcg_all[Species %in% c("Arctozenus"), ISSCAAP:=39] # Miscellaneous marine fishes
					sl_rcg_all[Species %in% c("Atelecyclus undecimdentatus","Majidae","Goneplax rhomboides","Macropodia longipes","Macropodia tenuirostris","Monodaeus couchii", "Corystes cassivelaunus","Macropodia rostrata"),ISSCAAP:=42] # Crabs, sea-spiders
					sl_rcg_all[Species == "Caridea",ISSCAAP:=47] # Miscellaneous marine crustaceans
					sl_rcg_all[Species %in% c("Dardanus arrosor","Paguristes eremita","Galathea squamifera","Galathea strigosa"),ISSCAAP:=44] # King crabs, squat-lobsters
					sl_rcg_all[Species %in% c("Sergia robusta","Atlantopandalus propinqvus","Philocheras trispinosus"),ISSCAAP:= 45] # Shrimps, prawns
					sl_rcg_all[Species %in% c("Euphausiidae"),ISSCAAP:= 46]
					sl_rcg_all[Species %in% c("Ampulla priamus","Neptunea contraria","Neptunea antiqua","Buccinidae","Ranella olearium","Scaphander lignarius"),ISSCAAP:=52] # Abalones, winkles, conchs
					sl_rcg_all[Species %in% c("Cidaris cidaris","Ophiothrix fragilis","Marthasterias glacialis","Ophiuridae","Asteriidae","Echinocardium cordatum"),ISSCAAP:=76] # Sea-urchins and other echinoderms
					sl_rcg_all[Species %in% c("Sepiida","Sepiolidae"),ISSCAAP:=57]  # Squids, cuttlefishes, octopuses
					sl_rcg_all[Species %in% c("Tealia","Adamsia palliata","Holothuriidae","Nereididae"),ISSCAAP:=77] #Miscellaneous aquatic invertebrates
					sl_rcg_all[Species == "Plantae",ISSCAAP:=94] # Miscellaneous aquatic plants
					sl_rcg_all[Species == "Melanitta nigra",ISSCAAP:=100] # NOT EXISTING
		
			# QCA should be zero
			sum(is.na(sl_rcg_all$ISSCAAP))

				# code for debugging:
					# unique(sl_rcg_all[is.na(sl_rcg_all$ISSCAAP),"Species"]$Species)
			
			
		# Ammodytes and Norway Pout appear classified as "33" - better assign to "37"
			sl_rcg_all[grepl(Species, pat= "Ammodytes"),ISSCAAP:=37] # Miscellaneous pelagic fishes
			sl_rcg_all[grepl(Species, pat= "Ammodytidae"),ISSCAAP:=37]  # Miscellaneous pelagic fishes
			sl_rcg_all[grepl(Species, pat= "Trisopterus esmarkii"),ISSCAAP:=37] # Miscellaneous pelagic fishes
			
		# Actinopterygii 39
			sl_rcg_all[grepl(Species, pat= "Actinopterygii"),ISSCAAP:=39]
		

		# Adds RCG Catch_group
			aux_spp_categ<-read.table("001_Inputs_Species_Categ/Table_Species_Categ.txt", header=T, sep="\t")
			sl_rcg_all[,Catch_group:=aux_spp_categ$RCM_NSEA_categ[match(sl_rcg_all$ISSCAAP,aux_spp_categ$ISSCAAP)]]
			
		# QCA should be zero			
			sum(is.na(sl_rcg_all$Catch_group))
		
			
			# some additional tweaks
			sl_rcg_all[grepl(Species, pat="Trachurus"),Species:="Trachurus spp."]
			sl_rcg_all[grepl(Species, pat="Lepidorhombus"),Species:="Lepidorhombus spp."]
			sl_rcg_all[grepl(Species, pat="Lophi"),Species:="Lophiidae"]
			
			sl_rcg_all[Species == "Myxine glutinosa",Catch_group:="other"]
			sl_rcg_all[Species == "Lichia amia",Catch_group:="large pelagic"]
			sl_rcg_all[Species == "Acipenser sturio",Catch_group:="diadromous"]
			sl_rcg_all[Species == "Melanitta nigra",Catch_group:="other"]
				
			# give it a check (see if it makes sense)
			 # check demersal
				head(sl_rcg_all[Catch_group == "demersal",list(Kg=sum(Weight_kg)),list(Species)] [order(-Kg),],20)
			# check flatfish
				head(sl_rcg_all[Catch_group == "flatfish",list(Kg=sum(Weight_kg)),list(Species)] [order(-Kg),],20)
			# check small pelagic
				head(sl_rcg_all[Catch_group == "small pelagic",list(Kg=sum(Weight_kg)),list(Species)] [order(-Kg),],20)
			# check large pelagic
				head(sl_rcg_all[Catch_group == "large pelagic",list(Kg=sum(Weight_kg)),list(Species)] [order(-Kg),],20)
			# check molluscs
				head(sl_rcg_all[Catch_group == "molluscs",list(Kg=sum(Weight_kg)),list(Species)] [order(-Kg),],20)
			# check crustaceans
				head(sl_rcg_all[Catch_group == "crustaceans",list(Kg=sum(Weight_kg)),list(Species)] [order(-Kg),],20)
			# check	elasmobranchs
				head(sl_rcg_all[Catch_group == "elasmobranchs",list(Kg=sum(Weight_kg)),list(Species)] [order(-Kg),],20)
			# check	diadromous
				head(sl_rcg_all[Catch_group == "diadromous",list(Kg=sum(Weight_kg)),list(Species)] [order(-Kg),],20)
			# check	incidental by-catch
				head(sl_rcg_all[Catch_group == "incidental by-catch",list(Kg=sum(Weight_kg)),list(Species)] [order(-Kg),],20)
			# check	other
				head(sl_rcg_all[Catch_group == "other",list(Kg=sum(Weight_kg)),list(Species)] [order(-Kg),],20)
			
		# adds ISCAAP and Catch_group to ca (based on sl and )
			ref_table<-unique(sl_rcg_all[,list(SpeciesAphiaID, ISSCAAP,Catch_group)])
			sum(duplicated(ref_table$SpeciesAphiaID))
			dim(ca_rcg_all); ca_rcg_all<-merge(ca_rcg_all, ref_table, by="SpeciesAphiaID", all.x=T); dim(ca_rcg_all)
			ca_rcg_all[is.na(ISSCAAP),ISSCAAP:=ASFIS_WoRMS_updt$ISSCAAP[match(SpeciesAphiaID, ASFIS_WoRMS_updt$AphiaID_accepted)]]
			ca_rcg_all[,Catch_group:=aux_spp_categ$RCM_NSEA_categ[match(ca_rcg_all$ISSCAAP,aux_spp_categ$ISSCAAP)]]

			# QCA should be zero			
				sum(is.na(ca_rcg_all$Catch_group))
				sum(is.na(ca_rcg_all$ISSCAAP))
			
			
# ========================
# factorization [establishes the order in unsorted bar graphs]
# ========================
	

	tr[,FlagCountry:=factor(FlagCountry, levels=sort(unique(FlagCountry))),]
	tr[,Harbour:=factor(Harbour, levels=sort(unique(Harbour))),]
	tr[,VesselLengthCategory:=factor(VesselLengthCategory, levels=c("<10","10-<12","12-<18","18-<24","24-<40",">40"))]
	
	tr_rcg_all[,FlagCountry:=factor(FlagCountry, levels=sort(unique(FlagCountry))),]
	tr_rcg_all[,Harbour:=factor(Harbour, levels=sort(unique(Harbour))),]
	tr_rcg_all[,VesselLengthCategory:=factor(VesselLengthCategory, levels=c("<10","10-<12","12-<18","18-<24","24-<40",">40"))]

	hh[,FishingActivityCategoryEuropeanLvl5:=factor(FishingActivityCategoryEuropeanLvl5, levels=sort(unique(FishingActivityCategoryEuropeanLvl5))),]
	hh[,FishingActivityCategoryEuropeanLvl6:=factor(FishingActivityCategoryEuropeanLvl6, levels=sort(unique(FishingActivityCategoryEuropeanLvl6))),]
	hh_rcg_all[,FishingActivityCategoryEuropeanLvl5:=factor(FishingActivityCategoryEuropeanLvl5, levels=sort(unique(FishingActivityCategoryEuropeanLvl5))),]
	hh_rcg_all[,FishingActivityCategoryEuropeanLvl6:=factor(FishingActivityCategoryEuropeanLvl6, levels=sort(unique(FishingActivityCategoryEuropeanLvl6))),]

#=========================
# creates CS objects to hauls and samples restricted in RCG region
#=========================

	tr_rcg<-tr_rcg_all
	
	if(target_region=="RCG_NA"){hh_rcg<-hh_rcg_all[Region=="NA",]}
	if(target_region=="RCG_BA"){hh_rcg<-hh_rcg_all[Region=="BA",]}
	if(target_region=="RCG_NSEA"){hh_rcg<-hh_rcg_all[Region=="NSEA",]}
	
	sl_rcg<-sl_rcg_all[CS_StationId %in% hh_rcg$CS_StationId,]
	hl_rcg<-hl_rcg_all[CS_SpeciesListId %in% sl_rcg$CS_SpeciesListId,]
	
	if(target_region=="RCG_NA"){ca_rcg<-ca_rcg_all[Region=="NA",]}
	if(target_region=="RCG_BA"){ca_rcg<-ca_rcg_all[Region=="BA",]}
	if(target_region=="RCG_NSEA"){ca_rcg<-ca_rcg_all[Region=="NSEA",]}

# =====================
# region specific factorization	
# =====================

	if(target_region=="RCG_BA"){
		
		# FishingGround		
		target_FishingGround<-c("22-24","25-32")
		print(hh_rcg[!FishingGround %in% c(NA,target_FishingGround),.N, "FlagCountry"])
		print(ca_rcg[!FishingGround %in% c(NA,target_FishingGround),.N, "FlagCountry"])	
		hh_rcg[,FishingGround:=factor(FishingGround, levels=target_FishingGround),]
		ca_rcg[,FishingGround:=factor(FishingGround, levels=target_FishingGround),]	

		target_Areas<-c('27.3.b.23','27.3.c.22','27.3.d.24','27.3.d.25','27.3.d.26','27.3.d.27','27.3.d.28','27.3.d.28.1','27.3.d.28.2','27.3.d.29','27.3.d.30','27.3.d.31','27.3.d.32')
		print(hh_rcg[!Area %in% target_Areas,.N, c("FlagCountry","Area")])
		print(ca_rcg[!Area %in% target_Areas,.N, c("FlagCountry","Area")])
		hh_rcg[,Area:=factor(Area, levels=target_Areas),]
		ca_rcg[,Area:=factor(Area, levels=target_Areas),]
				
		# AreaMap
		target_AreaMap<-c('27.3.b.23','27.3.c.22','27.3.d.24','27.3.d.25','27.3.d.26','27.3.d.27','27.3.d.28', '27.3.d.29','27.3.d.30','27.3.d.31','27.3.d.32')
		hh_rcg[,AreaMap:=factor(AreaMap, levels=target_AreaMap),]
		ca_rcg[,AreaMap:=factor(AreaMap, levels=target_AreaMap),]		

}

	if(target_region=="RCG_NSEA"){
		
		# FishingGround		
		target_FishingGround<-c("1+2", "3a", "4+7d", "5a+12+14", "21.0-6")
		print(hh_rcg[!FishingGround %in% c(NA,target_FishingGround),.N, "FlagCountry"])
		print(ca_rcg[!FishingGround %in% c(NA,target_FishingGround),.N, "FlagCountry"])	
		hh_rcg[,FishingGround:=factor(FishingGround, levels=target_FishingGround),]
		ca_rcg[,FishingGround:=factor(FishingGround, levels=target_FishingGround),]	
		
		# Areas
		target_Areas<-c('27.1','27.2','27.2.a','27.2.a.1','27.2.a.2','27.2.b','27.2.b.2','27.3.a','27.3.a.20','27.3.a.21','27.4','27.4.a','27.4.b','27.4.c','27.5.a','27.7.d','27.12','27.14','27.14.a','27.14.b','27.14.b.1','27.14.b.2','21.0.A','21.0.B','21.1','21.1.A','21.1.B','21.1.C','21.1.D','21.1.E','21.1.F','21.2.H','21.2.J','21.3','21.3.K','21.3.L','21.3.M','21.3.N','21.3.O','21.6.G')
		print(hh_rcg[!Area %in% target_Areas,.N, c("FlagCountry","Area")])
		print(ca_rcg[!Area %in% target_Areas,.N, c("FlagCountry","Area")])
		hh_rcg[,Area:=factor(Area, levels=target_Areas),]
		ca_rcg[,Area:=factor(Area, levels=target_Areas),]

		# AreaMap
		 target_AreaMap<-c('27.1', '27.2.a','27.2.b','27.3.a.20','27.3.a.21','27.4.a','27.4.b','27.4.c','27.5.a','27.7.d','27.12', '27.14.a','27.14.b', '21.0.A','21.0.B','21.1','21.1.A','21.1.B','21.1.C','21.1.D','21.1.E','21.1.F','21.2.H','21.2.J','21.3','21.3.K','21.3.L','21.3.M','21.3.N','21.3.O','21.6.G')
		 hh_rcg[,AreaMap:=factor(AreaMap, levels=target_AreaMap),]
		 ca_rcg[,AreaMap:=factor(AreaMap, levels=target_AreaMap),]		
	
	
	}
	if(target_region=="RCG_NA"){
		
		# FishingGround		
		target_FishingGround<-c("5b","6","7a","7bcjk","7e","7fgh","8abde","8c+9","10")
		print(hh_rcg[!FishingGround %in% c(NA,target_FishingGround),.N, "FlagCountry"])
		print(ca_rcg[!FishingGround %in% c(NA,target_FishingGround),.N, "FlagCountry"])
		hh_rcg[,FishingGround:=factor(FishingGround, levels=c("5b","6","7a","7bcjk","7e","7fgh","8abde","8c+9","10")),]
		ca_rcg[,FishingGround:=factor(FishingGround, levels=c("5b","6","7a","7bcjk","7e","7fgh","8abde","8c+9","10")),]

		target_Area<-c('27.5.b','27.5.b.1','27.5.b.2','27.6','27.6.a','27.6.a.n','27.6.a.s','27.6.b','27.7','27.7.a','27.7.b','27.7.c','27.7.e','27.7.f','27.7.g','27.7.h','27.7.j','27.7.k','27.8','27.8.a','27.8.b','27.8.c','27.8.d','27.8.e','27.9','27.9.a','27.9.b','27.9.b.1','27.9.b.2','27.10','27.10.a','27.10.b')
		print(hh_rcg[!Area %in% c(NA,target_Area),.N, "FlagCountry"])
		print(ca_rcg[!Area %in% c(NA,target_Area),.N, "FlagCountry"])
		hh_rcg[,Area:=factor(Area, levels=c('27.5.b','27.5.b.1','27.5.b.2','27.6','27.6.a','27.6.a.n','27.6.a.s','27.6.b','27.7','27.7.a','27.7.b','27.7.c','27.7.e','27.7.f','27.7.g','27.7.h','27.7.j','27.7.k','27.8.a','27.8.b','27.8.c','27.8.d','27.8.e','27.9.a','27.9.b','27.9.b.1','27.9.b.2','27.10','27.10.a','27.10.b')),]
		ca_rcg[,Area:=factor(Area, levels=c('27.5.b','27.5.b.1','27.5.b.2','27.6','27.6.a','27.6.a.n','27.6.a.s','27.6.b','27.7','27.7.a','27.7.b','27.7.c','27.7.e','27.7.f','27.7.g','27.7.h','27.7.j','27.7.k','27.8.a','27.8.b','27.8.c','27.8.d','27.8.e','27.9.a','27.9.b','27.9.b.1','27.9.b.2','27.10','27.10.a','27.10.b')),]
		
		# AreaMap
		target_AreaMap<-c('27.5.b', '27.6.a', '27.6.b', '27.7.a','27.7.b','27.7.c','27.7.e','27.7.f','27.7.g','27.7.h','27.7.j','27.7.k','27.8.a','27.8.b','27.8.c','27.8.d','27.8.e','27.9.a','27.9.b', '27.10.a','27.10.b','27.14.a','27.14.b')
		hh_rcg[,AreaMap:=factor(AreaMap, levels=target_AreaMap),]
		ca_rcg[,AreaMap:=factor(AreaMap, levels=target_AreaMap),]	
		
		}

	
# ========================
# saves data
# ========================	

	file_info_cs<-rbind(file.info(file_tr), file.info(file_hh), file.info(file_sl), file.info(file_hl), file.info(file_ca))

	time_tag<-format(Sys.time(), "%Y%m%d%H%M")
	#time_tag<-202004191807

	
	save(tr_rcg_all, hh_rcg_all, sl_rcg_all, hl_rcg_all ,ca_rcg_all, file_info_cs, file = paste(dir_output_rcg, paste("\\RDB",target_region,"CS_all", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
	save(tr_rcg, hh_rcg, sl_rcg, hl_rcg ,ca_rcg, file_info_cs, file = paste(dir_output_rcg, paste("\\RDB",target_region,"CS_rcg", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
	
	save(tr, hh, sl, hl, ca, file_info_cs, file = paste(dir_output_all, paste("\\RDB","All_Regions","CS", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
		
	
