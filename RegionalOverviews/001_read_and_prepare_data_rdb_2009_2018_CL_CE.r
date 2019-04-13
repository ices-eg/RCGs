# RCG subgroup work on Regional Fisheries and Sampling Overview
	# Nuno, Lucia, Sven, Marta, Gwladys, Hans, Henrik, Kirsten, Perttu, Alastair, Liz, Emilie, Joël
	# 2019
	
# script prepares datasets for further analysis

# note on CS subsets
	# two types of objects are produced: hh_rcg_all and hh_rcg
	# object rcg_all includes all hauls, etc of trips that registered >=1 haul in RCG areas [i.e., may be pan-regional]
	# object rcg includes only data from hauls RCG area
		# example:
			# if a long trip fished in BA and NSEA
				# all its data will be in rcg_all from BA and NSEA
				# only the data from hauls in BA will be in rcg (BA), only the data from hauls in NA will be in rcg (NA). If you merge the two you get the full trip present in rcg_all.

				
	# 2019-04-04: CE: added var LandingCountry (2 first letters of Harbour)
	# 2019-04-04: CE: effort variables passed to numeric (some sum exceeded integer limits)
	# 2019-04-06: CE: added data correction: TripsNumber==999
	# 2019-04-07: fixed bug sl_rcg was being assigned to hl_rcg
	# 2019-04-07: added preparation of stock: CL
	# 2019-04-07: added preparation of stock: CE
	# 2019-04-07: added preparation of stock: CS
	# 2019-04-13: split CE+CL and CS and Stock preparation (avoids memory issues) 
	# 2019-04-13: adapted structure of data directory to multiple RCGs
	

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
 
	# wishlist - ask HKN to remove "last line" and standardize names of files
 
 rm(list=ls())
 library(data.table)

 
 # read file names
 	file_cl <- "data\\001_original\\CL 2009-2018.csv" # should be CL Landings 2009-2018
	file_ce <- "data\\001_original\\CE Effort 2009-2018.csv" 
	 
 
# read data
	cl<-fread(file_cl, stringsAsFactors=FALSE, verbose=FALSE, TRUE, sep=";", na.strings="NULL", nrows=9346797)
	ce<-fread(file_ce, stringsAsFactors=FALSE, verbose=FALSE, fill=TRUE, sep=";", na.strings="NULL", nrows = 1066183)
 

# QCA: duplicates (eliminates if existing)
	dim(cl); cl<-unique(cl); dim(cl)
	dim(ce); ce<-unique(ce); dim(ce)
 
# ====================== 
# create directory structure
# ====================== 

dir.create(paste("data\\002_prepared\\RCG_NA", sep=""),recursive=TRUE, showWarnings=FALSE)
dir.create(paste("data\\002_prepared\\RCG_BA", sep=""),recursive=TRUE, showWarnings=FALSE)
dir.create(paste("data\\002_prepared\\RCG_NSEA", sep=""),recursive=TRUE, showWarnings=FALSE)
 
# ========================
# rename column (should be moved to extraction)
# ======================== 	
	
	colnames(cl)[colnames(cl)=="vesselLengthCategory"]<-"VesselLengthCategory"

	
# ====================== 
# Set Prep Options 
# ======================  
 
target_region <- "RCG_NA" # "RCG_BA", "RCG_NSEA"
year_start <- 2009
year_end <- 2018
dir_output_rcg<-paste("data\\002_prepared\\",target_region,sep="")
dir_output_all<-"data\\002_prepared"


# ======================
# Tweak on areas/region 
# ====================== 
 
# CL and CE 
 
 # QCA: check for ambiguous records (e.g., 27.7; 27.3)
	# Area 27.7
		ce[Area=="27.7",.N,c("FlagCountry","Region","FishingGround","Area")]
		ce[Area=="27.7",.N,c("FlagCountry","Region","FishingGround","Area","StatisticalRectangle")]
		# corrects
		ce[Area=="27.7" & FlagCountry=="IRL","FishingGround"]<-NA
		ce[Area=="27.7" & FlagCountry=="IRL","StatisticalRectangle"]<-NA
	#	Area 27.3
		ce[Area=="27.3",.N,c("FlagCountry","Region","FishingGround","Area")]
		# records from LTU "OTM_SPF_32-69_0_0" are likely NSEA
			ce[Area=="27.3" & FlagCountry=="LTU","Region"]<-"NSEA"
			ce[Area=="27.3" & FlagCountry=="LTU","FishingGround"]<-NA
			ce[Area=="27.3" & FlagCountry=="LTU","Area"]<-"27.3.a"
 			ce[Area=="27.3",.N, c("FlagCountry","Year","Region","FishingGround","Area")]


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
			table(cl[Region=="BS" &  !Area %in% target_areas,"Area"])
			table(cl[!Region=="BS" &  Area %in% target_areas,"Area"])
			# QCA: should yield BS 
			#table(cl_rcg$Region)
				# corrects
				#cl_rcg[!Region=="BS",FishingGround:=NA,]	
				#cl_rcg[!Region=="BS",Region:="BS",]		
		
		ce_rcg <- ce[Area %in% target_areas & Year>=year_start & Year<=year_end,]
			# QCA: should yield 0
				# ATT: a few records 27.3 and 27.7 in BA?! 
				table(ce[Region=="BS" &  !Area %in% target_areas,"Area"])	 
				table(ce[!Region=="BS" &  Area %in% target_areas,"Area"])	 
			# QCA: should yield BS 
				#table(ce_rcg$Region)
				# corrects
					#ce_rcg[!Region=="BS",FishingGround:=NA,]				
					#ce_rcg[!Region=="BS",Region:="BS",]	
		
		target_trips <- hh[Area %in% target_areas & Year>=year_start & Year<=year_end,unique(CS_TripId),]
		tr_rcg_all <- tr[CS_TripId %in% target_trips,]
		hh_rcg_all <- hh[CS_TripId %in% target_trips,]
		sl_rcg_all <- sl[CS_TripId %in% target_trips,]
		hh_rcg_all <- hh[CS_TripId %in% target_trips,]
		ca_rcg_all <- ca[CS_TripId %in% target_trips,]
		
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
				table(cl_rcg$Region)
				table(cl_rcg[!Region=="NSEA",Area])
					# corrects				
					cl_rcg[!Region=="NSEA" & (Area %in% target_areas_nsea | grepl(Area, pat="21.") ),Region:="NSEA",]
					table(cl_rcg$Region)
			
		ce_rcg <- ce[ (Area %in% target_areas_nsea | grepl(Area, pat="21.") ) & Year>=year_start & Year<=year_end,]
			# QCA: should yield 0
				# ATT: a few records 41, 51 and 57 in NSEA [these have not been included in ce_rcg] 
				table(ce[Region=="NSEA" & !(Area %in% target_areas_nsea | grepl(Area, pat="21.") ),"Area"])	
				table(ce[!Region=="NSEA" & (Area %in% target_areas_nsea | grepl(Area, pat="21.") ),"Area"])	
			# QCA: should yield NSEA
				# ATT: a few records (21 - NAFO) not NSEA?!			
				table(ce_rcg$Region)
				table(ce_rcg[!Region=="NSEA",Area])		
					# corrects	
					ce_rcg[!Region=="NSEA" & grepl(Area, pat="21."),Region:="NSEA",]
					table(ce_rcg$Region)
	
		
		
		target_trips <- hh[(Area %in% target_areas_nsea | grepl(Area, pat="21.") ) & Year>=year_start & Year<=year_end,unique(CS_TripId),]
		tr_rcg_all <- tr[CS_TripId %in% target_trips,]
		hh_rcg_all <- hh[CS_TripId %in% target_trips,]
		sl_rcg_all <- sl[CS_TripId %in% target_trips,]
		hh_rcg_all <- hh[CS_TripId %in% target_trips,]
		ca_rcg_all <- ca[CS_TripId %in% target_trips,]
					# corrects	
					hh_rcg_all[!Region=="NSEA" & grepl(Area, pat="21."),Region:="NSEA",]
					ca_rcg_all[!Region=="NSEA" & grepl(Area, pat="21."),Region:="NSEA",]
	
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
	
	# formats CE 
	ce[,HarbourDesc:=iconv(HarbourDesc, from="UTF-8", to="")]
	ce[,HarbourDesc:=toupper(HarbourDesc)]
	ce_rcg[,HarbourDesc:=iconv(HarbourDesc, from="UTF-8", to="")]
	ce_rcg[,HarbourDesc:=toupper(HarbourDesc)]


		
# ========================	
# Creates additional variables
# ========================	

	# CL

		cl[,OfficialLandingCatchWeight_ton := OfficialLandingCatchWeight/1000]
		cl[,OfficialLandingCatchWeight_1000ton := OfficialLandingCatchWeight/1000000]
		cl_rcg[,OfficialLandingCatchWeight_ton := OfficialLandingCatchWeight/1000]
		cl_rcg[,OfficialLandingCatchWeight_1000ton := OfficialLandingCatchWeight/1000000]
	
	# CE 
		# can be improved
		ce[,LandingCountry:=substr(Harbour, start=1, stop=2)]
		ce_rcg[,LandingCountry:=substr(Harbour, start=1, stop=2)]

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
		# CL [note: at the moment this is only being run on cl_rcg, not on the larger cl]
		# =====================		
				
		cl_rcg[,ISSCAAP:=ASFIS_WoRMS_updt$ISSCAAP[match(cl_rcg$SpeciesAphiaID, ASFIS_WoRMS_updt$AphiaID_accepted)]]
		
			# QCA should yield zero, if not more tweaks are needed
			sum(is.na(cl_rcg$Species))
			
			# QCA should yield zero, if not more tweaks are needed
			sum(is.na(cl_rcg$ISSCAAP))
			
			cl_rcg[Species == "Gaidropsarus guttatus",ISSCAAP:=32] # Cods, hakes, haddocks
			cl_rcg[Species == "Mullus barbatus",ISSCAAP:=33] # Miscellaneous coastal fishes
			cl_rcg[Species == "Auxis rochei",ISSCAAP:=36] # Tunas, bonitos, billfishes
			cl_rcg[Species == "Auxis thazard",ISSCAAP:=36] # Tunas, bonitos, billfishes
			cl_rcg[Species == "Scombrinae Rafinesque",ISSCAAP:=36] # Tunas, bonitos, billfishes
			cl_rcg[Species == "Scomberesox saurus",ISSCAAP:=37] # Miscellaneous pelagic fishes
			cl_rcg[Species == "Selachii",ISSCAAP:=38] # Sharks, rays, chimaeras
			cl_rcg[Species == "Squatina",ISSCAAP:=38] # Sharks, rays, chimaeras
			cl_rcg[Species == "Pisces",ISSCAAP:=39] # Marine fishes not identified
			cl_rcg[Species == "Macropodia",ISSCAAP:=42] # Crabs, sea-spiders
			cl_rcg[Species == "Dardanus arrosor",ISSCAAP:=44] # King crabs, squat-lobsters
			cl_rcg[Species == "Pasiphaea",ISSCAAP:=45] # Shrimps, prawns
			cl_rcg[Species == "Dendrobranchiata",ISSCAAP:=45] # Shrimps, prawns
			cl_rcg[Species == "Crangon",ISSCAAP:=45] # Shrimps, prawns
			cl_rcg[Species == "Decapodiformes",ISSCAAP:=47] # Miscellaneous marine crustaceans
			cl_rcg[Species == "Megabalanus azoricus",ISSCAAP:=47] # Miscellaneous marine crustaceans
			cl_rcg[Species == "Gibbula",ISSCAAP:=52] # Abalones, winkles, conchs
			cl_rcg[Species == "Venerupis philippinarum",ISSCAAP:=56] # Clams, cockles, arkshells
			cl_rcg[Species == "Arcopagia crassa",ISSCAAP:=56] # Clams, cockles, arkshells
			cl_rcg[Species == "Loligo forbesii",ISSCAAP:=57] # Squids, cuttlefishes, octopuses
			cl_rcg[Species == "Echinidae",ISSCAAP:=76] # Sea-urchins and other echinoderms
			cl_rcg[Species == "Laminaria",ISSCAAP:=91] # Brown seaweeds
			
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
			cl_rcg[Species == "Lichia amia",Catch_group:="large pelagic"]
			cl_rcg[Species == "Acipenser sturio",Catch_group:="diadromous"]
			
			# give it a check (see if it makes sense)
			 # check demersal
				head(cl_rcg[Catch_group == "demersal",list(Kg=sum(OfficialLandingCatchWeight)),list(Species)] [order(-Kg),],20)
			# check flatfish
				head(cl_rcg[Catch_group == "flatfish",list(Kg=sum(OfficialLandingCatchWeight)),list(Species)] [order(-Kg),],20)
			# check small pelagic
				head(cl_rcg[Catch_group == "small pelagic",list(Kg=sum(OfficialLandingCatchWeight)),list(Species)] [order(-Kg),],20)
			# check large pelagic
				head(cl_rcg[Catch_group == "large pelagic",list(Kg=sum(OfficialLandingCatchWeight)),list(Species)] [order(-Kg),],20)
			# check molluscs
				head(cl_rcg[Catch_group == "molluscs",list(Kg=sum(OfficialLandingCatchWeight)),list(Species)] [order(-Kg),],20)
			# check crustaceans
				head(cl_rcg[Catch_group == "crustaceans",list(Kg=sum(OfficialLandingCatchWeight)),list(Species)] [order(-Kg),],20)
			# check	elasmobranchs
				head(cl_rcg[Catch_group == "elasmobranchs",list(Kg=sum(OfficialLandingCatchWeight)),list(Species)] [order(-Kg),],20)
			# check	diadromous
				head(cl_rcg[Catch_group == "diadromous",list(Kg=sum(OfficialLandingCatchWeight)),list(Species)] [order(-Kg),],20)
			# check	incidental by-catch
				head(cl_rcg[Catch_group == "incidental by-catch",list(Kg=sum(OfficialLandingCatchWeight)),list(Species)] [order(-Kg),],20)			
			# check	other
				head(cl_rcg[Catch_group == "other",list(Kg=sum(OfficialLandingCatchWeight)),list(Species)] [order(-Kg),],20)

		
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
	
	ce_rcg[,FlagCountry:=factor(FlagCountry, levels=sort(unique(FlagCountry))),]
	ce_rcg[,FishingActivityCategoryEuropeanLvl5:=factor(FishingActivityCategoryEuropeanLvl5, levels=sort(unique(FishingActivityCategoryEuropeanLvl5))),]
	ce_rcg[,FishingActivityCategoryEuropeanLvl6:=factor(FishingActivityCategoryEuropeanLvl6, levels=sort(unique(FishingActivityCategoryEuropeanLvl6))),]
	ce_rcg[,Harbour:=factor(Harbour, levels=sort(unique(Harbour))),]
	ce_rcg[,VesselLengthCategory:=factor(VesselLengthCategory, levels=c("<10","10-<12","12-<18","18-<24","24-<40",">40"))]
	ce_rcg[,DaysAtSea:=as.numeric(DaysAtSea)]
	ce_rcg[,KWDays:=as.numeric(KWDays)]
	ce_rcg[,GTDays:=as.numeric(GTDays)]


	# region specific	
	if(target_region=="RCG_BA"){stop("define")}
	if(target_region=="RCG_NSEA"){stop("define")}
	if(target_region=="RCG_NA"){
		target_FishingGround<-c("5b","6","7a","7bcjk","7e","7fgh","8abde","8c+9","10")
		print(cl_rcg[!FishingGround %in% c(NA,target_FishingGround),.N, "FlagCountry"])
		print(ce_rcg[!FishingGround %in% c(NA,target_FishingGround),.N, "FlagCountry"])
		cl_rcg[,FishingGround:=factor(FishingGround, levels=target_FishingGround),]
		ce_rcg[,FishingGround:=factor(FishingGround, levels=target_FishingGround),]
		
		target_Areas<-c('27.5.b','27.5.b.1','27.5.b.2','27.6','27.6.a','27.6.a.n','27.6.a.s','27.6.b','27.7','27.7.a','27.7.b','27.7.c','27.7.e','27.7.f','27.7.g','27.7.h','27.7.j','27.7.k','27.8.a','27.8.b','27.8.c','27.8.d','27.8.e','27.9.a','27.9.b','27.9.b.1','27.9.b.2','27.10','27.10.a','27.10.b')
		print(cl_rcg[!Area %in% target_Areas,.N, c("FlagCountry","Area")])
		print(ce_rcg[!Area %in% target_Areas,.N, c("FlagCountry","Area")])
		cl_rcg[,Area:=factor(Area, levels=target_Areas),]
		ce_rcg[,Area:=factor(Area, levels=target_Areas),]
		}	
	
# ================	
# data update: IRL	
# ================	
	
	# update: TripsNumber == 999 are TripsNumber = 0
	ce[TripsNumber==999 & FlagCountry=="IRL",TripsNumber:=0,]
	ce_rcg[TripsNumber==999 & FlagCountry=="IRL",TripsNumber:=0,]

	
# ========================
# saves data
# ========================	

	file_info_cl<-file.info(file_cl)
	file_info_ce<-file.info(file_ce)

	time_tag<-format(Sys.time(), "%Y%m%d%H%M")

	save(cl_rcg, file_info_cl, file = paste(dir_output_rcg, paste("\\RDB",target_region,"CL", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
	save(ce_rcg, file_info_ce, file = paste(dir_output_rcg, paste("\\RDB",target_region,"CE", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
	
	save(cl, file_info_cl, file = paste(dir_output_all, paste("\\RDB","All_Regions","CL", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))
	save(ce, file_info_ce, file = paste(dir_output_all, paste("\\RDB","All_Regions","CE", year_start, year_end, "prepared",time_tag, sep="_"),".Rdata", sep=""))

	

