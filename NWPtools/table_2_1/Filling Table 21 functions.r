#fill-in Table 2.1 set of functions
#Joël Vigneau, July 2021

loadEurostat <- function(path) {
	#loads all files including 'fish_ca' in the name and formatted as a '.tsv' file
	listFiles <- list.files(path)
	euroFiles <- listFiles[grepl('fish_ca',listFiles)]
	euroFiles <- euroFiles[grepl('.tsv',euroFiles)]
	euroDF <- data.frame()
	if (length(euroFiles)>1) {
		for (ff in 1:length(euroFiles)) {
		DF <- read.table(paste(path,euroFiles[ff],sep='/'), header=TRUE, sep='\t', as.is=TRUE)
		for (nc in 2:ncol(DF)) DF[,nc] <- gsub(' p','',DF[,nc])  #cleaning of the provisionary lable 'p' attached to some figures
		euroDF <- rbind.data.frame(euroDF, DF)
		}
	} else {
		print('there\'s no EUROSTAT file here!')
	}
	return(euroDF)
}
	

loadFides <- function(path,ctry=NA,years,isFides=TRUE) {
	#TAC <- read excel files per year and coutry, e.g. UE2018.xls
	# not the standard format of FIDES, so to be changed when these are available
	if (isFides) {
	TAC <- data.frame()
	fName <- unlist(lapply(ctry,function(x) paste0(x,years,'.xls')))
	fName <- paste(path,fName,sep='/')
	for (cc in 1:length(fName)) {
		tac <- read_excel(fName[cc], sheet=1, skip=0, col_names=TRUE)
		tac$year <- as.numeric(substring(fName[cc],nchar(fName[cc])-7,nchar(fName[cc])-4))
		TAC <- rbind.data.frame(TAC, tac)
	}
	} else {
	#use of the table proposed in https://griffincarpenter.org/reports/european-fishing-quotas-2001-2021/
		GEO     <- read.table(paste(pathIn,'geo.def',sep='/'),header=TRUE,sep=";", as.is=TRUE)
		GEO$geo <- toupper(GEO$geo)
		tac <- read_excel(paste(path,'Record of European TACs.xlsx',sep='/'), sheet=1, skip=0, col_names=TRUE)
		tac <- tac[tac$'Amendment check' %in% 'Final',] #Avoid duplication with 'Not Final'
		tac$Level[tac$Level %in% 'EU'] <- 'European union (27 MS)'
		species <- unlist(lapply(strsplit(tac$Abbreviation, split='/'), function(x) x[1]))
		area <- unlist(lapply(strsplit(tac$Abbreviation, split='/'), function(x) x[2]))
		TAC <- data.frame(Country = tac$Level, 'Stock Group' = NA, Species=species, Area=area,  'SC Type'=NA,
							'Parent Species' = NA, 'Parent Area' = tac$'TAC Zone', adaptedQuota = tac$'Agreed TAC',
							Margin = NA, Catches = NA, 'SC Catches' = NA, 'percent cons' = NA, Unit=NA, 'Fishing stop' = NA, 
							year= tac$Year)
		TAC <- merge(TAC, GEO,  by.x='Country', by.y = 'Geopolitical_entity', all.x=TRUE)
		TAC <- TAC[,c(17,2:15)]
		TAC$Catches <- as.numeric(TAC$Catches)
		TAC$adaptedQuota <- as.numeric(as.character(TAC$adaptedQuota))
		TAC <- TAC[TAC$year %in% years,]
	}
	return(TAC)
}

loadNational <- function(pathIn, years) {
	SAC <- data.frame()
	fName <- paste('P08_SACROIS_OBSDEB_CAPTURES_',years,'.csv',sep='')
	fName <- paste(pathIn, fName, sep='/')
		for (fn in fName) {
			sac <- read.table(fn, sep=';', encoding = 'UTF-8', header=TRUE, as.is=TRUE)
			SAC <- rbind.data.frame(SAC, sac)
		}

	SAC$SECTEUR <- SAC$SECTEUR_COD
	secteur<- read.csv('C:/_PROGRAMME/DATA/refTables/ISIH-13605-secteur_hierarchie-20210712162005.txt', header=TRUE, sep=';',as.is=TRUE)
	sectr<- read.csv('C:/_PROGRAMME/DATA/refTables/ISIH-13605-secteur-20210712164004.txt', header=TRUE, sep=';', as.is=TRUE)

	SAC <- merge(SAC,sectr[,c('SECT_COD','TSECT_COD','SECT_LIB')], by.x='SECTEUR_COD', by.y='SECT_COD', all.x=TRUE)
	SAC <- merge(SAC, secteur[secteur$TSECT_COD_PERE %in% 1,c('SECT_COD','SECT_COD_PERE')], by.x='SECTEUR_COD',by.y='SECT_COD', all.x=TRUE)
	SAC$SECTEUR[SAC$TSECT_COD %in% 2] <- substring(SAC$SECTEUR[SAC$TSECT_COD %in% 2],1,2)
	SAC$SECTEUR[SAC$TSECT_COD %in% 3] <- substring(SAC$SECTEUR[SAC$TSECT_COD %in% 3],1,2)
	SAC$SECTEUR[SAC$TSECT_COD %in% 5] <- SAC$SECT_COD_PERE[SAC$TSECT_COD %in% 5]
	SAC$SECTEUR[SAC$TSECT_COD %in% 6] <- SAC$SECT_COD_PERE[SAC$TSECT_COD %in% 6]
	SAC$SECTEUR[SAC$TSECT_COD %in% 10] <- paste('27.',as.numeric(SAC[SAC$TSECT_COD %in% 10,'SECTEUR_COD'])/1000,sep='')
	SAC$SECTEUR[SAC$TSECT_COD %in% 13] <- convert.statsq.icesarea(SAC[SAC$TSECT_COD %in% 13,'SECTEUR_COD'])$subdivs
	SAC$SECTEUR[SAC$TSECT_COD %in% 30] <- SAC$SECT_COD_PERE[SAC$TSECT_COD %in% 30]
	SAC$SECTEUR[SAC$TSECT_COD %in% 31] <- SAC$SECT_COD_PERE[SAC$TSECT_COD %in% 31]
	SAC$SECTEUR[SAC$TSECT_COD %in% 32] <- SAC$SECT_COD_PERE[SAC$TSECT_COD %in% 32]
	SAC$SECTEUR[SAC$TSECT_COD %in% 40] <- SAC$SECT_COD_PERE[SAC$TSECT_COD %in% 40]
	SAC$SECTEUR[SAC$TSECT_COD %in% 41] <- SAC$SECT_COD_PERE[SAC$TSECT_COD %in% 41]
	SAC$SECTEUR[SAC$TSECT_COD %in% 42] <- SAC$SECT_COD_PERE[SAC$TSECT_COD %in% 42]
	SAC$SECTEUR[SAC$TSECT_COD %in% 43] <- SAC$SECT_COD_PERE[SAC$TSECT_COD %in% 43]
	SAC$SECTEUR[SAC$TSECT_COD %in% 51] <- SAC$SECT_COD_PERE[SAC$TSECT_COD %in% 51]
	SAC$SECTEUR[SAC$TSECT_COD %in% 54] <- SAC$SECT_COD_PERE[SAC$TSECT_COD %in% 54]
	SAC$SECTEUR[SAC$TSECT_COD %in% 56] <- SAC$SECT_COD_PERE[SAC$TSECT_COD %in% 56]
	SAC$SECTEUR[SAC$TSECT_COD %in% 57] <- SAC$SECT_COD_PERE[SAC$TSECT_COD %in% 57]
	SAC$SECTEUR[SAC$TSECT_COD %in% 58] <- SAC$SECT_COD_PERE[SAC$TSECT_COD %in% 58]

	#Cleaning
	pipo <- strsplit(SAC$SECTEUR, split='.')
	SAC$SECTEUR[which(unlist(lapply(pipo, length))==8)] <- substring(SAC$SECTEUR[which(unlist(lapply(pipo, length))==8)],1,6)
	SAC$SECTEUR <- gsub('VIII','27.8.',SAC$SECTEUR)
	SAC$SECTEUR <- gsub('VII','27.7.',SAC$SECTEUR)
	SAC$SECTEUR <- gsub('VI','27.6.',SAC$SECTEUR)
	SAC$SECTEUR <- gsub('IV','27.4.',SAC$SECTEUR)
	SAC$SECTEUR <- gsub('V','27.5.',SAC$SECTEUR)
	SAC$SECTEUR <- gsub('XII','27.12.',SAC$SECTEUR)
	SAC$SECTEUR <- gsub('XI','27.11.',SAC$SECTEUR)
	SAC$SECTEUR <- gsub('IX','27.9.',SAC$SECTEUR)
	SAC$SECTEUR <- gsub('X','27.10.',SAC$SECTEUR)
	SAC$SECTEUR <- gsub('III','27.3.',SAC$SECTEUR)
	SAC$SECTEUR <- gsub('II','27.2.',SAC$SECTEUR)
	SAC$SECTEUR <- gsub('I','27.1.',SAC$SECTEUR)
	SAC$SECTEUR <- gsub('.5.b1','.5.b',SAC$SECTEUR)
	SAC$SECTEUR <- gsub('.5.b2','.5.b',SAC$SECTEUR)
	SAC$SECTEUR[SAC$RG_COD %in% 'MY'] <- 51
	SAC$SECTEUR <- gsub('[[:punct:]]','_',SAC$SECTEUR)
	SAC$SECTEUR <- toupper(SAC$SECTEUR)
	SAC$SECTEUR[SAC$SECTEUR %in% '37'] <- SAC$SECTEUR_COD[SAC$SECTEUR %in% '37']
	SAC$SECTEUR[SAC$SECTEUR %in% c('MD11','MD12','MD21','MD22','MD23','MD40','MDSP','MLGE','MLGO','MLLA','GSA07','MT11','MT12','MT21','MT22','MT23','MT40','MTSP')] <- '3712'
	SAC$SECTEUR[SAC$SECTEUR %in% c('MD31','MD32','MD33','MLCO','MT31','MT32','MT33','MT34','MD34')] <- '3713'
	SAC$SECTEUR[SAC$SECTEUR %in% c('GSA05','GSA06','GSA111')] <- '3711'
	SAC$SECTEUR[SAC$SECTEUR %in% c('GSA08','GSA112','GSA12')] <- '3713'
	SAC$SECTEUR[SAC$SECTEUR %in% c('GSA13','GSA21')] <- '3722'
	SAC$SECTEUR[SAC$SECTEUR %in% '27_1_'] <- '27_1'
	SAC$SECTEUR[SAC$SECTEUR %in% '27_10_'] <- '27_10'
	SAC$SECTEUR[SAC$SECTEUR %in% '27_12_'] <- '27_12'
	SAC$SECTEUR[SAC$SECTEUR %in% '3711'] <- '37_1_1'
	SAC$SECTEUR[SAC$SECTEUR %in% '3712'] <- '37_1_2'
	SAC$SECTEUR[SAC$SECTEUR %in% '3713'] <- '37_1_3'
	SAC$SECTEUR[SAC$SECTEUR %in% c('3720','3722')] <- '37_2_2'
	SAC$SECTEUR[SAC$SECTEUR %in% '3721'] <- '37_2_1'
	SAC$SECTEUR[SAC$SECTEUR %in% '3732'] <- '37_3_2'
	return(SAC)
}	

#For info, this is the structure of the nat file, but you don't need all these fields...
# only (to be checked) ANNEE (i.e. year), ESP_COD_FAO, CAPTURES_KG, SECTEUR (area)
# str(NAT)
# 'data.frame':	1740434 obs. of  25 variables:
 # $ SECTEUR_COD        : chr  "" "" "" "" ...
 # $ ANNEE              : int  2017 2017 2017 2017 2017 2017 2017 2017 2017 2017 ...
 # $ SOURCE             : chr  "OBSDEB" "OBSDEB" "OBSDEB" "OBSDEB" ...
 # $ RG_COD             : chr  "GY" "GY" "GY" "GY" ...
 # $ QAM_COD            : chr  "CY" "CY" "CY" "CY" ...
 # $ DCR_SEGMENT_CE_LIB : chr  "Filets dérivants et filets fixes" "Filets dérivants et filets fixes" "Filets dérivants et filets fixes" "Filets dérivants et filets fixes" ...
 # $ CLASSE_TAILLE      : chr  "VL1012" "VL1012" "VL1012" "VL1012" ...
 # $ PORT_EXP_COD       : chr  "ECY" "MCY" "ECY" "MCY" ...
 # $ PORT_EXP_LIB       : chr  "Sinnamary" "Saint Georges" "Sinnamary" "Saint Georges" ...
 # $ METIER_DCF_5_COD   : chr  "GND_DEF" "GND_DEF" "GND_DEF" "GND_DEF" ...
 # $ METIER_DCF_5_LIB   : chr  "Filets maillants dérivants - Poissons démersaux" "Filets maillants dérivants - Poissons démersaux" "Filets maillants dérivants - Poissons démersaux" "Filets maillants dérivants - Poissons démersaux" ...
 # $ NIV_SECTEUR_COD    : chr  "" "" "" "" ...
 # $ TRIMESTRE          : int  1 1 1 1 1 1 1 1 1 1 ...
 # $ ESP_COD_FAO        : chr  "YNA" "YNV" "AXP" "AXP" ...
 # $ ESP_LIB_FAO        : chr  "Acoupa toeroe" "Acoupa cambucu" "Mâchoiron crucifix" "Mâchoiron crucifix" ...
 # $ CAPTURES_KG        : num  8763 1655 865 611 436 ...
 # $ CAPTURES_KG_INF_95 : num  8134 1576 803 582 415 ...
 # $ CAPTURES_KG_SUP_95 : num  9392 1734 927 640 457 ...
 # $ VALEUR_EUROS       : num  27417 3717 1138 804 774 ...
 # $ VALEUR_EUROS_INF_95: num  25449 3540 1057 766 737 ...
 # $ VALEUR_EUROS_SUP_95: num  29385 3894 1220 842 812 ...
 # $ SECTEUR            : chr  "" "" "" "" ...
 # $ TSECT_COD          : int  NA NA NA NA NA NA NA NA NA NA ...
 # $ SECT_LIB           : chr  NA NA NA NA ...
 # $ SECT_COD_PERE      : chr  NA NA NA NA ...
# > 

loadRCG <- function(path){
  
  # data prepared by the iSSG on Catch, effort and sampling overviews -> https://community.ices.dk/ExternalSites/datacollection/Regional%20coordination%20meetings%202017/RCGIntersessionalWork/_layouts/15/start.aspx#/SitePages/HomePage.aspx?RootFolder=%2FExternalSites%2Fdatacollection%2FRegional%20coordination%20meetings%202017%2FRCGIntersessionalWork%2F2021%20Meeting%20docs%2F06%2E%20Data%2FRDB%20data%2FPrepared%5FData&FolderCTID=0x01200012FE02F9A107C74AB6D1BF2EA07B1474&View=%7B3D371B98%2DB1E5%2D455D%2D9EA9%2D06746B7D2B5B%7D
  load(paste(path,'/RDB_All_Regions_CL_2009_2020_prepared_20210428.Rdata' ,sep = ''))
  
  
  # aggregate
  #rcgDF = aggregate(OfficialLandingCatchWeight_ton ~Year + FlagCountry + Species + SpeciesAphiaID + Area, data = cl, FUN = sum, na.rm = TRUE)  
  rcgDF = data.table::setDT(cl)[, .(OfficialLandingCatchWeight_ton = sum(OfficialLandingCatchWeight_ton)), by = .( Year, FlagCountry,Species, SpeciesAphiaID, Area)]
  rcgDF = as.data.frame(rcgDF)
  
  # Tweak to Swedish catches in "27.3.d.28.1"
	# the area of catches is "27.3.d.28" - In the RDB prep script the catches were wrongly missassigned to "27.3.d.28.1"
	# the issue has been corrected in the script but a new prepared file has not been produced, so a tweak is put here
	# This code segment can be deleted if a new file is produced
	rcgDF[rcgDF$FlagCountry=="SWE" & rcgDF$Area=="27.3.d.28.1" & rcgDF$Year %in% c(2009:2020),"Area"]<-"27.3.d.28.2"
  
  rm(cl)
  return(rcgDF)
}