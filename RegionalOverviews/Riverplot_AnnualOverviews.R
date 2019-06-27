library (riverplot)

		# RCG
		target_region<-"RCG_NA"
		
			if (target_region == "RCG_NA")
			{
			load("data\\002_prepared\\RCG_NA\\RDB_RCG_NA_CL_2009_2018_prepared_201905101612.Rdata")
			load("data\\002_prepared\\RCG_NA\\RDB_RCG_NA_CE_2009_2018_prepared_201905101612.Rdata")
			}
			
			if (target_region == "RCG_BA")
			{
			load("data\\002_prepared\\RCG_BA\\RDB_RCG_BA_CL_2009_2018_prepared_201905101612.Rdata")
			load("data\\002_prepared\\RCG_BA\\RDB_RCG_BA_CE_2009_2018_prepared_201905101612.Rdata")
			}		
			if (target_region == "RCG_NSEA")
			{
			load("data\\002_prepared\\RCG_NSEA\\RDB_RCG_NSEA_CL_2009_2018_prepared_201905101612.Rdata")
			load("data\\002_prepared\\RCG_NSEA\\RDB_RCG_NSEA_CE_2009_2018_prepared_201905101612.Rdata")
			}	

source("funs\\func_riverplotfun.r")			
			
cl_rcg <- subset(cl_rcg,Year==2018)
ce_rcg <- subset(ce_rcg,Year==2018)
cl_rcg<-as.data.frame(cl_rcg)
ce_rcg<-as.data.frame(ce_rcg)
#gc(reset=T)

pal <- read.table("aux_colours.txt", header=T, sep="\t", colClasses="character", na.strings="", comment.char="")

# pick the colour scheme:
pal$colour <- pal$colour4


# CL
riverplotfun(cl_rcg, title='FlagCountry (left) to LandingCountry (right) - landings - all species',palette=pal,filename=paste(target_region,'_2.1.8_r1.png', sep=""))
riverplotfun(subset(cl_rcg,Catch_group=='small pelagic'),title='FlagCountry (left) to LandingCountry (right) - landings - small pelagic',palette=pal,filename=paste(target_region,'_2.2.8_r1.png', sep=""))
riverplotfun(subset(cl_rcg,Catch_group=='demersal'),title='FlagCountry (left) to LandingCountry (right) - landings - demersal',palette=pal,filename=paste(target_region,'_2.3.8_r1.png', sep=""))
riverplotfun(subset(cl_rcg,Catch_group=='flatfish'),title='FlagCountry (left) to LandingCountry (right) - landings - flatfish',palette=pal,filename=paste(target_region,'_2.4.8_r1.png', sep=""))

# CE
riverplotfun(ce_rcg, left = 'FlagCountry', right = 'HarbourCountry', title='FlagCountry (left) to HarbourCountry (right) - TripsNumber - all vessel lengths',value='TripsNumber',filename=paste(target_region,'_2.5.8_r1.png', sep=""))
	# note - the following does not work well in larger RCGs - pallete insufficient for no of countries
	riverplotfun(ce_rcg, left = 'FlagCountry', right = 'HarbourCountry', title='FlagCountry (left) to HarbourCountry (right) - TripsNumber - all vessel lengths',palette=pal ,value='TripsNumber',filename=paste(target_region,'_2.5.8_r1.png', sep=""))





