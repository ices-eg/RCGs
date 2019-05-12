require(riverplot)

		# RCG
		target_region<-"RCG_NSEA"
		
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

			
cl_rcg <- subset(cl_rcg,Year==2018)
ce_rcg <- subset(ce_rcg,Year==2018)
cl_rcg<-as.data.frame(cl_rcg)
ce_rcg<-as.data.frame(ce_rcg)
#gc(reset=T)

pal <- read.table("aux_colours.txt", header=T, sep="\t", colClasses="character", na.strings="", comment.char="")

# plot.new()
# legend('left',legend=pal$Country,fill=as.character(pal$colour1),title='colour1')
# legend('center',legend=pal$Country,fill=as.character(pal$colour2),title='colour2')
# legend('right',legend=pal$Country,fill=as.character(pal$colour3),title='colour3')


# pick the colour scheme:
pal$colour <- pal$colour4

riverplotfun <- function(data=cl_rcg,left='FlagCountry',right='LandingCountry',
                         value='OfficialLandingCatchWeight',palette=NULL,
                         threshold=0.001,title='',save=TRUE,
                         filename='riverplot.png',width=6,
                         height=4,pointsize=10,res=600,...) {
  # data: data frame e.g. cl_rcg 
  # left: variable name (in quotes) of the variable on the left of the plot, e.g. FlagCountry
  # right: as above, e.g. LandingCountry
  # value: determines the width of the edges
  # palette: custom colour palette: data frame with the columns 'Country' and 'colour'
  # threshold: remove any edges below the threshold (proportion of total Value)
  # title: plot title
  # png: save as png (TRUE) or display in current graphics device (FALSE)
  # the rest are parameters for png()

  edges <- aggregate(data[,value],list(N1=toupper(data[,left]),N2=tolower(data[,right])),sum,na.rm=T)
  names(edges)[3] <- 'Value'
  edges <- subset(edges,Value>sum(edges$Value)*threshold)
  nodes <- data.frame(ID=sort(unique(edges$N1)),x=0)
  nodes <- rbind(nodes,data.frame(ID=sort(unique(edges$N2)),x=1))
  i <- as.numeric(factor(toupper(nodes$ID)))
  
  if(is.null(palette)){
   Country <-  sort(unique(toupper(c(levels(data[,left]),levels(data[,right])))))
   colour <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", 
                "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#E5C494", "#B15928", 
                "#FDDAEC", "#E7298A", "#FFFFCC", "#FFED6F", "#F2F2F2", "#AAAAAA", 
                "#666666")
   palette <- data.frame(Country,colour=rep(colour,100)[1:length(Country)])
  } 
  i <-match(toupper(nodes$ID),palette$Country)
  nodes$col <- as.character(palette$colour)[i]
  nodes$col <- ifelse(is.na(nodes$col),'#0000FF',nodes$col)
  #legend('center',legend=nodes$ID,fill=nodes$col,ncol=3)
  
  r <- makeRiver(nodes, edges)
  if(save) png(filename,width,height,'in',pointsize,res=res,...)
    par(mar=c(0,0,3,0))
    plot(r,srt=0,text.pos=2,node_margin=0.05,plot_area=0.96)
    mtext(title,3,0)
  if(save) dev.off()
}

# CE
riverplotfun(cl_rcg, title='FlagCountry (left) to LandingCountry (right) - landings - all species',palette=pal,filename=paste(target_region,'_2.1.8_r1.png', sep=""))
riverplotfun(subset(cl_rcg,Catch_group=='small pelagic'),title='FlagCountry (left) to LandingCountry (right) - landings - small pelagic',palette=pal,filename=paste(target_region,'_2.2.8_r1.png', sep=""))
riverplotfun(subset(cl_rcg,Catch_group=='demersal'),title='FlagCountry (left) to LandingCountry (right) - landings - demersal',palette=pal,filename=paste(target_region,'_2.3.8_r1.png', sep=""))
riverplotfun(subset(cl_rcg,Catch_group=='flatfish'),title='FlagCountry (left) to LandingCountry (right) - landings - flatfish',palette=pal,filename=paste(target_region,'_2.4.8_r1.png', sep=""))

# CL
riverplotfun(ce_rcg, left = 'FlagCountry', right = 'HarbourCountry', title='FlagCountry (left) to HarbourCountry (right) - TripsNumber - all vessel lengths',value='TripsNumber',filename=paste(target_region,'_2.5.8_r1.png', sep=""))





