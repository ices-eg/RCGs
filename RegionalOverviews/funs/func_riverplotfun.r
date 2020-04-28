riverplotfun <- function(data=cl_rcg,left='FlagCountry',right='LandingCountry',
                         value='OfficialLandingCatchWeight',palette=NULL,
                         threshold=0.001,title='',save=TRUE,
                         filename='riverplot.png',width=6,
                         height=4,pointsize=10,res=600,addToTitle,newVarName,var1,var2,...) {
  
  	# prepares a river plot
	# Hans Gerritsen, MI, Sweden
	# Developed @ RCG subgroup work 2019
	
	
	# e.g.: 
		# CE:
			#riverplotfun(ce_rcg, left = 'FlagCountry', right = 'HarbourCountry', title='FlagCountry (left) to HarbourCountry (right) - TripsNumber - all vessel lengths',value='TripsNumber',filename=paste(target_region,'_2.5.8_r1.png', sep=""))
  
  
  # data data frame e.g. cl_rcg 
  # left variable name (in quotes) of the variable on the left of the plot, e.g. FlagCountry
  # right as above, e.g. LandingCountry
  # value determines the width of the edges
  # palette custom colour palette data frame with the columns 'Country' and 'colour'
  # threshold remove any edges below the threshold (proportion of total Value)
  # title plot title
  # png save as png (TRUE) or display in current graphics device (FALSE)
  # the rest are parameters for png()

  require(riverplot)
  assignInNamespace("debug",FALSE,ns="riverplot")
  
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

    caption <- paste("River plot of ",newVarName," between ", rename_var1(var1)," (left) and ", rename_var2(var2)," (right). Width of line is proportional to landings", sep="")  
    
  if(save) png(filename,width,height,'in',pointsize,res=res,...)
    par(mar=c(0,0,3,0))
    plot(r,srt=0,text.pos=2,node_margin=0.05,plot_area=0.96)
    mtext(title,3,0)
  if(save) dev.off()
    return(list(r, caption))
}




# # data preparation
#
#
#
#
# load('../Data/RDB_RCG_NA_CL_2009_2017_prepared_201904020909.RData')
# load('../Data/RDB_RCG_NA_CE_2009_2017_prepared_201904020909.RData')
# 
# cl <- subset(cl_rcg,Year==2017)
# ce <- subset(ce_rcg,Year==2017)
# rm(cl_rcg,ce_rcg)
# gc(reset=T)
# 
# pal <- read.table('aux_colours.txt',T)
# plot.new()
# legend('left',legend=pal$Country,fill=as.character(pal$colour1),title='colour1')
# legend('center',legend=pal$Country,fill=as.character(pal$colour2),title='colour2')
# legend('right',legend=pal$Country,fill=as.character(pal$colour3),title='colour3')
# 
# 
# # pick the colour scheme:
# pal$colour <- pal$colour3
# 
# riverplotfun(cl,title='2017 - all species',palette=pal,filename='2.1.8_r1.png')
# riverplotfun(subset(cl,Catch_group=='small pelagic'),title='2017 - small pelagic',palette=pal,filename='2.2.8_r1.png')
# riverplotfun(subset(cl,Catch_group=='demersal'),title='2017 - demersal',palette=pal,filename='2.3.8_r1.png')
# riverplotfun(subset(cl,Catch_group=='flatfish'),title='2017 - flatfish',palette=pal,filename='2.4.8_r1.png')
# 
# # no landing country in ce
# ce$LandingCountry2 <- substring(ce$Harbour,1,2)
# # manual fix
# ce$LandingCountry <- NA
# ce$LandingCountry[ce$LandingCountry2=='BE'] <- 'BEL'
# ce$LandingCountry[ce$LandingCountry2=='DE'] <- 'DEU'
# ce$LandingCountry[ce$LandingCountry2=='DK'] <- 'DNK'
# ce$LandingCountry[ce$LandingCountry2=='ES'] <- 'ESP'
# ce$LandingCountry[ce$LandingCountry2=='FO'] <- 'FRO'
# ce$LandingCountry[ce$LandingCountry2=='FR'] <- 'FRA'
# ce$LandingCountry[ce$LandingCountry2=='GB'] <- 'GBR'
# ce$LandingCountry[ce$LandingCountry2=='GG'] <- 'GGY'
# ce$LandingCountry[ce$LandingCountry2=='IE'] <- 'IRL'
# ce$LandingCountry[ce$LandingCountry2=='IM'] <- 'IMN'
# ce$LandingCountry[ce$LandingCountry2=='IS'] <- 'ISL'
# ce$LandingCountry[ce$LandingCountry2=='JE'] <- 'JEY'
# ce$LandingCountry[ce$LandingCountry2=='MA'] <- 'MAR'
# ce$LandingCountry[ce$LandingCountry2=='NL'] <- 'NLD'
# ce$LandingCountry[ce$LandingCountry2=='NO'] <- 'NOR'
# ce$LandingCountry[ce$LandingCountry2=='PL'] <- 'POL'
# ce$LandingCountry[ce$LandingCountry2=='PT'] <- 'PRT'
# 
# 
# riverplotfun(ce,title='2017 - KWDays',value='KWDays',filename='2.5.8_r1.png')

