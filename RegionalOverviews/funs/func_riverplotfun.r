riverplotfun <- function(data=cl_rcg,left='FlagCountry',right='LandingCountry',
                         value='OfficialLandingCatchWeight',palette=NULL,
                         threshold=0.001,title='',save=TRUE,
                         filename='riverplot.png',width=6,
                         height=4,pointsize=10,res=600,...) {
  
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
