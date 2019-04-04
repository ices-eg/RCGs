barplot_var_by_one_var <- function(x,  Var, var1, tapply_type, type_of_threshold, value_of_threshold, sorted = FALSE, graph_par=list(oma = c(1,1,1,1), mai = c(1,1,.5,.5), ylab_line = 4, cex.x = 1, col=NA), grouped = FALSE)
	{
	#
	# prepares a barplot of Var per var1
	# Nuno Prista, SLU, Sweden
	# Developed @ RCM NS&EA 2017-18, RCG subgroup work 2019
	
	
	# e.g.: 
		# CA:
			# barplot_var_by_one_var(x = data1$caSub, Var = "Age" , var1 = "FlagCountry", tapply_type = "length")
		# CL: 
			# barplot_var_by_one_var(x = data1$clSub, Var = "OfficialLandingCatchWeight" , var1 = "FlagCountry", tapply_type = "sum")

	# ATT:
		# need to check if the droplevels inside the if on type_of_threshold  scramble bar order when unordered - could have to develop a rcg_specific re-factorization
		
		# 2019-04-01: added argument colour
		# 2019-04-01: added argument type_of_threshold, value_of_threshold
		# 2019-04-01: added annotation (percent x, y, threshold)
		# 2019-04-01: added output
		# 2019-04-02: improved subtitle when no threshold is used
		# 2019-04-04: bug corrected: when NA existed, sum was NA (added na.rm)
		# 2019-04-04: added argument grouped [for mfrow and mfcol type graphs]

		
		percent_Var <- round(sum(!is.na(x[,Var]))/dim(x)[1]*100,2)
		percent_var1 <- round(sum(!is.na(x[,var1]))/dim(x)[1]*100,2)

	# subset to threshold
		if(is.na(type_of_threshold)) stop("check type_of_threshold")
		if(!type_of_threshold == "NULL")
			{
			if(type_of_threshold == "cum_percent")
				{
			# determines names to keep
				v1<-cumsum(sort(tapply(x[,Var], x[,var1], sum, na.rm=T), decreasing=T))/sum(x[,Var], na.rm=T)*100
				selected_names<-names(v1[v1<=as.numeric(value_of_threshold)])
			# subsets data to names
				x<-droplevels(x[x[,var1] %in% selected_names,])
				}
			if(type_of_threshold == "absolute")
				{			
			# determines names to keep
				v1<-tapply(x[,Var], x[,var1], sum, na.rm=T)
				selected_names<-names(v1[v1>as.numeric(value_of_threshold)])
			# subsets data to names
				x<-droplevels(x[x[,var1] %in% selected_names,])
				}	
			if(type_of_threshold == "main")
				{			
			# determines names to keep
				v1<-sort(tapply(x[,Var], x[,var1], sum, na.rm=T), decreasing=T)
				selected_names<-names(v1[1:as.numeric(value_of_threshold)])
			# subsets data to names
				x<-droplevels(x[x[,var1] %in% selected_names,])
				}				
			}
			
	#windows(5,5); 
	print(par("new"))
	if(grouped==TRUE) par(cex=0.8, mai = graph_par$mai) else  par(cex=0.8, oma = graph_par$oma, mai = graph_par$mai)
	print(par("new"))
	if (tapply_type == "length") { t1<-tapply(x[,Var], list(x[,var1]), length); y_title = paste("count of", Var) }
	if (tapply_type == "sum") { t1<-tapply(x[,Var], list(x[,var1]), sum, na.rm=T); y_title = paste("sum of", Var) }
	t1[is.na(t1)]<-0
	if (sorted==TRUE) {t1<-sort(t1, decreasing=T)}
	
	# colour
		if(is.na(graph_par$col)) 
			{
			colour_scale <- rainbow(n = nrow(t1))
			} else {
					colour_table<-read.table("aux_colours.txt", header=T)
					if(graph_par$col=="colour1") colour_scale<-colour_table$colour1[match(names(t1),a$Country)]
					if(graph_par$col=="colour2") colour_scale<-colour_table$colour2[match(names(t1),a$Country)]
					}

	barplot(t1, las=2, col=colour_scale, ylab = "", main = paste(Var,"by", var1), cex.names = graph_par$cex.x)
	title(ylab=y_title, line = graph_par$ylab_line)
	if(!type_of_threshold == "NULL")
		{
		title(main=paste("y:", percent_Var,"%; x:",percent_var1,"%; ",type_of_threshold,"(",value_of_threshold,")", sep=""), cex.main=0.9, line = 0)
		} else {
			title(main=paste("y:", percent_Var,"%; x:",percent_var1,"%; ","all_data", sep=""), cex.main=0.9, line = 0)
				}
	out<-data.frame(var1 = rownames(t1), Var = t1, row.names=NULL)
	out
	}
