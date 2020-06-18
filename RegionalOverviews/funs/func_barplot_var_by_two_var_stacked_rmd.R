barplot_var_by_two_var_stacked <- function(x,  Var, var1, var2, tapply_type, proportion, type_of_threshold, value_of_threshold, sorted = FALSE, graph_par=list(oma = c(1,1,1,1), mai = c(1,1,.5,.5), ylab_line = 4, cex.x = 1, col=NA), legend_par, grouped=FALSE, title_root="", save_plot_to_list=TRUE, filter)
	{
	# Prepares a barplot of Var per var1 (with var 2 stacked)
	# Nuno Prista, SLU, Sweden
	# Developed @ RCM NS&EA 2017-18, RCG subgroup work 2019
		
	# e.g.: 
		# CA:
			# barplot_var_by_two_var_stacked(x = data1$caSub,  Var = "Age", var1 = "FlagCountry", var2 = "CatchCategory", tapply_type = "length")
		# CL: 
			# barplot_var_by_two_var_stacked(x = data1$clSub,  Var = "OfficialLandingCatchWeight", var1 = "FlagCountry", var2 = "Area", tapply_type = "sum")

	# ATT:
		# need to check if the droplevels inside the if on type_of_threshold  scramble bar order when unordered - could have to develop a rcg_specific re-factorization
			

		# 2019-04-04: bug corrected: when NA existed, sum was NA (added na.rm)
		# 2019-04-04: added argument grouped [for mfrow and mfcol type graphs]
		# 2019-04-04: improved title
		# 2019-04-11: added tapply_type length_unique
		# 2019-04-11: added title_root
		# 2019-04-14: improve colour specification		
		# 2019-05-08: changed output to list with values "table" and "plot"
		# 2019-05-08: added argument save_plot_to_list (saves plot as second argument of final list)
    # 2020-04-07: added captions K.Krakówka
    # 2020-06-18: added catch groups 
  source("../../funs/fun_rename.r")
  
		percent_Var <- round(sum(!is.na(x[,Var]))/dim(x)[1]*100,2)
		percent_var1 <- round(sum(!is.na(x[,var1]))/dim(x)[1]*100,2)
		percent_var2 <- round(sum(!is.na(x[,var2]))/dim(x)[1]*100,2)

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

			
	#windows(10,5);
	#if(grouped==TRUE) par(cex=0.8, mai = graph_par$mai) else  par(cex=0.8, oma = graph_par$oma, mai = graph_par$mai)	
	if (tapply_type == "length_unique") { t1<-tapply(x[,Var], list(x[,var2],x[,var1]), function(y){length(unique(y))}); y_title = paste("Unique of", rename_var(Var)) }
	if (tapply_type == "length") { t1<-tapply(x[,Var], list(x[,var2],x[,var1]), length); y_title = paste("count of", rename_var(Var)) }
	if (tapply_type == "sum") { t1<-tapply(x[,Var], list(x[,var2],x[,var1]), sum, na.rm=T);  y_title = paste("sum of", rename_var(Var)) }
	t1[is.na(t1)]<-0
	if(sorted==TRUE) {v1<-names(sort(apply(t1,2,sum), decreasing=T)); t1<-t1[,v1]}
	if(proportion==TRUE) {t1 <- prop.table(t1,2); y_title = paste("prop of", rename_var(Var)); t1[is.na(t1)]<-0}
	
		# # colour
		# if(is.na(graph_par$col)) 
			# {
			# colour_scale <- rainbow(n = nrow(t1))
			# } else {
					# colour_table<-read.table("aux_colours.txt", header=T)
					# if(graph_par$col=="colour1") colour_scale<-colour_table$colour1[match(names(t1),a$Country)]
					# if(graph_par$col=="colour2") colour_scale<-colour_table$colour2[match(names(t1),a$Country)]
					# }

	# colour
		if(is.na(graph_par$col) | nrow(t1)>24 | (!is.na(graph_par$col) & nrow(t1) > length(unique(colour_table[[graph_par$col]])))) # 24 is the number of distinct colours in KBH palette 
			{
			colour_scale <- rainbow(n = nrow(t1))
			} else {
					if(var2 %in% c("LandingCountry", "FlagCountry"))
						{
						colour_scale<-colour_table[[graph_par$col]][match(rownames(t1),colour_table$Country)]
						} else {
								colour_scale<-unique(colour_table[[graph_par$col]])
								}
					}			
		if(nrow(t1)>length(colour_scale)) stop ("check colours")	
					
	#barplot(t1, las=2, legend.text=rownames(t1), col=rainbow(n = nrow(t1)), ylab = "", main = paste(Var,"by", var1, "and", var2), cex.names = graph_par$cex.x)
	#windows(10,5); par(cex=0.8, oma = graph_par$oma, mai = graph_par$mai)

	# bar.coords<-barplot(t1, plot=F) # required for legend
	# legend_pars<-eval(parse(text=legend_par)) # required for legend
	# #if(proportion==TRUE){legend_pars$y=1} # required for legend
	# barplot(t1, las=2, legend.text=rownames(t1), col=colour_scale, ylab = "", main = NULL, cex.names = graph_par$cex.x, args.legend = legend_pars)
	# if(title_root!="") title(main = paste(title_root,":",Var,"by", var1, "and", var2), line = 1.8) else title(main = paste(Var,"by", var1, "and", var2), line = 1.8)
	# title(ylab=y_title, line = graph_par$ylab_line)
	# if(!type_of_threshold == "NULL")
	# 	{
	# 	title(main=paste("y:", percent_Var,"%; x:",percent_var1,"%; z:",percent_var2,"%; ",type_of_threshold,"(",value_of_threshold,")", sep=""), cex.main=0.9, line = .7)
	# 	} else {
	# 		title(main=paste("y:", percent_Var,"%; x:",percent_var1,"%; z:",percent_var2,"%; ","all_data", sep=""), cex.main=0.9, line = .7)
	# 			}

	if(save_plot_to_list)
	{
	  
    windows(9,5)
	  dev.control("enable")
	  var1<-rename_var1(var1)
	  Var<-rename_var(Var)
	  var2<-rename_var2(var2)
	  if(grouped==TRUE) par(cex=0.8, mai = graph_par$mai) else  par(cex=0.8, oma = graph_par$oma, mai = graph_par$mai)	
	  
	  legend_par <- str_replace(legend_par, "cex=0.7", "cex=0.7, bty = \"n\"")
	  
	  bar.coords<-barplot(t1, plot=F) # required for legend
	  legend_pars<-eval(parse(text=legend_par)) # required for legend
	  #if(proportion==TRUE){legend_pars$y=1} # required for legend
	  barplot(t1, las=2, legend.text=rownames(t1), col=colour_scale, ylab = "", main = NULL, cex.names = graph_par$cex.x, args.legend = legend_pars)
	  if(title_root!="") title(main = paste(title_root,":",Var,"by", var1, "and", var2), line = 1.8) else title(main = paste(Var,"by", var1, "and", var2), line = 1.8)
	  title(ylab=y_title, line = graph_par$ylab_line)
	  if(filter=="small pelagic" || filter=="flatfish"|| filter=="demersal"){
	    #catch group
	    if(!type_of_threshold == "NULL")
	    {
	      if(type_of_threshold == "cum_percent" & percent_var1==100 & percent_var2==100 & percent_Var==100 & proportion==TRUE ){
	        caption<-paste(Var, ' of ',filter,' by ', var1, ' and ', var2,'. ', var1,' and ', var2,' displayed comprise ',value_of_threshold, '% of the total ',Var,'.' )
	      }
	      title(main=paste("y:", percent_Var,"%; x:",percent_var1,"%; z:",percent_var2,"%; ",type_of_threshold,"(",value_of_threshold,")", sep=""), cex.main=0.9, line = .7)
	      ########
	      if(type_of_threshold == "main" & percent_var1==100 & percent_var2==100 & percent_Var==100 & proportion==TRUE ){
	        caption<-paste(Var, ' of ',filter,' by ',value_of_threshold, 'main', var1,' and ', var2,'.')
	      }
	      if(type_of_threshold == "main" & percent_var1!=100 & percent_var2==100 & percent_Var==100 & proportion==TRUE ){
	        var1<-rename_var1(var1)
	        Var<-rename_var(Var)
	        var2<-rename_var2(var2)
	        caption<-paste(Var, ' of ',filter,' in the ',value_of_threshold, 'main', var1,'(',percent_var1, '% of rows entering analysis had known ',var1,') and ', var2,'.')
	      }
	      title(main=paste("y:", percent_Var,"%; x:",percent_var1,"%; z:",percent_var2,"%; ",type_of_threshold,"(",value_of_threshold,")", sep=""), cex.main=0.9, line = .7)
	      
	      ##########
	    } else {
	      title(main=paste("y:", percent_Var,"%; x:",percent_var1,"%; z:",percent_var2,"%; ","all_data", sep=""), cex.main=0.9, line = .7)
	      if(percent_var1==100 & percent_var2==100 & percent_Var==100 & proportion==TRUE ){      
	        caption<-paste(Var, ' of ',filter,' by ',var1,' and ', var2, '.',sep="")}
	      else{
	        if(percent_var1==100 & percent_var2!=100 & percent_Var==100 & proportion==TRUE ){
	          caption<-paste(Var, ' of ',filter,' by ', var1,' and ',var2,'. The',var2,' was given for ', percent_var2, '% of observations.',sep='')
	        }
	        else{
	          caption<-paste(Var, ' of ',filter,' by ', var1,' and ',var2,'. The',var1,' was given for ', percent_var1, '% of observations.', sep='')
	        }
	      }
	    }
	    #catch end
	  }else{
	  if(!type_of_threshold == "NULL")
	  {
	    if(type_of_threshold == "cum_percent" & percent_var1==100 & percent_var2==100 & percent_Var==100 & proportion==TRUE ){
	      caption<-paste(Var,' by ', var1, ' and ', var2,'. ', var1,' and ', var2,' displayed comprise ',value_of_threshold, '% of the total ',Var,'.' )
	    }
	    title(main=paste("y:", percent_Var,"%; x:",percent_var1,"%; z:",percent_var2,"%; ",type_of_threshold,"(",value_of_threshold,")", sep=""), cex.main=0.9, line = .7)
	    ########
	    if(type_of_threshold == "main" & percent_var1==100 & percent_var2==100 & percent_Var==100 & proportion==TRUE ){
	      caption<-paste(Var,' by ',value_of_threshold, 'main', var1,' and ', var2,'.')
	    }
	    if(type_of_threshold == "main" & percent_var1!=100 & percent_var2==100 & percent_Var==100 & proportion==TRUE ){
	      var1<-rename_var1(var1)
	      Var<-rename_var(Var)
	      var2<-rename_var2(var2)
	      caption<-paste(Var,' in the ',value_of_threshold, 'main', var1,'(',percent_var1, '% of rows entering analysis had known ',var1,') and ', var2,'.')
	    }
	    title(main=paste("y:", percent_Var,"%; x:",percent_var1,"%; z:",percent_var2,"%; ",type_of_threshold,"(",value_of_threshold,")", sep=""), cex.main=0.9, line = .7)
	    
	    ##########
	  } else {
	    title(main=paste("y:", percent_Var,"%; x:",percent_var1,"%; z:",percent_var2,"%; ","all_data", sep=""), cex.main=0.9, line = .7)
	    if(percent_var1==100 & percent_var2==100 & percent_Var==100 & proportion==TRUE ){      
	      caption<-paste(Var,' by ',var1,' and ', var2, '.',sep="")}
	    else{
	      if(percent_var1==100 & percent_var2!=100 & percent_Var==100 & proportion==TRUE ){
	        caption<-paste(Var,' by ', var1,' and ',var2,'. The',var2,' was given for ', percent_var2, '% of observations.',sep='')
	      }
	      else{
	        caption<-paste(Var,' by ', var1,' and ',var2,'. The',var1,' was given for ', percent_var1, '% of observations.', sep='')
	      }
	    }
	  }}
	  p <- recordPlot()
	  out<-list(table = data.frame(var1 = rownames(t1), Var = t1, row.names=NULL), plot = p, caption=caption)
	  dev.off()
	} else {
	  if(grouped==TRUE) par(cex=0.8, mai = graph_par$mai) else  par(cex=0.8, oma = graph_par$oma, mai = graph_par$mai)	
	  bar.coords<-barplot(t1, plot=F) # required for legend
	  legend_pars<-eval(parse(text=legend_par)) # required for legend
	  #if(proportion==TRUE){legend_pars$y=1} # required for legend
	  barplot(t1, las=2, legend.text=rownames(t1), col=colour_scale, ylab = "", main = NULL, cex.names = graph_par$cex.x, args.legend = legend_pars)
	  if(title_root!="") title(main = paste(title_root,":",Var,"by", var1, "and", var2), line = 1.8) else title(main = paste(Var,"by", var1, "and", var2), line = 1.8)
	  title(ylab=y_title, line = graph_par$ylab_line)
	  if(!type_of_threshold == "NULL")
	  {
	    title(main=paste("y:", percent_Var,"%; x:",percent_var1,"%; z:",percent_var2,"%; ",type_of_threshold,"(",value_of_threshold,")", sep=""), cex.main=0.9, line = .7)
	  } else {
	    title(main=paste("y:", percent_Var,"%; x:",percent_var1,"%; z:",percent_var2,"%; ","all_data", sep=""), cex.main=0.9, line = .7)
	  }
	  out<-list(table = data.frame(var1 = rownames(t1), Var = t1, row.names=NULL), plot = NULL,caption=NULL)
	  out
	}
	out
}

	
