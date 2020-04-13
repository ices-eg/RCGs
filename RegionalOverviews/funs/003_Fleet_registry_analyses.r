#=====================
# Script that reads and produces a set of outputs from fleet register
# made for RCG NSEA 2018
# Nuno Prista
#=====================

	# 2019-04-10: Adapted to new (temporary) format 
	
	# Note: values are based on License indicator == "Y" and differ slightly from previous extractions of "Active Fleet" from websit

	# wishlist: 
		# combination of absolute and variation in script (no need for excel steps)
	
	
rm(list=ls())


# functions and packages
	require(data.table)
	require(xlsx)

FAZ_SEGMENTACAO_COMPRIMENTO_DCF2<-function(dados, coluna = "Loa"){
		# Nuno Prista, IPMA, Portugal
		# 2015
		# FAZ_SEGMENTACAO_COMPRIMENTO_DCF adaptada para baltico
		# instrucoes: 'dados' e uma data.frame; 'coluna' e uma string com o nome da coluna que contem os comprimentos a categorizar (por omissao denominada "Loa")
		
		# definido por Appendix III da Decisao da Comissao 2010/93/EU de 18 Dezembro 2009
			# 0-< 8 m 
			# 8-< 10 m 
			# 10-< 12 m
			# 12-< 18 m
			# 18-< 24 m
			# 24-< 40 m
			# 40 m and larger
		
		# 11-03-2015: optimizado com funcao "cut"
		# 16-04-2015: alteracao do nome da funcao e da coluna final para DCF (era PNAB2)
		# 16-04-2015: explicitado o include.lowest=T por forma a tornar mais claro a categorizacao efectuada na funcao

		dados[,coluna]<-as.numeric(as.character(dados[,coluna]))
		dados$SEG_DCF<-cut(dados[,coluna], breaks=c(0,8,10,12,18,24,40,200), labels=c("[0-8[","[8-10[","[10-12[","[12-18[","[18-24[","[24-40[","[40+["), include.lowest=T, right=F)
		dados
}

	
	fleetreg<-data.table()

	# reads data 20190410
		# based on full history
	
	dir_data<-"data\\fleet_reg\\"
		
	for (ctry in c("BEL","DEU","DNK","ESP","EST","FIN","FRA","GBR","IRL","LTU","LVA","NLD","POL","PRT","SWE"))
		{
		print(ctry)
		fleetreg<-rbind(fleetreg,fread(paste(dir_data, ctry,"_export_20190410_fixed.csv", sep = ""), sep = ";",stringsAsFactors=FALSE, verbose=FALSE))	
		}	

	# subsets data
		fleetreg<-fleetreg[License_Ind=="Y",]

		
	# creates status date date and combines data
	
		fleetreg$V2018<-0
		fleetreg$V2017<-0
		fleetreg$V2016<-0
		fleetreg$V2015<-0
		fleetreg$V2014<-0
		fleetreg$V2013<-0
		fleetreg$V2012<-0
		fleetreg$V2011<-0		
		fleetreg$V2010<-0		
		fleetreg$V2009<-0
		
		fleetreg[Event_Start_Date<=20180101 & Event_End_Date>=20180101,V2018:=20180101,]
		fleetreg[Event_Start_Date<=20170101 & Event_End_Date>=20170101,V2017:=20170101,]
		fleetreg[Event_Start_Date<=20160101 & Event_End_Date>=20160101,V2016:=20160101,]
		fleetreg[Event_Start_Date<=20150101 & Event_End_Date>=20150101,V2015:=20150101,]
		fleetreg[Event_Start_Date<=20140101 & Event_End_Date>=20140101,V2014:=20140101,]
		fleetreg[Event_Start_Date<=20130101 & Event_End_Date>=20130101,V2013:=20130101,]
		fleetreg[Event_Start_Date<=20120101 & Event_End_Date>=20150201,V2012:=20120101,]
		fleetreg[Event_Start_Date<=20110101 & Event_End_Date>=20150101,V2011:=20110101,]
		fleetreg[Event_Start_Date<=20100101 & Event_End_Date>=20151001,V2010:=20100101,]
		fleetreg[Event_Start_Date<=20090101 & Event_End_Date>=20150901,V2009:=20090101,]
	
		fleetreg$Status_date<-0
	
		target_cols<-c("Country_Code","CFR","Event_Code","Event_Start_Date","Event_End_Date","License_Ind","Registration_Nbr","Ext_Marking","Vessel_Name","Port_Code","Port_Name","IRCS_Code","IRCS","Vms_Code","Gear_Main_Code","Gear_Sec_Code","Loa","Lbp","Ton_Ref","Ton_Gt","Ton_Oth","Ton_Gts","Power_Main","Power_Aux","Hull_Material","Com_Year","Com_Month","Com_Day","Segment","Exp_Country","Exp_Type","Public_Aid_Code","Decision_Date","Decision_Seg_Code","Construction_Year","Construction_Place","Status_date")
	
		df2018<-as.data.frame(fleetreg[V2018==20180101,])
		df2018$Status_date<-20180101
		df2018<-df2018[target_cols]
			
		df2017<-as.data.frame(fleetreg[V2017==20170101,])
		df2017$Status_date<-20170101
		df2017<-df2017[target_cols]

		df2016<-as.data.frame(fleetreg[V2016==20160101,])
		df2016$Status_date<-20160101
		df2016<-df2016[target_cols]

		df2015<-as.data.frame(fleetreg[V2015==20150101,])
		df2015$Status_date<-20150101
		df2015<-df2015[target_cols]

		df2014<-as.data.frame(fleetreg[V2014==20140101,])
		df2014$Status_date<-20140101
		df2014<-df2014[target_cols]

		df2013<-as.data.frame(fleetreg[V2013==20130101,])
		df2013$Status_date<-20130101
		df2013<-df2013[target_cols]

		df2012<-as.data.frame(fleetreg[V2012==20120101,])
		df2012$Status_date<-20120101
		df2012<-df2012[target_cols]

		df2011<-as.data.frame(fleetreg[V2011==20110101,])
		df2011$Status_date<-20110101
		df2011<-df2011[target_cols]

		df2010<-as.data.frame(fleetreg[V2010==20100101,])
		df2010$Status_date<-20100101
		df2010<-df2010[target_cols]

		df2009<-as.data.frame(fleetreg[V2009==20090101,])
		df2009$Status_date<-20090101
		df2009<-df2009[target_cols]
		
		
		
		fleetreg<-as.data.table(rbind(df2018, df2017, df2016, df2015, df2014, df2013, df2012, df2011, df2010, df2009))
	
	
	# a few formats and quality checks
		fleetreg$Power_Main<-as.numeric(fleetreg$Power_Main)
		fleetreg$Ton_Gt<-as.numeric(fleetreg$Ton_Gt)
		fleetreg$Ton_Ref<-as.numeric(fleetreg$Ton_Ref)
		# QCA: should yield 0
		sum(is.na(fleetreg$Power_Main))
		sum(is.na(fleetreg$Ton_Gt))
		sum(is.na(fleetreg$Ton_Ref))
			# ATT: fix
				# justification: sum(!fleetreg$Ton_Gt==fleetreg$Ton_Ref, na.rm=T)
				fleetreg[is.na(fleetreg$Ton_Gt),"Ton_Gt"]<-fleetreg[is.na(fleetreg$Ton_Gt),"Ton_Ref"]
				sum(is.na(fleetreg$Ton_Gt))
	
	# adds Loa classes
		fleetreg<-FAZ_SEGMENTACAO_COMPRIMENTO_DCF2(dados = as.data.frame(fleetreg), coluna = "Loa")
		# QCA: should yield 0
		sum(is.na(fleetreg$SEG_DCF))	
	
	# annual table vessels
		res_fleetreg<-table(fleetreg$SEG_DCF, fleetreg$Country_Code, fleetreg$Status_date)
		res_fleetreg_pwr<-tapply(fleetreg$Power_Main, list(fleetreg$SEG_DCF,fleetreg$Country_Code, fleetreg$Status_date), sum)
		res_fleetreg_gt<-tapply(fleetreg$Ton_Gt, list(fleetreg$SEG_DCF,fleetreg$Country_Code, fleetreg$Status_date), sum)
	
		res_fleetreg_pwr[is.na(res_fleetreg_pwr)]<-0
		res_fleetreg_gt[is.na(res_fleetreg_gt)]<-0
	
	# table difference between years
		res_fleetreg_2017_2016<-res_fleetreg[,,2]-res_fleetreg[,,1]
		res_fleetreg_2018_2017<-res_fleetreg[,,3]-res_fleetreg[,,2]
	
		res_fleetreg_2017_2016_pwr<-round(res_fleetreg_pwr[,,2]-res_fleetreg_pwr[,,1])
		res_fleetreg_2018_2017_pwr<-round(res_fleetreg_pwr[,,3]-res_fleetreg_pwr[,,2])
	
		res_fleetreg_2017_2016_gt<-round(res_fleetreg_gt[,,2]-res_fleetreg_gt[,,1])
		res_fleetreg_2017_2016_gt<-round(res_fleetreg_gt[,,3]-res_fleetreg_gt[,,2])

	
	# save 
		write.xlsx(as.data.frame.matrix(res_fleetreg[,,1]), sheetName = "2016", file="outputs/RCG_NA/Annual_Overview/fleet_reg/res_fleetreg.xlsx")
		write.xlsx(as.data.frame.matrix(res_fleetreg[,,2]), sheetName = "2017", file="outputs/RCG_NA/Annual_Overview/fleet_reg/res_fleetreg.xlsx", append=T)
		write.xlsx(as.data.frame.matrix(res_fleetreg[,,3]), sheetName = "2018", file="outputs/RCG_NA/Annual_Overview/fleet_reg/res_fleetreg.xlsx", append=T)
		write.xlsx(as.data.frame.matrix(res_fleetreg_2017_2016), sheetName = "2017-2016", file="outputs/RCG_NA/Annual_Overview/fleet_reg/res_fleetreg.xlsx", append=T)
		write.xlsx(as.data.frame.matrix(res_fleetreg_2018_2017), sheetName = "2018-2017", file="outputs/RCG_NA/Annual_Overview/fleet_reg/res_fleetreg.xlsx", append=T)
	
		write.xlsx(as.data.frame.matrix(round(res_fleetreg_pwr[,,1]/1000,1)), sheetName = "2016", file="outputs/RCG_NA/Annual_Overview/fleet_reg/res_fleetreg_pwr.xlsx")
		write.xlsx(as.data.frame.matrix(round(res_fleetreg_pwr[,,2]/1000,1)), sheetName = "2017", file="outputs/RCG_NA/Annual_Overview/fleet_reg/res_fleetreg_pwr.xlsx", append=T)
		write.xlsx(as.data.frame.matrix(round(res_fleetreg_pwr[,,3]/1000,1)), sheetName = "2018", file="outputs/RCG_NA/Annual_Overview/fleet_reg/res_fleetreg_pwr.xlsx", append=T)
		write.xlsx(as.data.frame.matrix(round(res_fleetreg_2017_2016_pwr/1000,1)), sheetName = "2017-2016", file="outputs/RCG_NA/Annual_Overview/fleet_reg/res_fleetreg_pwr.xlsx", append=T)
		write.xlsx(as.data.frame.matrix(round(res_fleetreg_2018_2017_pwr/1000,1)), sheetName = "2018-2017", file="outputs/RCG_NA/Annual_Overview/fleet_reg/res_fleetreg_pwr.xlsx", append=T)

		write.xlsx(as.data.frame.matrix(round(res_fleetreg_gt[,,1]/1000,1)), sheetName = "2016", file="outputs/RCG_NA/Annual_Overview/fleet_reg/res_fleetreg_gt.xlsx")
		write.xlsx(as.data.frame.matrix(round(res_fleetreg_gt[,,2]/1000,1)), sheetName = "2017", file="outputs/RCG_NA/Annual_Overview/fleet_reg/res_fleetreg_gt.xlsx", append=T)
		write.xlsx(as.data.frame.matrix(round(res_fleetreg_gt[,,3]/1000,1)), sheetName = "2018", file="outputs/RCG_NA/Annual_Overview/fleet_reg/res_fleetreg_gt.xlsx", append=T)
		write.xlsx(as.data.frame.matrix(round(res_fleetreg_2017_2016_gt/1000,1)), sheetName = "2017-2016", file="outputs/RCG_NA/Annual_Overview/fleet_reg/res_fleetreg_gt.xlsx", append=T)
		write.xlsx(as.data.frame.matrix(round(res_fleetreg_2017_2016_gt/1000,1)), sheetName = "2018-2017", file="outputs/RCG_NA/Annual_Overview/fleet_reg/res_fleetreg_gt.xlsx", append=T)
		
	# Multiannual
		
		source("funs/func_barplot_var_by_one_var.r")	
		source("funs/func_barplot_var_by_two_var_stacked.r")
		graph_det_all <- read.table("graphical_parameters/RCG_NA/MultiAnnual_Overview/MultiAnnualOverview_RCG_NA_FleetRegister_Graphical_details.txt", sep="\t", stringsAsFactors=FALSE, header=T)

		
	
		for (Graph_group in unique(graph_det_all$Graph_group))
		{
		print(paste("Graph group:",Graph_group))		
		graph_det<-graph_det_all[graph_det_all$Graph_group==Graph_group,]
		
		# detects group
		if(nrow(graph_det)>1) {windows(10,10); par(oma = eval(parse(text=graph_det$graph_par))$oma, mfrow=eval(parse(text=graph_det$graph_par))$mfrow)}
		if(nrow(graph_det)==1) {windows(10,5)} # oma parameter is set inside the barplot function

		# runs graphs
		for (i in 1:nrow(graph_det))
		{
		print(i)
		df1<-fleetreg[fleetreg$Country_Code==graph_det$Country_Code[i],]
		if(graph_det$Graph_type[i]==1)
			{
			res<-barplot_var_by_one_var(x = as.data.frame(df1), Var = graph_det$Var[i] , var1 = graph_det$var1[i], tapply_type = graph_det$tapply_type[i], type_of_threshold = graph_det$type_of_threshold[i], value_of_threshold = graph_det$value_of_threshold[i], sorted=graph_det$sorted[i], graph_par = eval(parse(text=graph_det$graph_par[i])), grouped = graph_det$Grouped[i])
			savePlot(filename = paste(graph_det$png_dir[i], paste(graph_det$png_name[i],".png", sep=""), sep="/"), type="png")
			write.table(res, file = paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".txt", sep=""), sep="\t", dec=".", row.names = TRUE)
			write.xlsx(res, file = paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".xlsx", sep=""), sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE)
			#dev.off()
			}
		if(graph_det$Graph_type[i]==2)
			{
			res<-barplot_var_by_two_var_stacked(x = as.data.frame(df1), Var = graph_det$Var[i] , var1 = graph_det$var1[i], var2 = graph_det$var2[i], tapply_type = graph_det$tapply_type[i], proportion = graph_det$proportion[i], type_of_threshold = graph_det$type_of_threshold[i], value_of_threshold = graph_det$value_of_threshold[i], sorted=graph_det$sorted[i], graph_par = eval(parse(text=graph_det$graph_par[i])), legend_par = graph_det$legend_par, grouped = graph_det$Grouped[i], title_root=graph_det$Country_Code[i])
			savePlot(filename = paste(graph_det$png_dir[i], paste(graph_det$png_name[i],".png", sep=""), sep="/"), type="png")
			write.table(res, file = paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".txt", sep=""), sep="\t", dec=".", row.names = TRUE)
			write.xlsx(res, file = paste(paste(graph_det$txt_dir[i], graph_det$txt_name[i], sep="/"),".xlsx", sep=""), sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE)
			#dev.off()
			}			
		}
		
		graphics.off()
		}	
		
		
		
		
		
	