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
		
		fleetreg[Event_Start_Date<=20180101 & Event_End_Date>=20180101,V2018:=20180101,]
		fleetreg[Event_Start_Date<=20170101 & Event_End_Date>=20170101,V2017:=20170101,]
		fleetreg[Event_Start_Date<=20160101 & Event_End_Date>=20160101,V2016:=20160101,]
		fleetreg[Event_Start_Date<=20150101 & Event_End_Date>=20150101,V2015:=20150101,]
		
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
	
		fleetreg<-as.data.table(rbind(df2018, df2017, df2016, df2015))
	
	
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
		
		
		