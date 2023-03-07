for (ctry in c("BEL","DEU" ,"DNK","ESP","EST","FIN","FRA","IRL","LTU","LVA","NLD","POL","PRT","SWE")) # "GBR" - no recent data in EU Fleet Register 
 #"BEL","DEU" ,"DNK","ESP","EST","FIN","FRA","GBR","IRL","LTU","LVA","NLD","POL","PRT","SWE"
{
  #change year
eu_fleet_register<-read.csv2(paste("../input/2022/FLEET-",ctry,".csv",sep = ""), sep=",", header=T, row.names=NULL, as.is=T) # check the separator used (',' or ';')
names(eu_fleet_register)
names(eu_fleet_register)<-c("Country_Code","CFR","UVI","Event_Code","Event_Start_Date","Event_End_Date",
                            "Registration_Nbr","Ext_Marking","Vessel_Name","Port_Code","IRCS","IRCS_Code",
                            "License_Ind","Vms_Code","ERS_Code","AIS_Code","MMSI","Vessel_Type","Gear_Main_Code",
                            "Gear_1_Code","Gear_2_Code","Gear_3_Code","Gear_4_Code","Gear_5_Code","Loa","Lbp","Ton_Gt","Ton_Oth",
                            "Ton_Gts","Power_Main","Power_Aux","Hull_Material","Com_Date","Segment",
                            "Exp_Country","Exp_Type","Public_Aid_Code","Construction_Year")

#eu_fleet_register<-str_replace(eu_fleet_register,"[,]", "")
a<-eu_fleet_register$Event_Start_Date
a<- strptime(as.character(a), "%d/%m/%Y") # need to check the date format present in the file, to select the right one to put here (e.g. "%Y-%m-%d", "%d/%m/%Y", other)
a<- format(a, "%Y%m%d")
eu_fleet_register$Event_Start_Date<-as.numeric(a)

a<-eu_fleet_register$Event_End_Date
a<- strptime(as.character(a), "%d/%m/%Y") # same as for the 'Event_Start_Date'
a<- format(a, "%Y%m%d")
eu_fleet_register$Event_End_Date<-as.numeric(a)
#change year 
write.csv2(eu_fleet_register, file=paste("../output/2022/",ctry,"_export.csv", sep=""), row.names=F)	
}

