#===============#
#inventory table#
#===============#


# observe({
#    
#    hideTab(inputId="tabs", target="Sampling overview")
#    
# })

data_list<-reactive({
  
  #if (is.null(input$file)) return()      
  
  req(input$file)
  
  load(input$file$datapath, envir = .GlobalEnv)
  
  # modify the CS.Rdata
  ca<-as.data.table(ca)
  #ca<-fread(file)
  ca$Region[ca$Region=="NA"|is.na(ca$Region)]<-'NATL'
  
  cainventory<-ca[,.(NoMaturityStage=sum(!is.na(MaturityStage)),NoMaturityStageTrips=length(unique(Trip[!is.na(MaturityStage)])),NoAge=sum(!is.na(Age)),NoAgeTrips=length(unique(Trip[!is.na(Age)])),NoLength=sum(!is.na(LengthClass)),NoLengthTrips=length(unique(Trip[!is.na(LengthClass)])),NoWeight=sum(!is.na(Weight)),NoWeightTrips=length(unique(Trip[!is.na(Weight)]))),by=c("Year","Region","FlagCountry","LandingCountry","Stock","Species","SamplingType","Quarter","CatchCategory","Sex")]
  
  # datatable wants factors for filter = 
  cainventory$FlagCountry<-as.factor(cainventory$FlagCountry)
  cainventory$LandingCountry<-as.factor(cainventory$LandingCountry)
  cainventory$Region<-as.factor(cainventory$Region)
  cainventory$Stock<-as.factor(cainventory$Stock)
  cainventory$Species<-as.factor(cainventory$Species)
  cainventory$SamplingType<-as.factor(cainventory$SamplingType)
  cainventory$Quarter<-as.factor(cainventory$Quarter)
  cainventory$CatchCategory<-as.factor(cainventory$CatchCategory)
  cainventory$Sex<-as.factor(cainventory$Sex)
  
  
  hh$StartQuarter <- quarter(ymd(hh$StartDate))
  #sl<-as.data.table(sl)
  #tr<-as.data.table(tr)
  
  #preparing master table
  sl_master<-merge(sl, tr[,list(CS_TripId, VesselIdentifier, SamplingCountry, SamplingMethod, VesselLengthCategory)], by="CS_TripId", all.x=T)
  
  
  sl_master <-
    merge(sl_master,
          hh[, list(
            Region,
            CS_TripId,
            CS_StationId,
            StartDate,
            StartQuarter,
            FishingTime,
            PosStartLatDec,
            PosStartLonDec,
            PosStopLatDec,
            PosStopLonDec,
            Area,
            FishingGround,
            StatisticalRectangle,
            FishingActivityCategoryEuropeanLvl5,
            FishingActivityCategoryEuropeanLvl6,
            Gear
          )],
          by = c("CS_TripId", "CS_StationId"),
          all.x = T)
  
  
  #class(sl_master)
  
  slinventory<-sl_master[,.(NoLength=sum(NoInSubSample),NoLengthTrips=length(unique(Trip[NoInSubSample>0])),WeigthKg=sum(SubSampleWeight_kg)),by=c("Year","Region","FlagCountry","LandingCountry","Stock","Species","SamplingType","StartQuarter","FishingGround","Area" ,"FishingActivityCategoryEuropeanLvl6", "CatchCategory","VesselLengthCategory")][NoLength>0|NoLengthTrips>0,]
  
  slinventory$Region[slinventory$Region=="NA"|is.na(slinventory$Region)]<-'NATL'
  
  slinventory$FlagCountry<-as.factor(slinventory$FlagCountry)
  slinventory$LandingCountry<-as.factor(slinventory$LandingCountry)
  slinventory$Region<-as.factor(slinventory$Region)
  slinventory$Stock<-as.factor(slinventory$Stock)
  slinventory$Species<-as.factor(slinventory$Species)
  slinventory$SamplingType<-as.factor(slinventory$SamplingType)
  slinventory$StartQuarter<-as.factor(slinventory$StartQuarter)
  slinventory$FishingGround<-as.factor(slinventory$FishingGround)
  slinventory$Area<-as.factor(slinventory$Area)
  slinventory$CatchCategory<-as.factor(slinventory$CatchCategory)
  
  ca_map<-ca
  
  #ca_map<-as.data.table(ca_map)
  
  ca_map<-ca_map[!(is.na(StatisticalRectangle)|StatisticalRectangle=='99u9'),]
  
  
  # 
  ca_map$lat<- ices.rect(ca_map$StatisticalRectangle)$lat
  # 
  ca_map$lon <- ices.rect(ca_map$StatisticalRectangle)$lon
  # 
  ca_map<-ca_map[,.(NoMaturityStage=sum(!is.na(MaturityStage)),NoMaturityStageTrips=length(unique(Trip[!is.na(MaturityStage)])),NoAge=sum(!is.na(Age)),NoAgeTrips=length(unique(Trip[!is.na(Age)])),NoLength=sum(!is.na(LengthClass)),NoLengthTrips=length(unique(Trip[!is.na(LengthClass)])),NoWeight=sum(!is.na(Weight)),NoWeightTrips=length(unique(Trip[!is.na(Weight)]))),by=c("Region","LandingCountry","Species","SamplingType","Quarter","CatchCategory","lat","lon")]
  # 
  
  ca_map$SamplingType<-as.factor(ca_map$SamplingType)
  ca_map$Quarter<-as.factor(ca_map$Quarter)
  ca_map$LandingCountry<-as.factor(ca_map$LandingCountry)
  ca_map$Species<-as.factor(ca_map$Species)
  
  list1<-vector(mode = "list")
  list1[[1]]<-cainventory
  list1[[2]]<-slinventory
  list1[[3]]<-ca_map
  
  
  list1
  
  
})



# ca for mapping






#do the master table 




# output for CA inventory

output$inventorytable_CA <- DT::renderDT(DT::datatable({data_list()[[1]]
  
}

, options = list(
  pageLength = 20,autoWidth=T,scrollX=TRUE
),filter = 'top'
))

# output for SL inventory
output$inventorytable_SL <- DT::renderDT(DT::datatable({data_list()[[2]]}
                                                       
                                                       , options = list(
                                                         pageLength = 20,autoWidth=T,scrollX=TRUE
                                                       ),filter = 'top'
))


#download widget
output$download_filtered_inventorytable_CA <- 
  downloadHandler(
    filename = "ca_inventory_data.csv",
    content = function(file){
      write.csv(cainventory[input[["inventorytable_CA_rows_all"]], ],
                file)
    }
  )
#download widget
output$download_filtered_inventorytable_SL <- 
  downloadHandler(
    filename = "sl_inventory_data.csv",
    content = function(file){
      write.csv(ca[input[["inventorytable_SL_rows_all"]], ],
                file)
    }
  )



