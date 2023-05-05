# ===========================================================================================#
#                                    Inventory tables                                        #   
#                                                                                            #
#                                         *                                                  #           
# This script uses pre-treated CS (Commercial fisheries sampling) tables(i.e. CA, HH, HL, SL,#
# TR) in order to generate summarized versions of these datasets and connects them in order  #
# to generate the list of datasets used in the application.                                  #
#                                                                                            #
# ===========================================================================================#

# ---------------------------------------------------------


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
  
  cainventory<-ca[,.( ## Generate a summarized version of ca by ...
    NumMaturityStageFish = sum(!is.na(MaturityStage)),
    NumMaturityStageTrips=length(unique(Trip[!is.na(MaturityStage)])),
    NumAgeFish=sum(!is.na(Age)),
    NumAgeTrips=length(unique(Trip[!is.na(Age)])),
    NumLengthFish=sum(!is.na(LengthClass)),
    NumLengthTrips=length(unique(Trip[!is.na(LengthClass)])),
    NumWeightFish=sum(!is.na(Weight)),
    NumWeightTrips=length(unique(Trip[!is.na(Weight)]))
    ),
    by=c( # ... a set of vars
      "Year",
      "Region",
      "FlagCountry",
      "LandingCountry",
      "Stock",
      "Species",
      "SamplingType",
      "Quarter",
      "CatchCategory",
      "Sex")] 
  
  # datatable wants factors for filter = 
  cainventory$Year <- as.factor(cainventory$Year)
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
  sl_master<-merge( # Generate a combination of sl and tr
    sl, # Providing information on the species caught 
    tr[,list( # Providing some additional information on the trip 
      CS_TripId, 
      VesselIdentifier, 
      SamplingCountry, 
      SamplingMethod, 
      VesselLengthCategory, 
      Harbour
      )], 
    by="CS_TripId", all.x=T
    )
    
  
  sl_master <- merge(sl_master, # Include the detailed information on the station
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
  
  tr_inventory <- sl_master %>% ## Check
    group_by(
      Year,
      StartQuarter, 
      Region,
      FlagCountry,
      LandingCountry,
      Stock,
      Species,
      SamplingType,
      Area,
      CatchCategory,
      VesselIdentifier,
      Harbour
    ) %>%
    filter(NoInSubSample>0) %>%
    dplyr::summarize(
      NumLengthFish=sum(NoInSubSample),
      NumLengthTrips=length(unique(Trip)),
      WeigthKg=sum(SubSampleWeight_kg)
    )
  
  #class(sl_master)
  
  
  slinventory<-sl_master[,.( ## Generate a summarized version of sl+tr+hh by ...
    NumLengthFish=sum(NoInSubSample),
    NumLengthTrips=length(unique(Trip[NoInSubSample>0])),
    WeigthKg=sum(SubSampleWeight_kg)
    ),
    by=c( # ... a set of vars
      "Year",
      "Region",
      "FlagCountry",
      "LandingCountry",
      "Stock",
      "Species",
      "SamplingType",
      "StartQuarter",
      "FishingGround",
      "Area",
      "FishingActivityCategoryEuropeanLvl6", 
      "CatchCategory",
      "VesselLengthCategory")][NumLengthFish>0|NumLengthTrips>0,]
  
  slinventory$Region[slinventory$Region=="NA"|is.na(slinventory$Region)]<-'NATL'
  
  slinventory$Year<-as.factor(slinventory$Year)
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
  

  # ca_map<-ca
  
  hl_master<- # Combine sl+tr+hh and hl in order to add length information
    merge(hl,
          sl_master[, list(
            Region,
            CS_TripId,
            CS_StationId,
            CS_SpeciesListId,
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
          by = c("CS_TripId", "CS_StationId", "CS_SpeciesListId"),
          all.x = T)
  
  hl_master <- dplyr::rename(hl_master, Stock = stock)
  
  cahl <- mutate(ca, 
                 CS_LengthId = NA,
                 CS_SpeciesListId = NA,
                 CS_StatioId = NA,
                 IndividualSex = NA,
                 SubSamplingCategory = NA,
                 NoAtLengthInSample = NA,
                 NoAtLengthInCatch = NA,
                 NoAtLengthInSample_ThousandIndiv = NA,
                 NoAtLengthInSample_MillionIndiv = NA) 
  
  ca_map <- rbind( # Finally combine ca and sl+tr+hh+hl 
    cahl, 
    hl_master, 
    fill = TRUE
    )

  #ca_map<-as.data.table(ca_map)
  ca_map<-ca_map[!(is.na(StatisticalRectangle)|StatisticalRectangle=='99u9'),]
  ca_map$lat<- ices.rect(ca_map$StatisticalRectangle)$lat
  ca_map$lon <- ices.rect(ca_map$StatisticalRectangle)$lon

  # 
  # ca_map<-ca_map[,.(NoMaturityStage=sum(!is.na(MaturityStage)),NoMaturityStageTrips=length(unique(Trip[!is.na(MaturityStage)])),NoAge=sum(!is.na(Age)),NoAgeTrips=length(unique(Trip[!is.na(Age)])),NoLength=sum(!is.na(LengthClass)),NoLengthTrips=length(unique(Trip[!is.na(LengthClass)])),NoWeight=sum(!is.na(Weight)),NoWeightTrips=length(unique(Trip[!is.na(Weight)]))),by=c("Region","LandingCountry","Species","SamplingType","Quarter","CatchCategory","lat","lon")]
  
  ca_map <- ca_map[,.( # Generate a summarized version of ca+sl+tr+hh+hl by ...
    NumMaturityStageFish=sum(!is.na(MaturityStage)),
    NumMaturityStageTrips=length(unique(Trip[!is.na(MaturityStage)])),
    NumAgeFish=sum(!is.na(Age)),NumAgeTrips=length(unique(Trip[!is.na(Age)])), 
    NumWeightFish=sum(!is.na(Weight)),
    NumWeightTrips=length(unique(Trip[!is.na(Weight)])), 
    NumLengthFish = sum(!is.na(NoAtLengthInSample)), 
    NumLengthTrips = length(unique(Trip[!is.na(NoAtLengthInSample)]))),
    by=c( # A set of vars. Note that this df is not grouped by year
      "Region",
      "LandingCountry",
      "Species",
      "SamplingType",
      "Quarter",
      "CatchCategory",
      "lat",
      "lon")]
  
  # 
  ca_map$Year<-as.factor(ca_map$Year)
  ca_map$SamplingType<-as.factor(ca_map$SamplingType)
  ca_map$Quarter<-as.factor(ca_map$Quarter)
  ca_map$LandingCountry<-as.factor(ca_map$LandingCountry)
  ca_map$Region<-as.factor(ca_map$Region)
  ca_map$Species<-as.factor(ca_map$Species)
  
  ca_map2<-ca
  ca_map2<-ca_map2[!(is.na(StatisticalRectangle)|StatisticalRectangle=='99u9'),]
  ca_map2$lat<- ices.rect(ca_map2$StatisticalRectangle)$lat
  ca_map2$lon <- ices.rect(ca_map2$StatisticalRectangle)$lon
  
  ca_map2<-ca_map2[,.( # Generate a summarized version of ca+sl+tr+hh+hl by ...
    NumMaturityStageFish=sum(!is.na(MaturityStage)),
    NumMaturityStageTrips=length(unique(Trip[!is.na(MaturityStage)])),
    NumAgeFish=sum(!is.na(Age)),NumAgeTrips=length(unique(Trip[!is.na(Age)])),
    NumLengthFish=sum(!is.na(LengthClass)),
    NumLengthTrips=length(unique(Trip[!is.na(LengthClass)])),
    NumWeightFish=sum(!is.na(Weight)),
    NumWeightTrips=length(unique(Trip[!is.na(Weight)]))),
    by=c( # A set of vars. Note that this df is grouped by year
      "Year",
      "Region",
      "FlagCountry",
      "Species",
      "SamplingType",
      "Quarter",
      "CatchCategory",
      "lat",
      "lon")]
  
  ca_map2$Year<-as.factor(ca_map2$Year)
  ca_map2$SamplingType<-as.factor(ca_map2$SamplingType)
  ca_map2$Quarter<-as.factor(ca_map2$Quarter)
  ca_map2$FlagCountry<-as.factor(ca_map2$FlagCountry)
  ca_map2$Region<-as.factor(ca_map2$Region)
  ca_map2$Species<-as.factor(ca_map2$Species)
  ca_map2$CatchCategory<-as.factor(ca_map2$CatchCategory)
  
  list1<-vector(mode = "list")
  list1[[1]]<-cainventory ## Summarized version of ca by "Year","Region","FlagCountry","LandingCountry","Stock","Species","SamplingType","Quarter","CatchCategory","Sex".
  list1[[2]]<-slinventory ## Summarized version of sl+tr+hh by "Year","Region","FlagCountry","LandingCountry","Stock","Species","SamplingType","StartQuarter","FishingGround","Area","FishingActivityCategoryEuropeanLvl6", "CatchCategory","VesselLengthCategory"
  list1[[3]]<-ca_map ## Summarized version of ca+sl+tr+hh+hl by "Region","LandingCountry","Species","SamplingType","Quarter","CatchCategory","lat","lon"
  list1[[4]]<-ca_map2 ## Summarized version of ca+sl+tr+hh+hl by "Year","Region","LandingCountry","Species","SamplingType","Quarter","CatchCategory","lat","lon"
  list1[[5]]<-tr_inventory ## Summarized version of sl+hh+tr by "Year","StartQuarter", "Region","FlagCountry","LandingCountry","Stock","Species","SamplingType","Area","CatchCategory","VesselIdentifier","Harbour"
  
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

# output for SL inventory
output$inventorytable_TR <- DT::renderDT(
  DT::datatable({data_list()[[5]]}, 
                options = list(
                  pageLength = 20,
                  autoWidth=T,
                  scrollX=TRUE
                  ),
                filter = 'top'
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
      write.csv(slinventory[input[["inventorytable_SL_rows_all"]], ], # replaced ca[input[["inventorytable_SL_rows_all"]], ] with slinventory[input[["inventorytable_SL_rows_all"]], ]
                file)
    }
  )

#download widget
output$download_filtered_inventorytable_TR <- 
  downloadHandler(
    filename = "tr_inventory_data.csv",
    content = function(file){
      write.csv(tr_inventory[input[["inventorytable_TR_rows_all"]], ],
                file)
    }
  )