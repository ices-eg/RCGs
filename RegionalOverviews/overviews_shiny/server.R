##--------------
## libraries
##--------------






##--------------
## data
##--------------

#load("data/ShinyTest_BigPicture.RData")
#load("data/inventory_ca.RData")
#load("data/graph_det.RData")



#setwd("C:/Users/Win10 Home x64/Desktop/Arbeitszeug/RCG_ISSG/RCGs/RegionalOverviews/overviews_shiny")

#group <- c("SamplingCountry","FlagCountry","LandingCountry","Year",                
           #"Quarter","Species","Area","SamplingType",        
          # "Metier","StatisticalRectangle","lat","lon") 

##--------------
## Fix a color for each country
##--------------




##--------------
## read functions
##--------------

#source("funs/pointsMap_func.r")	
#source("funs/choroplethMap_func.R")
#source("funs/func_barplot_var_by_one_var.r")	


##--------------
## Mapping
##--------------

#world <- ne_countries(scale = "medium", returnclass = "sf")

##--------------
## server
##--------------

server <- function(input, output, session){
   
   options(shiny.maxRequestSize = 500*1024^2)
   # *********************
   # define the report format of the output 
   # *********************
   
   output$report <- downloadHandler(
      filename = function() {
         paste("disclaimer_", Sys.Date(), ".pdf", sep='')
      },
      
      content = function(file) {
         file.copy("www/01_hello.pdf", file)
      }
   )
# old version for a .docx variante            
#      filename = function() {
#         paste('report_', Sys.Date(), '.docx', sep='')
#      },						
#      
#      content = function(file) {
#         x <- read_docx()
#         print(x, target = file)
#      }
#   )
   
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
      
      slinventory<-sl_master[,.(NoLength=sum(NoInSubSample),NoLengthTrips=length(unique(Trip[NoInSubSample>0])),WeigthKg=sum(SubSampleWeight_kg)),by=c("Year","Region","FlagCountry","LandingCountry","Stock","Species","SamplingType","StartQuarter","Area" ,"FishingActivityCategoryEuropeanLvl6", "CatchCategory")][NoLength>0|NoLengthTrips>0,]
      
      slinventory$Region[slinventory$Region=="NA"|is.na(slinventory$Region)]<-'NATL'
      
      slinventory$FlagCountry<-as.factor(slinventory$FlagCountry)
      slinventory$LandingCountry<-as.factor(slinventory$LandingCountry)
      slinventory$Region<-as.factor(slinventory$Region)
      slinventory$Stock<-as.factor(slinventory$Stock)
      slinventory$Species<-as.factor(slinventory$Species)
      slinventory$SamplingType<-as.factor(slinventory$SamplingType)
      slinventory$StartQuarter<-as.factor(slinventory$StartQuarter)
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

   
   
   
 
   
   # *********************
   # Tab "with functions"
   # *********************
   
   # -----------------------------------
   # Reactive variables 
   # -----------------------------------
   
   #vars <- reactive({str_remove(group, input$group)})
   
   #output$listvars <- renderUI({
    #  facet <- vars()
    #  selectInput ("facet", "Facet", facet,  multiple = F)
   #})
   
   # -----------------------------------
   # Plots
   # -----------------------------------
   
   
   #output$plot1<- renderPlot({
      
    #  if (input$view == 0) return(invisible(NULL))
     # isolate(
      #   if(input$plottype == "Map"){
            
       #     pointsMap_func (df= inventory_ca,
        #                    var= input$var,
         #                   groupBy= input$group,
          #                  facet = input$facet,
           #                 func = 'sum',
            #                points_coord = inventory_ca)
      #   }else{
       #     if(input$plottype == "Barplot"){
        #       
         #      barplot_var_by_one_var(x = as.data.frame(inventory_ca),
          #                            Var = input$var,
           #                           var1 = input$group,
            #                          tapply_type = "sum",
             #                         type_of_threshold="cum_percent",
              #                        value_of_threshold=100,
               #                       graph_par = eval(parse(text=graph_det$graph_par[1])))
            #}
        # }
     #)
  # })
   
   # ******************
   # Tab "with leaflet"
   # ******************
   
      output$absolute <- renderUI({
         req(input$file)
         absolutePanel(
            id = "controls",
            class = "panel panel-default",
            fixed = TRUE,
            draggable = TRUE,
            bottom = "auto",
            left = "auto",
            right = 20,
            top = 100,
            width = 450,
            height = "auto",
            h2("Sampling explorer"),
            # uiOutput("countryui"),uiOutput("speciesui"),uiOutput("samptypeui"),uiOutput("quarterui"),
            selectizeInput(
               "country",
               "Country",
               choices =
                  c("All", levels(data_list()[[3]]$LandingCountry)),
               multiple = TRUE,
               selected = "All",
               options = list(plugins = list("remove_button", "drag_drop"))
            ),
            selectizeInput(
               "species",
               "Species",
               choices =
                  c("All", levels(data_list()[[3]]$Species)),
               multiple = TRUE,
               selected = "All",
               options = list(plugins = list("remove_button", "drag_drop"))
            ),
            selectizeInput(
               "samtype",
               "Sampling Type",
               choices =
                  c("All", levels(data_list()[[3]]$SamplingType)),
               multiple = TRUE,
               selected = "All",
               options = list(plugins = list("remove_button", "drag_drop"))
            ),
            selectizeInput(
               "quarter",
               "Quarter",
               choices =
                  c("All", levels(data_list()[[3]]$Quarter)),
               multiple = TRUE,
               selected = "All",
               options = list(plugins = list("remove_button", "drag_drop"))
            ),
            
            selectInput ("N_var2", "Variable", var, multiple = F),
            checkboxInput("rec", "ICES Rectangles"),
            br(),
            actionButton ("view2", "View"),
            downloadButton("down", "Generate report"),
            br(),
            hr(),
            br(),
            br(),
            plotOutput("plot2", height = 300)
         )
         
         
         
      })
   # output ca_map
#    output$countryui<-renderUI({selectizeInput(
#       "country",
#       "Country",choices=
#       c("All", levels(data_list()[[3]]$LandingCountry)),
#       multiple = TRUE,
#       selected = "All",
#       options = list(plugins = list("remove_button", "drag_drop"))
#    )})
# 
#    output$speciesui<-renderUI({
# selectizeInput(
#    "species",
#    "Species",choices=
#    c("All", levels(data_list()[[3]]$Species)),
#    multiple = TRUE,
#    selected = "All",
#    options = list(plugins = list("remove_button", "drag_drop"))
# )})
# 
#   output$samptypeui<-renderUI({
# selectizeInput(
#    "samtype",
#    "Sampling Type",choices=
#    c("All", levels(data_list()[[3]]$SamplingType)),
#    multiple = TRUE,
#    selected = "All",
#    options = list(plugins = list("remove_button", "drag_drop"))
# )})
#   
#   output$quarterui<- renderUI({
# selectizeInput(
#    "quarter",
#    "Quarter",choices=
#    c("All", levels(data_list()[[3]]$Quarter)),
#    multiple = TRUE,
#    selected = "All",
#    options = list(plugins = list("remove_button", "drag_drop"))
# )})


   
   
   
   # -----------------------------------
   # Filtered data
   # -----------------------------------
   
   df <- reactive({

      data<-data_list()[[3]]
      data<-as.data.frame(data)

      if (!("All" %in% input$country)){
         data <- data[data$LandingCountry %in% input$country,]
      }
      if (!("All" %in% input$species )){
         data <- data[data$Species %in% input$species,]
      }
      if (!("All" %in% input$samtype)){
         data <- data[data$SamplingType %in% input$samtype,]
      }
      if (!("All" %in% input$quarter)){
         data <- data[data$Quarter %in% input$quarter,]
      }

      data <- data[, c("LandingCountry", "Quarter",  "Species", "SamplingType",
                       "lat", "lon", input$N_var2)]
      names(data) <- c("LandingCountry", "Quarter",  "Species", "SamplingType",
                       "lat", "lon", "aux")
      data
   })

   # # --------------------------------------
   # # Data aggregation at location 
   # # filtered data updated with view button
   # # --------------------------------------
   # 
   filter_df <- eventReactive(input$view2, {
      if(nrow(df())!=0){
         dat<-aggregate(list(aux = df()$aux),
                         by = list(lat = df()$lat, lon = df()$lon),
                                   # LandingCountry = df()$LandingCountry,
                                   # Species = df()$Species,
                                   # SamplingType = df()$SamplingType,
                                   # Quarter = df()$Quarter),
                         FUN = sum)
      }
      else {
         dat <- df()
      }
   })
   # 
   
   # -----------------------------------
   # Debugging
   # -----------------------------------
   
   # output$debug <- renderTable({
   # #head(df(), 5)
   #   head(filter_df(), 5)
   # })
   
   
   # -----------------------------------
   # leaflet map and barplots
   # -----------------------------------
   
   
   
   #input$view2ColorsBAR <- colour_table$colour4
  ColorsBAR <- colour_table$colour4
  names(ColorsBAR) <- colour_table$Country
  colScaleBAR<-scale_fill_manual(name="LandingCountry", values=ColorsBAR)
  
  pal.rmd<-reactive({colorNumeric ("viridis", domain = as.numeric(filter_df()$aux))})
  
  
  output$down <- downloadHandler(
     # For PDF output, change this to "report.pdf"
     filename = paste("report",Sys.Date(),".html",sep=''),
     
     content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        # 
        
        
        # Set up parameters to pass to Rmd document
        params <- list(c = input$country, s = input$species,
                       t = input$samtype, q = input$quarter,
                       v = input$N_var2, data1 = df(),data2 = filter_df(),
                       f = colScaleBAR,
                       p = pal.rmd())
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
     }
  )
  
  # -----------------------------------
  # leaflet map and barplots
  # -----------------------------------
  
   
   
   output$map <- renderLeaflet({
      leaflet() %>% addProviderTiles(providers$CartoDB.Positron)  %>% 
         setView(lng = -5,lat =  52, zoom = 5)
   })
   
   
   
   
   
   
   # -----------------------------------
   # Add filtered data to map
   # -----------------------------------
   
   observeEvent(input$view2,{
      #if (input$plottype == "Map")
      pal<-colorNumeric ("viridis", domain = as.numeric(filter_df()$aux))
      
      leafletProxy("map", data = filter_df())%>% 
         clearMarkers()%>% clearControls() %>% addProviderTiles(providers$CartoDB.Positron) %>% 
         addCircleMarkers(color=~pal(aux),
                           stroke=F,
                           radius=~ (sqrt(sqrt(aux))+0.6),
                           fillOpacity=0.8)%>%
         addLegend( "bottomleft", pal=pal, values=~aux, title = input$N_var2,opacity = 0.8)
   })
   
   # -----------------------------------
   # Add ICES Rectangles Shapefile
   # -----------------------------------
   
   observe({
      proxy<-leafletProxy("map", data = filter_df())
      proxy%>%clearShapes()
      if (input$rec){
         proxy%>% addPolygons(data=ices.rect, weight=.4,fillOpacity = .1,color = 'grey',group = 'ices_squares',label = ~paste0(ICESNAME),highlight = highlightOptions(weight = 3, color = "red",
                  bringToFront = TRUE))
      }
   })
   
   # -----------------------------------
   # barplot to panel
   # -----------------------------------
   
   
   output$plot2 <- renderPlot ({
      #input$view2
     if (input$view2==0) return()
   
      #validate(need(input$plottype=="Barplot", message=FALSE))
      ColorsBAR <- colour_table$colour4
      names(ColorsBAR) <- colour_table$Country
      colScaleBAR<-scale_fill_manual(name="LandingCountry", values=ColorsBAR)
     isolate({ ggplot(df(), aes(x=LandingCountry, y=aux, fill=LandingCountry)) +
         geom_bar(stat="identity")+
         colScaleBAR +
         theme_bw()+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))+
         labs(y = input$N_var2)
     
       
      })
     
   })
   

   
   
    # ******************
   # Tab "with ggplot"
   # ******************
   
   # -----------------------------------
   # Filtered data
   # -----------------------------------
   
  # df3 <- reactive({
      
   #   data3<-inventory_ca
      
    #  if (!("All" %in% input$country3)){
    #     data3 <- data3[data3$LandingCountry %in% input$country3,]
     # }
   #   if (!("All" %in% input$species3 )){
    #     data3 <- data3[data3$Species %in% input$species3,]
    #  }
     # if (!("All" %in% input$samtype3)){
   #      data3 <- data3[data3$SamplingType %in% input$samtype3,]
   #   }
    #  if (!("All" %in% input$quarter3)){
     #    data3 <- data3[data3$Quarter %in% input$quarter3,]
   #   }
      
    #  data3 <- data3[, c("LandingCountry", "Quarter",  "Species", "SamplingType", 
     #                    "lat", "lon", input$N_var3)]
      #names(data3) <- c("LandingCountry", "Quarter",  "Species", "SamplingType", 
       #                 "lat", "lon", "aux")
      #data3
#   })
   
   # --------------------------------------
   # Data aggregation at location 
   # filtered data updated with view button
   # --------------------------------------
   
   # filter_df3 <- eventReactive(input$view3, {
   #    
   #    if(nrow(df3())!=0){
   #       
   #       dat3<-aggregate (list(aux = df3()$aux),
   #                        by = list(lat = df3()$lat, lon = df3()$lon,
   #                                  LandingCountry = df3()$LandingCountry,
   #                                  Species = df3()$Species,
   #                                  SamplingType = df3()$SamplingType,
   #                                  Quarter = df3()$Quarter),
   #                        FUN = sum) # not sure should be set TRUE
   #    }
   #    else {
   #       dat3 <- df3()
   #    }
   #    dat3
   #    
   # }) 
   
   # # -----------------------------------
   # # Debugging
   # # -----------------------------------
   # 
   # output$debug3 <- renderTable({
   #   #head(df3(), 5)
   #   head(filter_df3(), 5)
   # })
   
   
   # output$debug3 <- renderText({
   #   #head(df3(), 5)
   #   head(sqrt(as.numeric(input$N_var3))*2, 5)
   # })
   
   # -----------------------------------
   # Mapa
   # -----------------------------------
   
   # viridis is the default colour/fill scale for ordered factors
   
   # output$plot3 <- renderPlot ({
   #    
   #    if(nrow(filter_df3())==0) return(invisible(NULL))
   #    
   #    ggplot(data = world) + geom_sf(fill= "antiquewhite") +
   #       geom_point(data = filter_df3(), aes(x = lon, y = lat, colour = aux, size = aux)) +
   #       scale_colour_viridis() +
   #       #facet_grid (SamplingType~LandingCountry)+
   #       facet_wrap(~SamplingType)+
   #       coord_sf(crs = "+init=epsg:4326", xlim = c(-25, 5), ylim = c(43, 63), expand = FALSE) +
   #       xlab("Longitude") + ylab("Latitude") + labs(size=input$N_var3, colour=input$N_var3)+
   #       ggtitle("North East Atlantic") +
   #       theme(
   #          text = element_text(color = "#22211d"),
   #          plot.background = element_rect(fill = "#ffffff", color = NA),
   #          panel.background = element_rect(fill = "aliceblue", color = NA),
   #          legend.background = element_rect(fill = "#ffffff", color = NA),
   #          panel.border = element_rect(
   #             colour = "black",
   #             fill = NA,
   #             size = 1.5
   #          ),
   #          panel.grid.major = element_line(color = gray(.8), linetype ='dashed', size = 0.5)
   #       )
   # })
   
} # end of the server