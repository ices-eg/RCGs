# ******************
# Tab "with ggplot"
# ******************

# -------------------------
# Updating selectize input == country 
# -------------------------

dg <- reactive({
  
  #data<-data_list()[[3]]
  data<-data_list()[[4]]
  data<-as.data.frame(data)
  
  if (!("All" %in% input$regiong)){
    data <- data[data$Region == input$regiong,]
  }
  data<-data%>%left_join(spptable)
  data
})


observe({
  # Updating selectize input
  #updateSelectInput(session, "countryg", choices = unique(dg()$LandingCountry), selected = sort(unique(dg()$LandingCountry))[1]) 
  #updateSelectInput(session, "countryg", choices = c("All",as.character(unique(dg()$LandingCountry))), selected = "All")
  updateSelectInput(session, "countryg", choices = c("All",as.character(unique(dg()$FlagCountry))), selected = "All")
  
  })


# -------------------------
# Updating selectize input == species
# -------------------------

gars <- reactive ({
  
  data <- dg()
  
  if (!("All" %in% input$countryg)){
    #data <- data[data$LandingCountry %in% input$countryg,]
    data <- data[data$FlagCountry %in% input$countryg,]
  }
  
  data <- data[, input$sppNamechoiceg]
  data
})


observe({
  # Updating selectize input
  updateSelectInput(session, "speciesg", choices =c("All", as.character(unique(gars()))), selected = "All") 
})



output$static <- renderUI({
  req(input$file)
  
  fluidRow(
    br(), br(),
    column(4,    
           selectInput(
                "regiong",
                "Region",
                choices =
                #c("All", levels(data_list()[[3]]$Region)),
                c("All", levels(data_list()[[4]]$Region)),
              multiple = F,
            selected = "All"
            ), 
           selectizeInput (
             "countryg", 
             "Country",
             choices =
                c(""),
               #c("All", levels(data_list()[[3]]$LandingCountry)),
             multiple = TRUE,
             #selected = "All",
             options = list(plugins = list("remove_button", "drag_drop"))
           ),
           radioButtons(
             "sppNamechoiceg",
             "Species",
             choices = c("LatinName",
                         "EnglishName",
                         "Alpha3ID", 
                         "WoRMSAphiaID"
             )
           ),
           selectizeInput(
             "speciesg",
             "Species",
             choices = c(""),
              #c("All", levels(data_list()[[3]]$Species)),
             multiple = TRUE,
             #selected = "All",
             options = list(plugins = list("remove_button", "drag_drop"))
           ),
           selectizeInput(
             "samtypeg",
             "Sampling Type",
             choices =
               #c("All", levels(data_list()[[3]]$SamplingType)),
              c("All", levels(data_list()[[4]]$SamplingType)),
             multiple = TRUE,
             selected = "All",
             options = list(plugins = list("remove_button", "drag_drop"))
           ),
           selectizeInput(
             "quarterg",
             "Quarter",
             choices =
              #c("All", levels(data_list()[[3]]$Quarter)),
             c("All", levels(data_list()[[4]]$Quarter)),
             multiple = TRUE,
             selected = "All",
             options = list(plugins = list("remove_button", "drag_drop"))
           ),
           selectInput ("N_var3", "Variable", var, multiple = F),
           hr(),
           selectInput ("facetx", "Facet by", facetvar2, multiple = F),
           #selectInput ("facety", "Facet.y", facetvar, multiple = F),
           hr(),
           div(style="display: inline-block;vertical-align:top;",actionButton ("view3", "View")), 
           div(style="display: inline-block;vertical-align:top;",downloadButton ("down3", "Download plot")),
           div(style="display: inline-block;vertical-align:top;",circleButton ("help3",  icon = icon("info-circle"), size = "sm")),
           bsModal("modal", "Help", "help3", size = "large",
                   includeHTML ("data/DescriptionStaticmap.txt"))
    ), 
    column(8, 
           br(),br(),
           plotOutput("plot3", height = "700px", width = "900px"),
           tableOutput("debug3")
           #verbatimTextOutput("debug3")
    )
  )
  
})


# -----------------------------------
# Filtered data
# -----------------------------------
df3 <- reactive({
  
  #data3<-data_list()[[3]]
  data3<-data_list()[[4]]
  data3<-as.data.frame(data3)
  data3<-data3%>%left_join(spptable)
  
  if (!("All" %in% input$regiong)){
    data3 <- data3[data3$Region == input$regiong,]
  }
  if (!("All" %in% input$countryg)){
    #data3 <- data3[data3$LandingCountry %in% input$countryg,]
    data3 <- data3[data3$FlagCountry %in% input$countryg,]
  }
  if (!("All" %in% input$speciesg)){
  if(input$sppNamechoiceg == "LatinName"){
    data3 <- data3[data3$Species %in% input$speciesg,]
  }else{
  if(input$sppNamechoiceg == "EnglishName"){
    data3 <- data3[data3$EnglishName %in% input$speciesg,]
  }else{
  if(input$sppNamechoiceg == "Alpha3ID"){
    data3 <- data3[data3$Alpha3ID %in% input$speciesg,]
  }else{
  if(input$sppNamechoiceg == "WoRMSAphiaID"){
    data3 <- data3[data3$WoRMSAphiaID %in% input$speciesg,]
    }
  }}}}
  if (!("All" %in% input$samtypeg)){
    data3 <- data3[data3$SamplingType %in% input$samtypeg,]
  }
  if (!("All" %in% input$quarterg)){
    data3 <- data3[data3$Quarter %in% input$quarterg,]
  }
  
  # data3 <- data3[, c("LandingCountry", "Quarter",  "Species", "SamplingType",
  #                    "lat", "lon", input$N_var3)]
  data3 <- data3[, c("FlagCountry", "Quarter",  "Species", "SamplingType",
                     "lat", "lon", input$N_var3)]
  # names(data3) <- c("LandingCountry", "Quarter",  "Species", "SamplingType",
  #                   "lat", "lon", "aux")
  names(data3) <- c("FlagCountry", "Quarter",  "Species", "SamplingType",
                    "lat", "lon", "aux")
  data3
})

# --------------------------------------
# Data aggregation at location 
# filtered data updated with view button
# --------------------------------------

filter_df3 <- eventReactive(input$view3, {
  if(nrow(df3())!=0){
    # dat3<-aggregate(list(aux = df3()$aux),
    #                by = list(lat = df3()$lat, lon = df3()$lon,
    #                       LandingCountry = df3()$LandingCountry,
    #                       Species = df3()$Species,
    #                       SamplingType = df3()$SamplingType,
    #                       Quarter = df3()$Quarter),
    #                FUN = sum)
    
    dat3 <- df3 ()[, c("lat", "lon", input$facetx, "aux")] 
    names(dat3) <- c("lat", "lon", "facet", "aux") 
    dat3 <- dat3 %>% group_by(lat, lon, facet) %>% # input$ not working in group_by
      summarise(aux=sum(aux))
  }
  else {
    dat3 <- df3()
  }
})

# # -----------------------------------
# # Debugging
# # -----------------------------------
# 
 # output$debug3 <- renderTable({
 #   #range(filter_df3()[filter_df3()$lat & filter_df3()$lon,]$lon)+ c(-1, 1)
 #    sum(filter_df3()$aux)
 # })


# output$debug3 <- renderText({
#   #head(df3(), 5)
#   head(sqrt(as.numeric(input$N_var3))*2, 5)
# })



# -----------------------------------
# Mapa
# -----------------------------------

#Reactives 

# viridis is the default colour/fill scale for ordered factors

    output$plot3 <- renderPlot ({
      
      input$view3
      
      isolate({
        
        if(nrow(filter_df3()) == 0 |sum(filter_df3()$aux) == 0) return({shinyalert("Oops!", "No data for this selection", type = "error", size = "xs")})
      
        ggplot(data = world) + geom_sf(fill= "antiquewhite") +
          geom_point(data = filter_df3(), aes(x = lon, y = lat, color = aux, size = aux)) +
          #scale_colour_viridis(guide ="legend") +
          scale_color_viridis(guide ="legend", limits = c(min(filter_df3()$aux), roundUpNice(max(filter_df3()$aux)))) +
          scale_size_continuous (limits = c(min(filter_df3()$aux), roundUpNice(max(filter_df3()$aux))))+
          #facet_grid (SamplingType~LandingCountry)+
          facet_wrap(~facet)+
          coord_sf(crs = "+init=epsg:4326",
                   xlim = range(filter_df3()[filter_df3()$lat & filter_df3()$lon,]$lon)+ c(-1, 1),
                   ylim = range(filter_df3()[filter_df3()$lat & filter_df3()$lon,]$lat) + c(-0.5,+0.5),
                   expand = FALSE) +
          xlab("Longitude") + ylab("Latitude") + labs(size=input$N_var3, colour=input$N_var3)+
          ggtitle(paste("Region:",input$regiong,"- Species:", input$speciesg, "- Sampling type:",input$samtypeg,"- Quarter:", input$quarterg, sep = " ")) +
          theme(
            plot.title = element_text(size =20,hjust = 0.5),
            text = element_text(color = "#22211d"),
            plot.background = element_rect(fill = "#ffffff", color = NA),
            panel.background = element_rect(fill = "aliceblue", color = NA),
            legend.background = element_rect(fill = "#ffffff", color = NA),
            panel.border = element_rect(
              colour = "black",
              fill = NA,
              size = 1.5
            ),
            panel.grid.major = element_line(color = gray(.8), linetype ='dashed', size = 0.5)
          )

      }) 
    })





# -----------------------------------
# Download plot 
# -----------------------------------


output$down3 <- downloadHandler(
 
   filename ="static_plot.png",
  
  content = function(static.plot){
    png(static.plot)  
    
    if(nrow(filter_df3()) == 0 |sum(filter_df3()$aux) == 0) return(invisible(NULL))
    
    static.plot <-ggplot(data = world) + geom_sf(fill= "antiquewhite") +
      geom_point(data = filter_df3(), aes(x = lon, y = lat, color = aux, size = aux)) +
      #scale_colour_viridis(guide ="legend") +
      scale_color_viridis(guide ="legend", limits = c(min(filter_df3()$aux), roundUpNice(max(filter_df3()$aux)))) +
      scale_size_continuous (limits = c(min(filter_df3()$aux), roundUpNice(max(filter_df3()$aux))))+
      #facet_grid (SamplingType~LandingCountry)+
      facet_wrap(~facet)+
      coord_sf(crs = "+init=epsg:4326",
               xlim = range(filter_df3()[filter_df3()$lat & filter_df3()$lon,]$lon)+ c(-1, 1),
               ylim = range(filter_df3()[filter_df3()$lat & filter_df3()$lon,]$lat) + c(-0.5,+0.5),
               expand = FALSE) +
      xlab("Longitude") + ylab("Latitude") + labs(size=input$N_var3, colour=input$N_var3)+
      ggtitle(paste("Region:",input$regiong,"- Species:", input$speciesg, "- Sampling type:",input$samtypeg,"- Quarter:", input$quarterg, sep = " ")) +
      theme(
        plot.title = element_text(size =20,hjust = 0.5),
        text = element_text(color = "#22211d"),
        plot.background = element_rect(fill = "#ffffff", color = NA),
        panel.background = element_rect(fill = "aliceblue", color = NA),
        legend.background = element_rect(fill = "#ffffff", color = NA),
        panel.border = element_rect(
          colour = "black",
          fill = NA,
          size = 1.5
        ),
        panel.grid.major = element_line(color = gray(.8), linetype ='dashed', size = 0.5)
      )
    print(static.plot)
    dev.off()
  }
)