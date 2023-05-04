# =========================================================#
#                    Leaflet interactive map               #   
#                                                          #
#                              *                           #           
#                                                          #
# This script filters the data in order to display them on # 
# a leaflet interactive map according to user´s selection. #
# =========================================================#

# ---------------------------------------------------------
# Updating selectize input == country 
# ---------------------------------------------------------

dd <- reactive({
  
  #data<-data_list()[[3]]
  data<-data_list()[[4]]
  data<-as.data.frame(data)
  
  if (!("All" %in% input$region)){
    data <- data[data$Region == input$region,]
  }
  data<-data%>%left_join(spptable)
  data
})

observe({
  #updateSelectInput(session, "country", choices = unique(dd()$LandingCountry), selected = sort(unique(dd()$LandingCountry))[1]) 
  #updateSelectInput(session, "country", choices = c("All",as.character(unique(dd()$LandingCountry))), selected = "All") 
  updateSelectInput(session, "country", choices = c("All",as.character(unique(dd()$FlagCountry))), selected = "All") 
  
  })

# ---------------------------------------------------------
# Updating selectize input == species
# ---------------------------------------------------------

vars <- reactive ({
  
  data <- dd()
  
  if (!("All" %in% input$country)){
    #data <- data[data$LandingCountry %in% input$country,]
    data <- data[data$FlagCountry %in% input$country,]
  }

  data <- data[, input$sppNamechoice]
  data
})


observe({
  # Updating selectize input
  updateSelectInput(session, "species", choices =c("All", as.character(unique(vars()))), selected = "All") 
})

# ---------------------------------------------------------
# Updating selectize input : 
# Remove All when another value is selected !!!All needs to be selected initially for the code to work!!! 
# Remove other values when All is selected again 
# ---------------------------------------------------------
# Country 
selected <- reactiveValues(v = NULL)
observeEvent(input$country, {
  selected$v <- input$country 
  if(selected$v[1] %in% "All" & length(selected$v) > 1){
    newSelection <- subset(input$country, !input$country %in% "All")
    updateSelectizeInput(session, "country", choices = c("All",as.character(unique(dd()$FlagCountry))), selected = newSelection)
  }else if(!selected$v[1] %in% "All" & sum(str_detect(selected$v, "All")) %in% 1){
    newSelection <- subset(input$country, input$country %in% "All")
    updateSelectizeInput(session, "country", choices = c("All",as.character(unique(dd()$FlagCountry))), selected = newSelection)
    }
})
#Species 
observeEvent(input$species, {
  selected$v <- input$species
  if(selected$v[1] %in% "All" & length(selected$v) > 1){
    newSelection <- subset(input$species, !input$species %in% "All")
    updateSelectizeInput(session, "species", choices = c("All",as.character(unique(vars()))), selected = newSelection)
  }else if(!selected$v[1] %in% "All" & sum(str_detect(selected$v, "All")) %in% 1){
    newSelection <- subset(input$species, input$species %in% "All")
    updateSelectizeInput(session, "species", choices = c("All",as.character(unique(vars()))), selected = newSelection)
  }
})
#Sampling type 
observeEvent(input$samtype, {
  selected$v <- input$samtype
  if(selected$v[1] %in% "All" & length(selected$v) > 1){
    newSelection <- subset(input$samtype, !input$samtype %in% "All")
    updateSelectizeInput(session, "samtype", choices = c("All", levels(data_list()[[4]]$SamplingType)), selected = newSelection)
  }else if(!selected$v[1] %in% "All" & sum(str_detect(selected$v, "All")) %in% 1){
    newSelection <- subset(input$samtype, input$samtype %in% "All")
    updateSelectizeInput(session, "samtype", choices = c("All", levels(data_list()[[4]]$SamplingType)), selected = newSelection)
  }
})
#Quarter
observeEvent(input$quarter, {
  selected$v <- input$quarter
  if(selected$v[1] %in% "All" & length(selected$v) > 1){
    newSelection <- subset(input$quarter, !input$quarter %in% "All")
    updateSelectizeInput(session, "quarter", choices =  c("All", levels(data_list()[[4]]$Quarter)), selected = newSelection)
  }else if(!selected$v[1] %in% "All" & sum(str_detect(selected$v, "All")) %in% 1){
    newSelection <- subset(input$quarter, input$quarter %in% "All")
    updateSelectizeInput(session, "quarter", choices =  c("All", levels(data_list()[[4]]$Quarter)), selected = newSelection)
  }
})

# Catch category
observeEvent(input$catchcat, {
  selected$v <- input$catchcat
  if(selected$v[1] %in% "All" & length(selected$v) > 1){
    newSelection <- subset(input$catchcat, !input$catchcat %in% "All")
    updateSelectizeInput(session, "catchcat", choices =  c("All", levels(data_list()[[4]]$CatchCategory)), selected = newSelection)
  }else if(!selected$v[1] %in% "All" & sum(str_detect(selected$v, "All")) %in% 1){
    newSelection <- subset(input$catchcat, input$catchcat %in% "All")
    updateSelectizeInput(session, "catchcat", choices =  c("All", levels(data_list()[[4]]$CatchCategory)), selected = newSelection)
  }
})

# observe({
#   if(input$country != "All"){
#     # Updating selectize input
#     updateSelectizeInput(session, "country", choices =c(as.character(unique(vars()))), selected = "") 
#   }
# 
# })

# ---------------------------------------------------------
# AbsolutePanel uiOutput
# ---------------------------------------------------------

output$absolute <- renderUI({
  req(input$file)
  absolutePanel(
    singleton(tags$head(tags$script(src = "code.js"))),
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
    h2("Sampling explorer", align = "center"),
    # uiOutput("countryui"),uiOutput("speciesui"),uiOutput("samptypeui"),uiOutput("quarterui"),
    selectInput(
      "year",
      "Year",
      choices =
        #c("All", levels(data_list()[[3]]$Region)),
        c(levels(data_list()[[4]]$Year)),
      multiple = F,
      selected = "All"
    ),
    selectizeInput(
      "quarter",
      "Quarter",
      choices =
        #c("All", levels(data_list()[[3]]$Quarter)),
        c("All", levels(data_list()[[4]]$Quarter)),
      multiple = TRUE,
      selected = "All",
      options = list(plugins = list("remove_button", "drag_drop"))
    ),
    selectInput(
      "region",
      "Region",
      choices =
      #c("All", levels(data_list()[[3]]$Region)),
      c("All", levels(data_list()[[4]]$Region)),
      multiple = F,
      selected = "All"
    ),
    selectizeInput(
      "country",
      "Country",
      choices = c(""),
      # c("All", levels(data_list()[[3]]$LandingCountry)),
      multiple = TRUE,
      #selected = "All",
      options = list(plugins = list("remove_button", "drag_drop"))
    ),
    radioButtons(
      "sppNamechoice",
      "Species",
      choices = c("LatinName",
                  "EnglishName",
                  "Alpha3ID", 
                  "WoRMSAphiaID"
                  ),
      inline= TRUE
    ),
    selectizeInput(
      "species",
      " ",
      choices = c(""),
      #  c("All", levels(data_list()[[3]]$Species)),
      multiple = TRUE,
      #selected = "All",
      options = list(plugins = list("remove_button", "drag_drop"))
    ),
    selectizeInput(
      "samtype",
      "Sampling Type",
      choices =
      #c("All", levels(data_list()[[3]]$SamplingType)),
      c("All", levels(data_list()[[4]]$SamplingType)),
      multiple = TRUE,
      selected = "All",
      options = list(plugins = list("remove_button", "drag_drop"))
    ),
    selectizeInput(
      "catchcat",
      "Catch category",
      choices =
        #c("All", levels(data_list()[[3]]$SamplingType)),
        c("All", levels(factor(data_list()[[4]]$CatchCategory))),
      multiple = TRUE,
      selected = "All",
      options = list(plugins = list("remove_button", "drag_drop"))
    ),
    popify(selectInput ("N_var2", "Variable", var, multiple = F), ""),
    checkboxInput("rec", "ICES Rectangles"),
    br(),
    actionButton ("view2", "View"),
    downloadButton("down", "Generate report"),
    br(),
    hr(),
    br(),
    br(),
    plotOutput("plot2", height = 300)
    , tableOutput("debug")
  )
})

# Pop up box in interactive map 
observeEvent(input$N_var2, {
    if(input$N_var2 %in% "NumAgeFish"){
      addPopover(session, "N_var2", "", content = "Number of fish with age recorded", trigger = "hover" , placement = "right")
      }else if(input$N_var2 %in% "NumAgeTrips"){
        addPopover(session, "N_var2", "", content =  "Numbers of trips with age samples", trigger = "hover" , placement = "right")
      }else if(input$N_var2 %in% "NumWeightFish"){
        addPopover(session, "N_var2",  "", content = "Number of weight measurements", trigger = "hover" , placement = "right")
      }else if(input$N_var2 %in% "NumWeightTrips"){
        addPopover(session, "N_var2",  "", content = "Numbers of trips with recorded weight", trigger = "hover" , placement = "right")
      }else if(input$N_var2 %in% "NumMaturityStageFish"){
        addPopover(session, "N_var2",  "", content = "Numbers of fish with maturity stage readings", trigger = "hover" , placement = "right")
      }else if(input$N_var2 %in% "NumMaturityStageTrips"){
        addPopover(session, "N_var2",  "", content = "Numbers of trips with recorded maturity stage", trigger = "hover" , placement = "right")
      }else if(input$N_var2 %in% "NumLengthFish"){
        addPopover(session, "N_var2",  "", content = "Numbers of fish with length measurements", trigger = "hover" , placement = "right")
      }else{
        addPopover(session, "N_var2",  "", content = "Numbers of trips with length samples", trigger = "hover" , placement = "right")
}})

# ---------------------------------------------------------
# Filtered data
# ---------------------------------------------------------

df <- reactive({
  
  #data<-data_list()[[3]]
  data<-data_list()[[4]]
  data<-as.data.frame(data)
  data<-data%>%left_join(spptable)
  
  if (!("All" %in% input$region)){
    data <- data[data$Region == input$region,]
  }
  if (!("All" %in% input$country)){
    #data <- data[data$LandingCountry %in% input$country,]
    data <- data[data$FlagCountry %in% input$country,]
  }
  if (!("All" %in% input$species)){
    
    if(input$sppNamechoice == "LatinName"){
      data <- data[data$Species %in% input$species,]
    }else{
    if(input$sppNamechoice == "EnglishName"){
      data <- data[data$EnglishName %in% input$species,]
    }else{
    if(input$sppNamechoice == "Alpha3ID"){
      data <- data[data$Alpha3ID %in% input$species,]
    }else{
    if(input$sppNamechoice == "WoRMSAphiaID"){
      data <- data[data$WoRMSAphiaID %in% input$species,]
    }
    }}}}
  
  if (!("All" %in% input$samtype)){
    data <- data[data$SamplingType %in% input$samtype,]
  }
  if (!("All" %in% input$quarter)){
    data <- data[data$Quarter %in% input$quarter,]
  }
  if (!("All" %in% input$catchcat)){
    data <- data[data$CatchCategory %in% input$catchcat,]
  }
  
  # data <- data[, c("LandingCountry", "Quarter",  "Species", "SamplingType",
  #                  "lat", "lon", input$N_var2)]
  data <- data[, c("FlagCountry", "Quarter",  "Species", "SamplingType",
                   "lat", "lon", input$N_var2, "NumMaturityStageTrips", "NumAgeTrips", "NumLengthTrips", "NumWeightTrips")]
  # names(data) <- c("LandingCountry", "Quarter",  "Species", "SamplingType",
  #                  "lat", "lon", "aux")
  names(data) <- c("FlagCountry", "Quarter",  "Species", "SamplingType",
                   "lat", "lon", "aux", "NumMaturityStageTrips", "NumAgeTrips", "NumLengthTrips", "NumWeightTrips")
  data$FlagCountry <- droplevels(data$FlagCountry)
  
  data
})

# ---------------------------------------------------------
# Data aggregation at location 
# filtered data updated with view button
# ---------------------------------------------------------
# 
#filter_df <- eventReactive(input$view2, { # Previous filter_df, consider to delete if edits are accepted
#  if(nrow(df())!=0){
#    dat<-aggregate(list(aux = df()$aux),
#                   by = list(lat = df()$lat, lon = df()$lon),
#                   # LandingCountry = df()$LandingCountry,
#                   # Species = df()$Species,
#                   # SamplingType = df()$SamplingType,
#                   # Quarter = df()$Quarter),
#                   FUN = sum)
#  }
#  else {
#    dat <- df()
#  }
#})

filter_df <- eventReactive(input$view2, { # Current filter_df, including number of trips contributing to the displayed statistics, by each of the coordinates combination. 
  if(nrow(df())!=0){
    dat<- df() %>% 
        mutate(code = as.factor(paste(lat, lon))) %>%
        distinct() %>%
        group_by(code) %>%
        dplyr::summarize(
          quarter_list = paste(unique(Quarter), collapse = ", "),
          flagcountry_list = paste(unique(FlagCountry), collapse = ", "),
          species_list = paste(unique(Species), collapse = ", "),
          samplingType_list = paste(unique(SamplingType), collapse = ", "),
          aux = sum(aux),
          n_trip = case_when( # How many trip are contributing
            input$N_var2 == "NumMaturityStageFish" ~ sum(as.numeric(NumMaturityStageTrips), na.rm = T),
            input$N_var2 == "NumAgeFish" ~ sum(as.numeric(NumAgeTrips), na.rm = T),
            input$N_var2 == "NumLengthFish" ~ sum(as.numeric(NumLengthTrips), na.rm = T),
            input$N_var2 == "NumWeightFish" ~ sum(as.numeric(NumWeightTrips), na.rm = T)
                              ),  
          lat = lat,
          lon = lon
          )
  }
  else {
    dat <- df()
  }
})

# -----------------------------------
# Debugging
# -----------------------------------

 # output$debug <- renderTable({
 # #head(filter_df(), 5)
 #  dat <-data_list()[[3]]
 #  unique(dat[dat$LandingCountry== "ESP",]$Species)
 #  #unique(vars()$LandingCountry)
 # 
 # })


# -----------------------------------
# leaflet map and barplots
# -----------------------------------

#input$view2ColorsBAR <- colour_table$colour4
ColorsBAR <- colour_table$colour4
names(ColorsBAR) <- colour_table$Country
#colScaleBAR<-scale_fill_manual(name="LandingCountry", values=ColorsBAR)
colScaleBAR<-scale_fill_manual(name="FlagCountry", values=ColorsBAR)

pal.rmd<-reactive({colorNumeric ("viridis", domain = as.numeric(filter_df()$aux))})


output$down <- downloadHandler(
  # For PDF output, change this to "report.pdf"
  filename = paste("report",Sys.Date(),".html",sep=''),
  
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    tempReport <- file.path(tempdir(), "report.Rmd")
    
    file.copy("markdown/report.Rmd", tempReport, overwrite = TRUE)
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
                     fillOpacity=0.8,
                     popup = ~paste0( # Add pop-up to each of the data point displayed 
                                      "<strong>", "Vessel country flag:", "</strong>", filter_df()$flagcountry_list, "<br>", 
                                      "<strong>", "Number of trips contributing:", "</strong>", filter_df()$n_trip, "<br>", 
                                      "<strong>","Quarter :","</strong>", filter_df()$quarter_list, "<br>", 
                                      "<strong>","Species :","</strong>", filter_df()$species_list,"<br>", 
                                      "<strong>","Sampling type: ","</strong>", filter_df()$samplingType_list,"<br>", 
                                      "<strong>", input$N_var2, " :", "</strong>", filter_df()$aux,"<br>", 
                                      "<strong>","Latitude: ","</strong>", filter_df()$lat,"<br>", 
                                      "<strong>","Longitude: ","</strong>", filter_df()$lon
                     #popup = popupTable() # Other options, like graphs or tables inside the pop-up might be considered later on]
                        ))  %>%
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
  #colScaleBAR<-scale_fill_manual(name="LandingCountry", values=ColorsBAR)
  colScaleBAR<-scale_fill_manual(name="FlagCountry", values=ColorsBAR)
  isolate({
    if(nrow(df())==0) return({shinyalert("Oops!", "No data for this selection", type = "error")})  
    ggplot(df(), aes(x=FlagCountry, y=aux, fill=FlagCountry)) +
      geom_bar(stat="identity")+
      colScaleBAR +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      labs(y = input$N_var2)
  })
  
})