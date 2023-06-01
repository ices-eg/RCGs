


# -------------------------
# Updating selectize input == country 
# -------------------------

pars <- reactive({
  
  data<-data_list()[[2]]
  data<-as.data.frame(data)
  data<-data%>%left_join(spptable)
  
  if (!("All" %in% input$fishgroundp)){
    data <- data[data$FishingGround %in% input$fishgroundp,]
  }
  
  data <- data[, input$sppNamechoicep]
  data
})


observe({
  # Updating selectize input
  #updateSelectInput(session, "countryg", choices = unique(dg()$LandingCountry), selected = sort(unique(dg()$LandingCountry))[1]) 
  updateSelectInput(session, "speciesp", choices = c("All",sort(as.character(unique(pars())))), selected = "All")
})



output$summary <- renderUI({
  
  tabsetPanel(type = "tabs",
              
       #------------------------------------
       # Sampling summary
       #------------------------------------
       tabPanel( 
         title = "Sampling summary", 
         fluidRow(
           br(),
           column(4,
                  p("Barplot"),
                  selectizeInput("yearp","Year",
                                 choices =c(levels(data_list()[[2]]$Year)),
                                 multiple = FALSE,
                                 selected = "All"),
                  selectizeInput("fishgroundp","Fishing Ground",
                                 choices =c("All", levels(data_list()[[2]]$FishingGround)),
                                 multiple = TRUE,
                                 selected = "All",
                                 options = list(plugins = list("remove_button", "drag_drop"))),
                  radioButtons(
                    "sppNamechoicep",
                    "Species",
                    choices = c("LatinName",
                                "EnglishName",
                                "Alpha3ID", 
                                "WoRMSAphiaID"
                    )
                  ),
                  selectizeInput("speciesp","Species",
                                 choices = c(""),
                                 #choices =c("All", sort(levels(data_list()[[2]]$Species))),
                                 multiple = TRUE,
                                 #selected = "All",
                                 options = list(plugins = list("remove_button", "drag_drop"))),
                  selectizeInput( "samtypep", "Sampling Type",
                    choices = c("All", levels(data_list()[[3]]$SamplingType)),
                    multiple = TRUE,
                    selected = "All",
                    options = list(plugins = list("remove_button", "drag_drop"))),
                  selectInput ("N_varX", "X axis", 
                                choices = c("LandingCountry", "FlagCountry","Area", "FishingActivityCategoryEuropeanLvl6"), 
                                  multiple = F),
                  prettyRadioButtons("groupX", label = "Group X", 
                                  choices = c("None", 
                                              "LandingCountry",
                                              "FlagCountry",
                                              "Area", 
                                              "FishingActivityCategoryEuropeanLvl6", 
                                              "CatchCategory")),
                  selectInput ("N_varY", "Y axis",
                                choices = c("NumLengthFish", "NumLengthTrips"), multiple = F), 
                  hr(),
                  div(style="display: inline-block;vertical-align:top;", actionButton ("view4", "View")),
                  div(style="display: inline-block;vertical-align:top;",downloadButton ("down4", "Download plot"))
                  ),
           column(8, 
                  plotOutput("sumplot", height = "600px", width = "1000px"),
                  # for logs
                  tableOutput("bugtable"))
           )
           )#,
       
       #------------------------------------
       # Sampling vs landing
       #------------------------------------
       
       # tabPanel( 
       #   title = "Sampling vs landings")
       
  )# end of the tabsetPanel
  
})



# -----------------------------------
# Filtered data
# -----------------------------------

# "Year","Region","FlagCountry","LandingCountry",
# "Stock","Species","SamplingType","StartQuarter","Area" ,
# "FishingActivityCategoryEuropeanLvl6", "CatchCategory","VesselLengthCategory"

dfp <- reactive({
  
  data<-data_list()[[2]] #SL
  data<-as.data.frame(data)
  data<-data%>%left_join(spptable)
  

  if (!("All" %in% input$fishgroundp )){
    data <- data[data$FishingGround %in% input$fishgroundp,]
  }
  if (!("All" %in% input$speciesp)){
    if(input$sppNamechoicep == "LatinName"){
      data <- data[data$Species %in% input$speciesp,]
    }else{
    if(input$sppNamechoicep == "EnglishName"){
      data <- data[data$EnglishName %in% input$speciesp,]
    }else{
    if(input$sppNamechoicep == "Alpha3ID"){
      data <- data[data$Alpha3ID %in% input$speciesp,]
    }else{
    if(input$sppNamechoicep == "WoRMSAphiaID"){
      data <- data[data$WoRMSAphiaID %in% input$speciesp,]
    }
    }}}}
  
  # if (!("All" %in% input$speciesp )){
  #   data <- data[data$Species %in% input$speciesp,]
  # }
  if (!("All" %in% input$samtypep)){
    data <- data[data$SamplingType %in% input$samtypep,]
  }
  
  if (input$groupX != "None" ) {
    data <- data[, c("Species","FishingGround", "SamplingType", input$N_varX, input$N_varY, input$groupX)]
    names(data) <- c("Species","FishingGround", "SamplingType","auxX", "auxY", "auxG")
  }
  
  if (input$groupX == "None" ) {
    data <- data[, c("Species","FishingGround", "SamplingType", input$N_varX, input$N_varY, input$N_varX)]
    names(data) <- c("Species","FishingGround", "SamplingType","auxX", "auxY", "auxG")
  }
  
  data

})



# -----------------------------------
# barplot to panel
# -----------------------------------


output$sumplot <- renderPlot ({

  if (input$view4==0) return()

  #safe_colorblind_palette <- colour_table$colour1

  isolate({ 
    if(nrow(dfp())==0) return({shinyalert("Oops!", "No data for this selection", type = "error")})
    ggplot(dfp(), aes(x=auxX, y=auxY, fill=auxG)) +
      geom_bar(position = "stack", stat="identity")+
      #scale_fill_manual(safe_colorblind_palette) +
      labs(y = input$N_varY, x = input$N_varX, fill = input$groupX, title = "")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))
  })
})

# # for log
# output$bugtable <- renderTable ({
# 
#   if (input$view4==0) return()
# 
#   head(dfp())
# })


# -----------------------------------
# Download plot 
# -----------------------------------


output$down4 <- downloadHandler(
  
  filename ="interactive_plot.png",
  
  content = function(interactive.plot){
    png(interactive.plot)  
    
    if (input$view4==0) return()
    
    interactive.plot <-ggplot(dfp(), aes(x=auxX, y=auxY, fill=auxG)) +
                            geom_bar(position = "stack", stat="identity")+
                            #scale_fill_manual(safe_colorblind_palette) +
                            labs(y = input$N_varY, x = input$N_varX, fill = input$groupX)+
                            theme_bw()+
                            theme(axis.text.x = element_text(angle = 90, hjust = 1),
                            axis.text=element_text(size=12),
                            axis.title=element_text(size=14,face="bold"))
    print(interactive.plot)
    dev.off()
  }
)
