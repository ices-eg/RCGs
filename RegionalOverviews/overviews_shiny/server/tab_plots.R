
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
                  selectizeInput("fishgroundp","Fishing Ground",
                                 choices =c("All", levels(data_list()[[2]]$FishingGround)),
                                 multiple = TRUE,
                                 selected = "All",
                                 options = list(plugins = list("remove_button", "drag_drop"))),
                  selectizeInput("speciesp","Species",
                                 choices =c("All", levels(data_list()[[2]]$Species)),
                                 multiple = TRUE,
                                 selected = "All",
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
                                choices = c("NoLength", "NoLengthTrips"), multiple = F), 
                  actionButton ("view4", "View")), 
           column(8, 
                  plotOutput("sumplot", height = "600px", width = "1000px"),
                  # for logs
                  tableOutput("bugtable"))
           )
           ),
       
       #------------------------------------
       # Sampling vs landing
       #------------------------------------
       
       tabPanel( 
         title = "Sampling vs landings")
       
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
  

  if (!("All" %in% input$fishgroundp )){
    data <- data[data$FishingGround %in% input$fishgroundp,]
  }
  if (!("All" %in% input$speciesp )){
    data <- data[data$Species %in% input$speciesp,]
  }
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

  isolate({ ggplot(dfp(), aes(x=auxX, y=auxY, fill=auxG)) +
      geom_bar(position = "stack", stat="identity")+
      #scale_fill_manual(safe_colorblind_palette) +
      labs(y = input$N_varY, x = input$N_varX, fill = input$groupX)+
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
