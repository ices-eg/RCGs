
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
                  selectizeInput("speciesp","Species",
                    choices =c("All", levels(data_list()[[2]]$Species)),
                    multiple = TRUE,
                    selected = "All",
                    options = list(plugins = list("remove_button", "drag_drop"))
                  ),
                  selectizeInput("fishgroundp","Fishing Ground",
                                 choices =c("All", levels(data_list()[[2]]$FishingGround)),
                                 multiple = TRUE,
                                 selected = "All",
                                 options = list(plugins = list("remove_button", "drag_drop"))
                  ),
                  selectInput ("N_varX", "X axis", 
                                choices = c("LandingCountry", "Area", "FishingActivityCategoryEuropeanLvl6"), 
                                  multiple = F),
                  selectInput ("N_varY", "Y axis",
                                choices = c("NoLength", "NoLengthTrips"), multiple = F), 
                  actionButton ("view4", "View")), 
           column(8, 
                  plotOutput("sumplot", height = "600px", width = "1000px"),
                  # for bugs
                  tableOutput(("bugtable")))
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
  
  if (!("All" %in% input$speciesp )){
    data <- data[data$Species %in% input$speciesp,]
  }
  
  if (!("All" %in% input$fishgroundp )){
    data <- data[data$FishingGround %in% input$fishgroundp,]
  }
  
  data <- data[, c("Species","FishingGround", input$N_varX, input$N_varY)]
  
  names(data) <- c("Species","FishingGround", "auxX", "auxY")
  
  data

})



# -----------------------------------
# barplot to panel
# -----------------------------------


output$sumplot <- renderPlot ({

  if (input$view4==0) return()
  
  # ColorsBAR <- colour_table$colour4
  # names(ColorsBAR) <- colour_table$Country
  # colScaleBAR<-scale_fill_manual(name="LandingCountry", values=ColorsBAR)
  isolate({ ggplot(dfp(), aes(x=auxX, y=auxY, fill=auxX)) +
      geom_bar(stat="identity")+
      #colScaleBAR +
      labs(y = input$N_varY, x = input$N_varX, fill = input$N_varX)+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))
  })
})

# for log
# output$bugtable <- renderTable ({
# 
#   if (input$view4==0) return()
# 
#   head(dfp())
# })