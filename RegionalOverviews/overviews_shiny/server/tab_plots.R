
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
                  selectInput ("N_var4", "Variable", var, multiple = F), 
                  selectizeInput("samtypep","Sampling Type",
                                 choices = c("All", levels(data_list()[[3]]$SamplingType)),
                                 multiple = TRUE, selected = "All",
                                 options = list(plugins = list("remove_button", "drag_drop"))),
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

dfp <- reactive({
  
  data<-data_list()[[3]]
  data<-as.data.frame(data)
  
  if (!("All" %in% input$samtypep)){
    data <- data[data$SamplingType %in% input$samtypep,]
  }
  
  data <- data[, c("LandingCountry","SamplingType",input$N_var4)]
  names(data) <- c("LandingCountry","SamplingType", "aux4")
  data
})



# -----------------------------------
# barplot to panel
# -----------------------------------


output$sumplot <- renderPlot ({
  #input$view2
  if (input$view4==0) return()
  
  #validate(need(input$plottype=="Barplot", message=FALSE))
  ColorsBAR <- colour_table$colour4
  names(ColorsBAR) <- colour_table$Country
  colScaleBAR<-scale_fill_manual(name="LandingCountry", values=ColorsBAR)
  isolate({ ggplot(dfp(), aes(x=LandingCountry, y=aux4, fill=LandingCountry)) +
      geom_bar(stat="identity")+
      colScaleBAR +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      labs(y = input$N_var4)
  })
})

# output$bugtable <- renderTable ({
#   
#   if (input$view4==0) return()
# 
#   head(dfp())
# })