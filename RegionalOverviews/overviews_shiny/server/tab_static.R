# ******************
# Tab "with ggplot"
# ******************
output$static <- renderUI({
  req(input$file)
  
  fluidRow(
    column(4, 
           selectizeInput (
             "countryg", 
             "Country",
             choices =
               c("All", levels(data_list()[[3]]$LandingCountry)),
             multiple = TRUE,
             selected = "All",
             options = list(plugins = list("remove_button", "drag_drop"))
           ),
           selectizeInput(
             "speciesg",
             "Species",
             choices =
               c("All", levels(data_list()[[3]]$Species)),
             multiple = TRUE,
             selected = "All",
             options = list(plugins = list("remove_button", "drag_drop"))
           ),
           selectizeInput(
             "samtypeg",
             "Sampling Type",
             choices =
               c("All", levels(data_list()[[3]]$SamplingType)),
             multiple = TRUE,
             selected = "All",
             options = list(plugins = list("remove_button", "drag_drop"))
           ),
           selectizeInput(
             "quarterg",
             "Quarter",
             choices =
               c("All", levels(data_list()[[3]]$Quarter)),
             multiple = TRUE,
             selected = "All",
             options = list(plugins = list("remove_button", "drag_drop"))
           ),
           selectInput ("N_var3", "Variable", var, multiple = F),
           hr(),
           selectInput ("facetx", "Facet by", facetvar, multiple = F),
           #selectInput ("facety", "Facet.y", facetvar, multiple = F),
           hr(),
           actionButton ("view3", "View")
    ), 
    column(8, 
           plotOutput("plot3", height = "700px", width = "900px")#,
           #tableOutput("debug3")
           #verbatimTextOutput("debug3")
    )
  )
  
})

# -----------------------------------
# Filtered data
# -----------------------------------
df3 <- reactive({
  
  data3<-data_list()[[3]]
  data3<-as.data.frame(data3)
  
  if (!("All" %in% input$countryg)){
    data3 <- data3[data3$LandingCountry %in% input$countryg,]
  }
  if (!("All" %in% input$speciesg )){
    data3 <- data3[data3$Species %in% input$speciesg,]
  }
  if (!("All" %in% input$samtypeg)){
    data3 <- data3[data3$SamplingType %in% input$samtypeg,]
  }
  if (!("All" %in% input$quarterg)){
    data3 <- data3[data3$Quarter %in% input$quarterg,]
  }
  
  data3 <- data3[, c("LandingCountry", "Quarter",  "Species", "SamplingType",
                     "lat", "lon", input$N_var3)]
  names(data3) <- c("LandingCountry", "Quarter",  "Species", "SamplingType",
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

output$plot3 <- renderPlot ({
  
  input$view3
  
  isolate({
    
    if(nrow(filter_df3())==0) return(invisible(NULL))
    
    ggplot(data = world) + geom_sf(fill= "antiquewhite") +
      geom_point(data = filter_df3(), aes(x = lon, y = lat, colour = aux, size = aux)) +
      scale_colour_viridis(guide ="legend") + 
      #facet_grid (SamplingType~LandingCountry)+
      facet_wrap(~facet)+
      coord_sf(crs = "+init=epsg:4326", xlim = c(-25, 5), ylim = c(43, 63), expand = FALSE) +
      xlab("Longitude") + ylab("Latitude") + labs(size=input$N_var3, colour=input$N_var3)+
      #ggtitle("North East Atlantic") +
      theme(
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

