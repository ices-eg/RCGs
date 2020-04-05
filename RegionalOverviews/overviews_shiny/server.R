##--------------
## libraries
##--------------

library(shinythemes)
library(shinyBS)
library(shinyTime)
library(shinyjs)
library(leaflet)
#library(leafem)
library(ggplot2)
library(stringr)

library("sf")
library(viridis)
library("rnaturalearth") # map of countries of the entire world
library("rnaturalearthdata") # Use ne_countries to pull country data



##--------------
## data
##--------------

#load("data/ShinyTest_BigPicture.RData")
load("data/inventory_ca.RData")
load("data/graph_det.RData")
inventory_ca$SamplingType <- as.factor(inventory_ca$SamplingType)
inventory_ca$Quarter <- as.factor(as.character(inventory_ca$Quarter))

setwd("C:/ISSG2/shiny/github_cloned/RCGs/Sampling_overview")

group <- c("SamplingCountry","FlagCountry","LandingCountry","Year",                
           "Quarter","Species","Area","SamplingType",        
           "Metier","StatisticalRectangle","lat","lon") 

##--------------
## Fix a color for each country
##--------------

colour_table<-read.table("data/aux_colours.txt", header=T, sep="\t", colClasses="character", na.strings="", comment.char="")


##--------------
## read functions
##--------------

source("funs/pointsMap_func.r")	
#source("funs/choroplethMap_func.R")
source("funs/func_barplot_var_by_one_var.r")	


##--------------
## Mapping
##--------------

world <- ne_countries(scale = "medium", returnclass = "sf")

##--------------
## server
##--------------

server <- function(input, output, session){
  
  
  # *********************
  # Tab "with functions"
  # *********************
   
   # -----------------------------------
   # Reactive variables 
   # -----------------------------------
  
   vars <- reactive({str_remove(group, input$group)})
   
   output$listvars <- renderUI({
      facet <- vars()
      selectInput ("facet", "Facet", facet,  multiple = F)
   })
   
   # -----------------------------------
   # Plots
   # -----------------------------------
   

   output$plot1<- renderPlot({
      
      if (input$view == 0) return(invisible(NULL))
      isolate(
         if(input$plottype == "Map"){
            
            pointsMap_func (df= inventory_ca,
                    var= input$var,
                    groupBy= input$group,
                    facet = input$facet,
                    func = 'sum',
                    points_coord = inventory_ca)
         }else{
            if(input$plottype == "Barplot"){
               
            barplot_var_by_one_var(x = as.data.frame(inventory_ca),
                     Var = input$var,
                     var1 = input$group,
                     tapply_type = "sum",
                     type_of_threshold="cum_percent",
                     value_of_threshold=100,
                     graph_par = eval(parse(text=graph_det$graph_par[1])))
            }
         }
      )
   })
   
   # ******************
   # Tab "with leaflet"
   # ******************
   
   # -----------------------------------
   # Filtered data
   # -----------------------------------
   
   df <- reactive({
     
     data<-inventory_ca
     
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
   
   # --------------------------------------
   # Data aggregation at location 
   # filtered data updated with view button
   # --------------------------------------
   
   filter_df <- eventReactive(input$view2, {
     if(nrow(df())!=0){
       dat<-aggregate (list(aux = df()$aux),
                          by = list(lat = df()$lat, lon = df()$lon,
                               LandingCountry = df()$LandingCountry,
                               Species = df()$Species,
                               SamplingType = df()$SamplingType,
                               Quarter = df()$Quarter),
                          FUN = sum)
       }
     else {
     dat <- df()
     }
   }) 
   
   
   # -----------------------------------
   # Debugging
   # -----------------------------------
   
    # output$debug <- renderTable({
    #   #head(df(), 5)
    #   head(filter_df(), 5)
    # })
   
   
   # -----------------------------------
   # leaflet map and barplots
   # -----------------------------------

   
 
      #input$view2
      
      
output$map <- renderLeaflet({
            leaflet() %>% addProviderTiles(providers$CartoDB.Positron)  %>% 
               setView(lng = -5,lat =  45.5, zoom = 5)
         })
         
      
     
    
   
   
   # -----------------------------------
   # Add filtered data to map
   # -----------------------------------
   
   observeEvent(input$view2,{
      #if (input$plottype == "Map")
     pal<-colorNumeric ("viridis", domain = as.numeric(filter_df()$aux))

     leafletProxy("map", data = filter_df())%>% 
       clearMarkers()%>% clearControls() %>% addProviderTiles(providers$CartoDB.Positron) %>% 
       addCircleMarkers (color=~pal(aux),
                         stroke=F,
                         radius=~ (sqrt(aux)*2),
                         fillOpacity=0.8)%>%
       addLegend( "bottomleft", pal=pal, values=~aux, title = input$N_var2,opacity = 0.8)
   })

   # ******************
   # Tab "with ggplot"
   # ******************
   
   # -----------------------------------
   # Filtered data
   # -----------------------------------
   
   df3 <- reactive({
      
      data3<-inventory_ca
      
      if (!("All" %in% input$country3)){
         data3 <- data3[data3$LandingCountry %in% input$country3,]
      }
      if (!("All" %in% input$species3 )){
         data3 <- data3[data3$Species %in% input$species3,]
      }
      if (!("All" %in% input$samtype3)){
         data3 <- data3[data3$SamplingType %in% input$samtype3,]
      }
      if (!("All" %in% input$quarter3)){
         data3 <- data3[data3$Quarter %in% input$quarter3,]
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
         
         dat3<-aggregate (list(aux = df3()$aux),
                         by = list(lat = df3()$lat, lon = df3()$lon,
                                   LandingCountry = df3()$LandingCountry,
                                   Species = df3()$Species,
                                   SamplingType = df3()$SamplingType,
                                   Quarter = df3()$Quarter),
                         FUN = sum) # not sure should be set TRUE
      }
      else {
       dat3 <- df3()
      }
       dat3
  
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

      if(nrow(filter_df3())==0) return(invisible(NULL))

      ggplot(data = world) + geom_sf(fill= "antiquewhite") +
         geom_point(data = filter_df3(), aes(x = lon, y = lat, colour = aux, size = aux)) +
         scale_colour_viridis() +
         #facet_grid (SamplingType~LandingCountry)+
         facet_wrap(~SamplingType)+
         coord_sf(crs = "+init=epsg:4326", xlim = c(-25, 5), ylim = c(43, 63), expand = FALSE) +
         xlab("Longitude") + ylab("Latitude") + labs(size=input$N_var3, colour=input$N_var3)+
         ggtitle("North East Atlantic") +
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

} # end of the server