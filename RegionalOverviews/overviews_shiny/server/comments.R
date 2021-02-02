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