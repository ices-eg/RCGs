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
library(knitr)
library("sf")
library(viridis)
library("rnaturalearth") # map of countries of the entire world
library("rnaturalearthdata") # Use ne_countries to pull country data
library(rgdal)
#library(webshot)
library(rmarkdown)
library(lubridate)
library(data.table)
library(mapplots)
library(shinybusy)
library(shinyWidgets)
library(ggrepel) # requiered by shinyappsio
library(dplyr)
library(rgeos) # requiered by shinyappsio
library(rintrojs) 
library(plotly) # added 2022

library(shinyalert)




##--------------
## data
##--------------

#load("data/ShinyTest_BigPicture.RData")
#load("data/inventory_ca.RData")
#load("data/graph_det.RData")

##--------------
## ICES rectangle shp
##--------------

ices.rect <- read_sf("shp/ices_rectangles/ices_squares_simple.shp")
#ices.rect <- read_sf("../data/shapefiles/ices_rectangles/ices_squares_simple.shp")
ices.rect<-as(ices.rect, 'Spatial')


##--------------
## Fix a color for each country
##--------------

colour_table<-read.table("data/aux_colours.txt", header=T, sep="\t", colClasses="character", na.strings="", comment.char="")

#inventory_ca$SamplingType <- as.factor(inventory_ca$SamplingType)
#inventory_ca$Quarter <- as.factor(as.character(inventory_ca$Quarter))


##--------------
## Mapping
##--------------

world <- ne_countries(scale = "medium", returnclass = "sf")

##--------------
## var for faceting widget
##--------------

facetvar <-
  c("LandingCountry",
    #"FlagCountry",
    "Species", 
    "SamplingType", 
    "Quarter")

facetvar2 <-
  c(#"LandingCountry",
    "FlagCountry",
    "Species", 
    "SamplingType", 
    "Quarter")

##--------------
## vars selectIntput widgets
##--------------

var <-
 c(
    "NumAgeFish",
    "NumAgeTrips",
    "NumLengthFish",
    "NumLengthTrips",
    "NumWeightFish",
    "NumWeightTrips",
    "NumMaturityStageFish",
    "NumMaturityStageTrips"
  )

 #group <-
  #c(
   # "SamplingCountry",
    #"FlagCountry",
    #"LandingCountry",
    #"Year",
    #"Quarter",
    #"Species",
    #"Area",
    #"SamplingType",
    #"Metier",
    #"StatisticalRectangle",
    #"lat",
    #"lon"
  #)

##--------------
## switch to different species nomenclature
##--------------

spptable <- read.csv("data/jointable.csv", sep= ";")


#setwd("C:/Users/Win10 Home x64/Desktop/Arbeitszeug/RCG_ISSG/RCGs/RegionalOverviews/overviews_shiny")
# 
# group <- c("SamplingCountry","FlagCountry","LandingCountry","Year",                
#            "Quarter","Species","Area","SamplingType",        
#            "Metier","StatisticalRectangle","lat","lon") 

##--------------
## round the max value in mapping variable otherwise viridis did not take into account
##-------------- 

roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}