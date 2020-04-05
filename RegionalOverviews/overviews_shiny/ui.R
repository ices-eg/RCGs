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

##--------------
## data
##--------------

#load("data/ShinyTest_BigPicture.RData")
load("data/inventory_ca.RData")
load("data/graph_det.RData")
inventory_ca$LandingCountry <-
  as.factor(inventory_ca$LandingCountry)
inventory_ca$Species <-
  as.factor(as.character(inventory_ca$Species))
inventory_ca$SamplingType <- as.factor(inventory_ca$SamplingType)
inventory_ca$Quarter <-
  as.factor(as.character(inventory_ca$Quarter))


var <-
  c(
    "N_measured_age",
    "N_trips_age",
    "N_measured_weight",
    "N_trips_weight",
    "N_measured_maturity",
    "N_trips_maturity"
  )
group <-
  c(
    "SamplingCountry",
    "FlagCountry",
    "LandingCountry",
    "Year",
    "Quarter",
    "Species",
    "Area",
    "SamplingType",
    "Metier",
    "StatisticalRectangle",
    "lat",
    "lon"
  )
# facet <- c("SamplingCountry","FlagCountry","LandingCountry","Year",
#             "Quarter","Species","Area","SamplingType",
#             "Metier","StatisticalRectangle","lat","lon")



##--------------
## ui
##--------------

ui <- navbarPage(
  "RCG overview",
  
  tabPanel("Home/About"),
  tabPanel("Fishery overview"),
  navbarMenu(
    "Sampling overview",
    tabPanel("Data explorer"),
    tabPanel("With functions",
             
             fluidRow(
               br(),
               br(),
               column(
                 4,
                 selectInput ("var", "Var", var, multiple = F),
                 selectInput ("group", "GroupBY", group, multiple = F),
                 conditionalPanel (condition = "input.plottype =='Map'",
                                   uiOutput("listvars")),
                 #selectInput ("facet", "Facet", facet,  multiple = F)),
                 radioButtons ("plottype", "Choose Plot", c("Map", "Barplot")),
                 actionButton ("view", "View")
               ),
               column(8,
                      #uiOutput("render_plot") # func do not like uiOutput
                      plotOutput("plot1"))
             )),
    
    ##--------------
    ## tabPanels "with leaflet"
    ##--------------

    
    tabPanel(
               "Interactive map",
               div(
                 class = "outer",
                 
                 tags$head(# Include our custom CSS
                   includeCSS("styles.css")),
                 # If not using custom CSS, set height of leafletOutput to a number instead of percent
                 leafletOutput("map", width = "100%", height = "100%"),
               
                 # Shiny versions prior to 0.11 should use class = "modal" instea#d.
                 absolutePanel(
                   id = "controls",
                   class = "panel panel-default",
                   fixed = TRUE,
                   draggable = TRUE,
                   bottom = "auto",
                   left = "auto",
                   right = 20,
                   top = 60,
                   width = 330,
                   height = "auto",
                   h2("Sampling explorer"),
                   selectizeInput (
                     "country",
                     "Country",
                     c("All", levels(inventory_ca$LandingCountry)),
                     multiple = TRUE,
                     selected = "All",
                     options = list(plugins = list("remove_button", "drag_drop"))
                   ),
                   selectizeInput (
                     "species",
                     "Species",
                     c("All", levels(inventory_ca$Species)),
                     multiple = TRUE,
                     selected = "All",
                     options = list(plugins = list("remove_button", "drag_drop"))
                   ),
                   selectizeInput (
                     "samtype",
                     "Sampling Type",
                     c("All", levels(inventory_ca$SamplingType)),
                     multiple = TRUE,
                     selected = "All",
                     options = list(plugins = list("remove_button", "drag_drop"))
                   ),
                   selectizeInput (
                     "quarter",
                     "Quarter",
                     c("All", levels(inventory_ca$Quarter)),
                     multiple = TRUE,
                     selected = "All",
                     options = list(plugins = list("remove_button", "drag_drop"))
                   ),
                   selectInput ("N_var2", "Variable", var, multiple = F),
                   actionButton ("view2", "View")
                 )
               )
               
             ),
             ##--------------
             ## tabPanels "with ggplot"
             ##--------------
             
      tabPanel("With ggplot",
                      fluidRow(
                        column(
                          4,
                          selectizeInput (
                            "country3",
                            "Country",
                            choices = c("All", levels(inventory_ca$LandingCountry)),
                            multiple = TRUE,
                            selected = "All",
                            options = list(plugins = list("remove_button", "drag_drop"))
                          ),
                          selectizeInput (
                            "species3",
                            "Species",
                            c("All", levels(inventory_ca$Species)),
                            multiple = TRUE,
                            selected = "All",
                            options = list(plugins = list("remove_button", "drag_drop"))
                          ),
                          selectizeInput (
                            "samtype3",
                            "Sampling Type",
                            c("All", levels(inventory_ca$SamplingType)),
                            multiple = TRUE,
                            selected = "All",
                            options = list(plugins = list("remove_button", "drag_drop"))
                          ),
                          selectizeInput (
                            "quarter3",
                            "Quarter",
                            c("All", levels(inventory_ca$Quarter)),
                            multiple = TRUE,
                            selected = "All",
                            options = list(plugins = list("remove_button", "drag_drop"))
                          ),
                          selectInput ("N_var3", "Variable", var, multiple = F),
                          actionButton ("view3", "View")
                        ),
                        column(8,
                               plotOutput("plot3", height = "600px")#,
                               #tableOutput("debug3")
                               #verbatimTextOutput("debug3")))
                        ))
             )),
             tabPanel("Stock overview")
             
             
             ##--------------
             ## tabPanels "with functions"
             ##--------------
             
             
             
    )
  
    # end of fluidPage
    