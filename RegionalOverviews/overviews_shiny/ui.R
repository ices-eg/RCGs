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
library(officer)
library(shinydashboard)
library(officer)
library(webshot)

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

ui <- fluidPage(
  # "RCG overview",
  
  #  tabPanel("Home/About", 
  #           titlePanel(title=div(img(src="rcglogo.jpg"))),
  #           sidebarLayout(
  #            sidebarPanel(),
  #              mainPanel(p("Disclaimer")))),
  # tabPanel("Home/About", 
  #            mainPanel(img(src="rcglogo.jpg"),
  #                       br(),
  #                       br(),
  #                       p(strong("Welcome to the RDB Overview Tool of the RCG groups"), style = "font-size:30px", align="center"),
  #                       br(style = "font-size:20px"),
  #                       img(src="start_fancy.jpg"),
  #                       br(),
  #                       br(),
  #                       p(strong("Disclaimer"), align="center"),
  #                       p("Tables, plots and graphs presented in this document and overviews are made for the coordination purposes of regional fisheries data collection and are not designed for any other use.", align="center"),
  #                       p("Data used for the outputs are extracted from the Regional Database (RDB) and EU Fleet Register. Due to different aggregations and reporting authorities, data can differ to those e.g. used for assessments or technical reports. Member States (MS) are responsible for uploading latest data and the latest year should be viewed as provisional. Data can be resubmitted by a MS for more than one previous year so there might be differences in earlier year reports if countries update back in time. Responsibility for the quality of the data and comparability to other data sources lies with the MS that provided these data.", align="center"),
  #                       br(),
  #                      p("The respective scripts and calculations used for data displaying are publicly available via the RCG github (https://github.com/ices-eg/RCGs) and subject to change as the work of the group progresses.", align="center")
  #                      ,downloadButton("report", "Download Disclaimer"))
  #           ),  
  
  shinyjs::useShinyjs(),
  
  # get window sizes.
  tags$head(tags$script('var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')),
  
  #get RCG logo on the right
  tagList(
    tags$head(tags$script(type="text/javascript", src = "code.js")),
    
    #navBarPage
    navbarPage( id = "tabs",
                windowTitle= tags$head(
                  tags$link(rel = "icon", type = "image/png", href = "minilogoRCG.png"),
                  tags$title("RCGs Shiny")
                ),
                
                # Main title        
                title = "RCG Overview Tool",
                #theme = shinytheme("sandstone"),
                #inverse = T,
                
                # tabs
                # -----------------------------------
                # Home tab
                # -----------------------------------
                
                tabPanel("Home/About", 
                         fluidRow(
                           column(12, 
                                  #br(),
                                  #HTML('<center><img src="rcglogo.jpg"></center>'),
                                  #br(),
                                  br(),
                                  p(strong("Welcome to the RDB Overview Tool of the RCG groups"), style = "font-size:30px", align="center"),
                                  br(), 
                                  br(),
                                  br(),
                                  HTML('<center><img src="start_fancy.png"></center>'),
                                  br(),
                                  br(),
                                  includeHTML ("data/DescriptionDisclaimer.txt"),
                                  br(),
                                  downloadButton("report", "Download Disclaimer")))
                ),   

# -----------------------------------
# Fisheries overview tab
# -----------------------------------
  
  tabPanel("Fishery overview"),

# -----------------------------------
# Sampling overview tab
# -----------------------------------

    navbarMenu(
    "Sampling overview",

    # -----------------------------------
    # Data explore subtab
    # -----------------------------------
        tabPanel("Inventory tables",tabsetPanel(
      
      type = "tabs", tabPanel("Upload your file",fileInput("file", h3(""),buttonLabel = "Browse",placeholder = "example.Rdata"),p("Upload RDB_All_Regions_YYYY.Rdata, available ", a(href="https://community.ices.dk/ExternalSites/datacollection/Regional%20coordination%20meetings%202017/RCGIntersessionalWork/_layouts/15/start.aspx#/SitePages/HomePage.aspx","here"))),tabPanel("CA inventory",downloadButton(outputId = 'download_filtered_inventorytable_CA',label = "Download the filtered data"), 
                                                                                                                                                                                                                                                                                                                                                                                      DT::dataTableOutput("inventorytable_CA")
      ),tabPanel("SL inventory",downloadButton(outputId = 'download_filtered_innvetorytable_SL',label = "Download the filtered data"), 
                 DT::dataTableOutput("inventorytable_SL")))),
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
             )
             #             ,downloadButton("report", "Generate report")
    ),
    

    # -----------------------------------
    # Interative map subtab
    # -----------------------------------
    
    tabPanel(
      "Interactive map",
      div(class = "outer",tags$head(# Include our custom CSS
        includeCSS("styles.css")),
        # If not using custom CSS, set height of leafletOutput to a number instead of percent
        leafletOutput("map", width = "100%", height = "100%"),
        # Shiny versions prior to 0.11 should use class = "modal" instea#d.
        
        # -----------------------------------
        # user Panel
        # -----------------------------------
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
          checkboxInput("rec", "ICES Rectangles"),
          br(),
          actionButton ("view2", "View")#,
          #downloadButton("report", "Generate report")
        )),
      
      # -----------------------------------
      # plot panel
      # -----------------------------------
      
      absolutePanel(
        id = "controls",
        class = "panel panel-default",
        fixed = TRUE,
        draggable = TRUE,
        bottom = 60,
        left = "auto",
        right = 20,
        top = "auto",
        width = 800,
        height = "auto",
        h2("Sampling plot"),
        plotOutput("plot2")
      )
    )#,# end interactive mapPanel
    
    # -----------------------------------
    # Generate report subtab
    # -----------------------------------
    # 
    # tabPanel("Download report",
    #     downloadButton("report", "Generate report")
    #     )# end of the 
    ), # end navMENU

    ##--------------
    ## tabPanels "with leaflet"
    ##--------------
    
#    tabPanel(
#      "Interactive map",
#      div(
#        class = "outer",
#        
#        tags$head(# Include our custom CSS
#          includeCSS("styles.css")),
#        # If not using custom CSS, set height of leafletOutput to a number instead of percent
#        leafletOutput("map", width = "100%", height = "100%"),
#        
#        # Shiny versions prior to 0.11 should use class = "modal" instea#d.
#        absolutePanel(
#          id = "controls",
#          class = "panel panel-default",
#          fixed = TRUE,
#          draggable = TRUE,
#          bottom = "auto",
#          left = "auto",
#          right = 20,
#          top = 60,
#          width = 330,
#          height = "auto",
#          h2("Sampling explorer"),
#          selectizeInput (
#            "country",
#            "Country",
#            c("All", levels(inventory_ca$LandingCountry)),
#            multiple = TRUE,
#            selected = "All",
#            options = list(plugins = list("remove_button", "drag_drop"))
#          ),
#          selectizeInput (
#            "species",
#            "Species",
#            c("All", levels(inventory_ca$Species)),
#            multiple = TRUE,
#            selected = "All",
#            options = list(plugins = list("remove_button", "drag_drop"))
#          ),
#          selectizeInput (
#            "samtype",
#            "Sampling Type",
#            c("All", levels(inventory_ca$SamplingType)),
#            multiple = TRUE,
#            selected = "All",
#            options = list(plugins = list("remove_button", "drag_drop"))
#          ),
#          selectizeInput (
#            "quarter",
#            "Quarter",
#            c("All", levels(inventory_ca$Quarter)),
#            multiple = TRUE,
#            selected = "All",
#            options = list(plugins = list("remove_button", "drag_drop"))
#          ),
#          selectInput ("N_var2", "Variable", var, multiple = F),
#          actionButton ("view2", "View"),
#          #                   downloadButton("report", "Generate report")
#        ))
#    ),

    
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
               )
               #                        ,downloadButton("report", "Generate report")
             )
    )
    #              ,downloadButton("report", "Generate report")
  ),
  
# -----------------------------------
# Stock overview tab
# -----------------------------------

tabPanel("Stock overview")

##--------------
## tabPanels "with functions"
##--------------

)# end of the taglist          
#)# end Navbarpage             
)# end of fluidPage