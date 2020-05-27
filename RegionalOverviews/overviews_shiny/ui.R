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
#library(webshot)

##--------------
## data
##--------------

#load("data/ShinyTest_BigPicture.RData")




# facet <- c("SamplingCountry","FlagCountry","LandingCountry","Year",
#             "Quarter","Species","Area","SamplingType",
#             "Metier","StatisticalRectangle","lat","lon")



##--------------
## ui
##--------------


ui <- bootstrapPage(tags$style(HTML(" body, pre { font-size: 12pt; } * { font-family: Arial,sans-serif }")),
                    tags$style(".shiny-file-input-progress {display: none}"),tags$head(tags$style(HTML(".selectize-input {width: 700px;}"))),
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
  
  #shinyjs::useShinyjs(),
  
  # get window sizes.
  # tags$head(tags$script('var dimension = [0, 0];
  #                       $(document).on("shiny:connected", function(e) {
  #                       dimension[0] = window.innerWidth;
  #                       dimension[1] = window.innerHeight;
  #                       Shiny.onInputChange("dimension", dimension);
  #                       });
  #                       $(window).resize(function(e) {
  #                       dimension[0] = window.innerWidth;
  #                       dimension[1] = window.innerHeight;
  #                       Shiny.onInputChange("dimension", dimension);
  #                       });
  #                       ')),
  # 
  # #get RCG logo on the right
  # tagList(
  #   tags$head(tags$script(type="text/javascript", src = "code.js")),
  #   
    #navBarPage
    navbarPage(theme = shinytheme("cosmo"), collapsible = TRUE,
                id = "tabs",
                
                
                # Main title        
                title = "RCG Overview Tool",
                #theme = shinytheme("sandstone"),
                #inverse = T,
                
                # tabs
                # -----------------------------------
                # Home tab
                # -----------------------------------
                
                tabPanel("Home", 
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
                                  downloadButton("report", "Download Disclaimer",class = 'centerAlign')))
                ),   

# -----------------------------------
# Fisheries overview tab
# -----------------------------------
  
  tabPanel("Input data",align='center',br(),p("Upload RDB_All_Regions_YYYY.Rdata, available ", a(href="https://community.ices.dk/ExternalSites/datacollection/Regional%20coordination%20meetings%202017/RCGIntersessionalWork/_layouts/15/start.aspx#/SitePages/HomePage.aspx","here"),align="center"),fileInput("file", h3(""),buttonLabel = "Browse",placeholder = "example.Rdata"), p("It might take a while",align="center")),

# -----------------------------------
# Sampling overview tab
# -----------------------------------

    navbarMenu(
    "Sampling overview",

    # -----------------------------------
    # Data explore subtab
    # -----------------------------------
    tabPanel("Inventory tables",
             tabsetPanel(
               type = "tabs",
               tabPanel(
                 "CA inventory",
                 align = 'center',
                 br(),
                 downloadButton(outputId = 'download_filtered_inventorytable_CA', label = "Download the filtered dataset"),
                 br(),
                 addSpinner(DT::dataTableOutput("inventorytable_CA"), spin = "circle", color = "grey")
               ),
               tabPanel(
                 "SL inventory",
                 align = 'center',
                 br(),
                 downloadButton(outputId = 'download_filtered_invetorytable_SL', label = "Download the filtered dataset"),
                 br(),
                 addSpinner(DT::dataTableOutput("inventorytable_SL"), spin = "circle", color = "grey")
               )
             )),

    

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
       uiOutput("absolute"),
        # absolutePanel(
        #   id = "controls",
        #   class = "panel panel-default",
        #   fixed = TRUE,
        #   draggable = TRUE,
        #   bottom = "auto",
        #   left = "auto",
        #   right = 20,
        #   top = 60,
        #   width = 400,
        #   height = "auto",
        #   h2("Sampling explorer"),
        #   uiOutput("countryui"),uiOutput("speciesui"),uiOutput("samptypeui"),uiOutput("quarterui"),
        #   selectInput ("N_var2", "Variable", var, multiple = F),
        #   checkboxInput("rec", "ICES Rectangles"),
        #   br(),
        #   actionButton ("view2", "View"),
        #   downloadButton("down", "Generate report"),
        #   br(),
        #   plotOutput("plot2",height=300)
        # )
        ))
    ) # end navMENU

    
  )
  
# -----------------------------------
# Stock overview tab
# -----------------------------------


##--------------
## tabPanels "with functions"
##--------------

)# end of the taglist          
#)# end Navbarpage             
# end of fluidPage