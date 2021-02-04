

shinyUI(bootstrapPage(tags$style(HTML(" body, pre { font-size: 12pt; } * { font-family: Arial,sans-serif }")),
                    tags$style(".shiny-file-input-progress {display: none}"),
                    tags$head(tags$style(HTML(".selectize-input {width: 700px;}"))),

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
        )),
    
    # -----------------------------------
    # Static map subtab
    # -----------------------------------
    
    tabPanel(
      "Static map",
      uiOutput("static")
    ),
    
    # -----------------------------------
    # interative plots subtab
    # -----------------------------------
    
    tabPanel(
      "Interactive plots"
    )
  )# end navMENU

# -----------------------------------
# Stock overview tab
# -----------------------------------

    ) # end Navbarpage
  )# end of bootstrapPage      
)# end of ui