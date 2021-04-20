

shinyUI(
  # bootstrapPage(tags$style(HTML(" body, pre { font-size: 12pt; } * { font-family: Arial,sans-serif }")),
  #                   tags$style(".shiny-file-input-progress {display: none}"),
  #                   tags$head(tags$style(HTML(".selectize-input {width: 700px;}"))),
  

    #navBarPage
    navbarPage(
               theme = "style.css",
               collapsible = TRUE,
               id = "navbar",
               useShinyjs(),
               fluid = TRUE,
               #inverse = TRUE,
               #title = "RCG Overview Tool",
               windowTitle= tags$head(
                 tags$link(rel = "icon", type = "image/png", href = "minilogoRCG.png"),
                 tags$title("RCGshiny")
               ),
               position = "fixed-top",
               header = tags$style(
                  ".navbar-right { float: right !important;}",
                  "body {padding-top: 55px;}"),

                # tabs
                # -----------------------------------
                # Home tab
                # -----------------------------------
                
                tabPanel(id ="tabHome", title = introBox(icon("home")),
                         #includeHTML('www/home.html')
                         # fluidRow(
                         #   column(12,
                         #          #br(),
                         #          #HTML('<center><img src="rcglogo.jpg"></center>'),
                         #          #br(),
                         #          br(),
                         #          p(strong("Welcome to the RDB Overview Tool of the RCG groups"), style = "font-size:30px", align="center"),
                         #          br(),
                         #          br(),
                         #          HTML('<center><img src="start_fancy.png"></center>'),
                         #          br(),
                         #          br(),
                         #          includeHTML ("data/DescriptionDisclaimer.txt"),
                         #          br(),
                         #          downloadButton("report", "Download Disclaimer",class = 'centerAlign'))
                         #   )
                         fluidRow(
                           column(12,align="center",
                                  img( src="RCGs_logo.png", height=150)
                           )),
                         br(), 
                         fluidRow(
                           column(12,align="center",
                                  includeHTML('www/home.html') 
                           )),
                         br(), 
                         fluidRow(
                           column(12, align="center",
                                  div(style="display: inline-block;",class= "image", img(id="inputID", src="input_logo_4.png", height=300, style="cursor:pointer;")),
                                  div(style="display: inline-block;",class= "image", img(id ="inventID", src="inventory_logo_4.png", height=300, style="cursor:pointer;")),
                                  div(style="display: inline-block;",class= "image", img(id ="mapIntID", src="mapInt_logo_4.png", height=300, style="cursor:pointer;")),
                                  div(style="display: inline-block;",class= "image", img(id ="mapStatID", src="mapStat_logo_4.png", height=300,style="cursor:pointer;")),
                                  div(style="display: inline-block;",class= "image", img(id ="plotID", src="plots_logo_4.png", height=300,style="cursor:pointer;"))

                         )
                         )
                         
                ),   

# -----------------------------------
# Fisheries overview tab
# -----------------------------------
  
  tabPanel(id = "tabInput", "Input data",align='center',br(),p("Upload RDB_All_Regions_YYYY.Rdata, available ", a(href="https://community.ices.dk/ExternalSites/datacollection/Regional%20coordination%20meetings%202017/RCGIntersessionalWork/_layouts/15/start.aspx#/SitePages/HomePage.aspx","here"),align="center"),fileInput("file", h3(""),buttonLabel = "Browse",placeholder = "example.Rdata"), p("It might take a while",align="center"),
           add_busy_spinner(spin = "scaling-squares", color = "grey", timeout = 5, position = "top-right", margins = c(55,20))
  ),

# -----------------------------------
# Sampling overview tab
# -----------------------------------

    navbarMenu(
    "Sampling overview",

    # -----------------------------------
    # Data explore subtab
    # -----------------------------------
    tabPanel(id="tabInventory", "Inventory tables",
             tabsetPanel(
               type = "tabs",
               tabPanel(
                 "CA inventory",
                 align = 'center',
                 br(),
                 downloadButton(outputId = 'download_filtered_inventorytable_CA', label = "Download the filtered dataset"),
                 br(),
                 #addSpinner(DT::dataTableOutput("inventorytable_CA"), spin = "circle", color = "grey")
                 DT::dataTableOutput("inventorytable_CA"),
                 add_busy_spinner(spin = "scaling-squares", color = "grey", timeout = 5, position = "top-right", margins = c(55,20))
               ),
               tabPanel(
                 "SL inventory",
                 align = 'center',
                 br(),
                 downloadButton(outputId = 'download_filtered_invetorytable_SL', label = "Download the filtered dataset"),
                 br(),
                 #addSpinner(DT::dataTableOutput("inventorytable_SL"), spin = "circle", color = "grey")
                 DT::dataTableOutput("inventorytable_SL"),
                 add_busy_spinner(spin = "scaling-squares", color = "grey", timeout = 5, position = "top-right", margins = c(55,20))
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
       useShinyalert(),
       add_busy_spinner(spin = "scaling-squares", color = "grey", timeout = 5, position = "top-right", margins = c(55,20))
        )),
    
    # -----------------------------------
    # Static map subtab
    # -----------------------------------
    
    tabPanel(
      "Static map",
      useShinyalert(),
      uiOutput("static"),
      add_busy_spinner(spin = "scaling-squares", color = "grey", timeout = 5, position = "top-right", margins = c(55,20))
    ),
    
    # -----------------------------------
    # interative plots subtab
    # -----------------------------------
    
    tabPanel(
      "Interactive plots", 
        uiOutput("summary"),
        add_busy_spinner(spin = "scaling-squares", color = "grey", timeout = 5, position = "top-right", margins = c(55,20))
      
    )
  )# end navMENU

# -----------------------------------
# Stock overview tab
# -----------------------------------

    ) # end Navbarpage
  #)# end of bootstrapPage      
)# end of ui