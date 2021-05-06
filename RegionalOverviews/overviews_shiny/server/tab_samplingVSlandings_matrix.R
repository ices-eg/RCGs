# This part is based on the WKBIOPTIM2 scripts - we didn't use the  original scripts,
# just ideas how to present sampling vs landings. 


################################################################################################################
# load CL data
################################################################################################################

cl_data = reactive({
  
  req(input$CLfile)
  
  # load the data
  load(input$CLfile$datapath, envir = .GlobalEnv)
  
  cl_rcg$Region[cl_rcg$Region=="NA"|is.na(cl_rcg$Region)]<-'NATL'
  
  cl_rcg
})

# when loaded print out a message for which region there will be the analysis
output$CLregionMessage = renderText({
  paste('You have uploaded CL data from ', unique(cl_data()$Region), ' region', sep ='')
})

################################################################################################################
# subset sl data according to cl and create combined dataset
################################################################################################################

SvL_master = reactive({
   
  # first  aggregate CS and CL, then combine whole datasets into one and then filter according to the selected parameters
  # but probably it would be better to first filter each file separately then aggregate and combine
  
  req(input$file)
  req(input$CLfile)
  
  # Filter CS only to the Region which is in CL
  sl_data <- as.data.frame(data_list()[[2]][data_list()[[2]]$Region %in% unique(cl_data()$Region),])
  
  # check is factor are not spoiling anything
  sl_data %>% 
    mutate(Quarter=as.numeric(as.character(StartQuarter))) %>% # some problem with factors <----- to do - to be checked
    rename(FishingActivityLvl6=FishingActivityCategoryEuropeanLvl6) %>% 
    group_by(Year,Quarter, Region, FlagCountry,FishingGround, Area, Species, FishingActivityLvl6, VesselLengthCategory) %>% 
    summarise(NoLengthTrips = sum(NoLengthTrips, na.rm =  TRUE),
              NoLength = sum(NoLength),
              WeigthKg = sum(WeigthKg)) -> samplingPart
  
  cl_data() %>%
    filter(Year %in% unique(samplingPart$Year)) %>%  
    group_by(Year, Quarter, Region, FlagCountry,FishingGround, Area, Species, FishingActivityLvl6, VesselLengthCategory ) %>%
    summarise(LandingsT = sum(LandingWeight_ton, na.rm=TRUE))-> LandingsPart
  
  
  samplingPart %>% 
    full_join(LandingsPart)-> samplingVSlandings
  
  samplingVSlandings
  
})

########################################################
# renderUI and update selectInputs
########################################################

# update countries
observe({
  updateSelectInput(session, "flagCountrySvL", choices = c("All",as.character(unique(SvL_master()$FlagCountry))), selected = "All")
})

# update species
varsSvL <- reactive ({
  
  data <- as.data.frame(SvL_master())
  
  if (!("All" %in% input$flagCountrySvL)){
    data <- data[data$FlagCountry %in% input$flagCountrySvL,]
  }
  
 # print(head(data))
  data <- data[, 'Species'] #<------------ to do - add species coding selection as in tab_interactive
  data
})

observe({
  updateSelectInput(session, "speciesSvL", choices =c("All", as.character(unique(varsSvL()))), selected = "All") 
})


########################################################
# renderUI 
########################################################
# FlagCountry, Species, Quarter, SampType

output$SvL_TabMatrix<- renderUI({
  
  req(SvL_master())
  
  fluidRow(
    br(),
    column(3,
           selectizeInput(
             "flagCountrySvL",
             "Country",
             choices = c(""),
             multiple = TRUE,
             options = list(plugins = list("remove_button", "drag_drop"))
           ),
           selectizeInput("speciesSvL","Species",
                          choices = c(""),
                          multiple = TRUE,
                          options = list(plugins = list("remove_button", "drag_drop"))),
           selectizeInput( "quarterSvL", "Quarter",
                           choices = c("All", unique(SvL_master()$Quarter)),
                           multiple = TRUE,
                           selected = "All",
                           options = list(plugins = list("remove_button", "drag_drop"))),
           hr(),
           div(style="display: inline-block;vertical-align:top;", actionButton ("view5", "View")),
    ),
    column(9,
           uiOutput("SvLfilter"),
           plotOutput("SvL_matrix", height = 800), # height = 'auto'
           DT::dataTableOutput("tableCL")
           )
  )
})


################################################################################################################
# Filtered data
################################################################################################################


SvL_masterF = reactive({
  
  req(SvL_master())
  
  data = as.data.frame(SvL_master())
  
  if (!("All" %in% input$speciesSvL )){
    data <- data[data$Species %in% input$speciesSvL,]
  }
  
  if (!("All" %in% input$flagCountrySvL )){
    data <- data[data$FlagCountry %in% input$flagCountrySvL,]
  }
  
  if (!("All" %in% input$quarterSvL )){
    data <- data[data$Quarter %in% input$quarterSvL,]
  }
  
  data
})

########################################################
# render SvL matrix
########################################################

output$SvL_matrix <-renderPlot({
  
  if (input$view5==0) return()
  
  isolate({ 
    if(nrow(SvL_masterF())==0) return({shinyalert("Oops!", "No data for this selection", type = "error")})
    
  SvL_masterF() %>%
    mutate(Gear = str_sub(FishingActivityLvl6, end = 3)) %>% 
    group_by(
      Region,
      #Area,
      Gear,
      Species
    ) %>%
    summarise(NoLengthTrips =sum(NoLengthTrips, na.rm=TRUE),
              LandingsT=sum(LandingsT, na.rm=TRUE)) %>% 
    mutate(NoLengthTrips = ifelse(NoLengthTrips==0, NA, NoLengthTrips))  -> samplingVSlandingsF
  
  ggplot(samplingVSlandingsF, aes(y = factor(Species),
                                  x = factor(Gear))) +        ## global aes
    geom_tile(aes(fill = LandingsT), na.rm = TRUE) +         ## to get the rect filled
    geom_point(aes(
      size =NoLengthTrips),
      shape =1,
      stroke = 1)  +    ## geom_point for circle illusion
    scale_fill_distiller(palette = "Spectral", direction = -1, trans = 'log10', na.value="transparent") +
    #scale_size(range = c(1, 20))+             ## to tune the size of circles
    theme_bw()
  })
  
  
  
}
#, height = function() { # this works but makes plot too big
#  session$clientData$output_SvL_matrix_width
#}
)

########################################################
# help table and print - later to be deleted
########################################################

# output$tableCL  <- DT::renderDT(DT::datatable({SvL_masterF()}
#                                               
#                                               , options = list(
#                                                 pageLength = 20,autoWidth=T,scrollX=TRUE
#                                               ),filter = 'top'
# ))
# 



########################################################

# to do
# - add in the matrix option to define X and Y axes
# - include sampType
# - sort species in the selectInput and matrix
# - display codes instead of latin names?

# barplot and lines e.g. bars = landings and lines = trips sampled 

# possible variables from CS:
# number of trips, number of species measured for length, number of species measured for age

# possible variables from CL
# sum landings

# possibles variables for filterring:
# flagcountry, Area, LandingCountry, Month, Quarter, SampType

# map
# total landings and sampling positions landings sampled weight by area quarter and stat rect(biptim)


# Doubts, problems, errors
# - CL data are divided by region and CS is for all regions, CL just for one 
#     suggest that for now the comparison is possible only by region and for 1 year
# - is it ok to take distinct tripsId from HH instead of TR?
# - error if there are no landings but are sampled trips for a given parameters. Error appears
#     when there is not a single square with landings in the matrix
# - how to add sampType
# - how to make larger plot
# - should I use here  datalist[[2]] or [[3]]
# - should I add sth to the inventory table? tr?

# continue working based on the wkbioptim2 report