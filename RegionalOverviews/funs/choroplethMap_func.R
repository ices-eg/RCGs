choroplethMap_func = function(df,
                              var,
                              groupBy,
                              func,
                              type_of_threshold = 'none',
                              value_of_threshold = NA,
                              points_coord,
                              plot_labels = TRUE,
                              time,
                              saveResults = FALSE,
                              outputPath,
                              Catch_group = NA) {
  # df - a data frame
  # var -  a column to be summmarised e.g. var = as.symbol('OfficialLandingCatchWeight')
  # groupBy - names of columns, by which the grouping should be carried out. IMPORTANT to write it as groupBy = c(...) e.g. groupBy = c('Harbours', 'HarboursDesc')
  #         - IMPORTANT - on the first place put sth you will be plotting by, eg Harbour
  # func - function summarising the data: sum, n_distinct, e.g. func = as.symbol('sum')
  # type_of_threshold - default set to 'none', other options: 'top_n', 'percent'
  # value_of_threshold - set it, if you defined any type_of_threshold
  # points_coord - shapefile, must have the same column name as in the df, e.g. Area if grooupBy = c('Area',...)
  # plot_labels  - TRUE/FALSE - should the labels of e.g. Harbours be displayed on a map?
  # time = name of column describing time, must be also included into the groupBy parameter, as.symbol('Year')
  #saveResults - TRUE/FALSE - do you want to save the results?
  # outputPath - path for saving plots and tables
  # Catch_group - if NA then all species will be included, other options: demersal/flatfish/smallpelagic/largepelagic
  
  # Marta Suska
  # NMFRI
  # msuska@mir.gdynia.pl
  ################################################################################
  
  require(rlang)
  require(ggplot2)
  require(sf)
  require(rnaturalearth)
  
  source('funs/group_func.R')
  
  # parameters
  
  #var = enquo(var) # this one if the var is set like var = OfficialLandingCatchWeight
  var = quo(UQ(var)) # this one to make it work with RCG_NA_CL_Graphical_details, this one if var is with quotations
  
  #func = enquo(func)  # this one if the var is set like func = sum
  func = quo(UQ(func)) # to make it work with RCG_NA_CL_Graphical_details, this one if func is with quotations
  
  groupBy = parse_quos(groupBy, env = caller_env()) # to make it work with RCG_NA_CL_Graphical_details, this one if groupBy is with quotations
  groupBy = enquo(groupBy)
  groupBy_name = quo_name(eval_tidy(quo(UQ(groupBy)))[[1]])
  
  var_name =  quo_text(var)
  func_name = quo_text(func)
  
  #time = enquo(time) # this one if time is set like time = Year
  time = quo(UQ(time)) # to make it work with RCG_NA_CL_Graphical_details, this one if time is with quotations, time = 'Year'
  
  
  
  # creating the groupped df
  grouping_result =  eval_tidy(quo(
    UQ(group_func)(
      df = df,
      var = !!var,
      groupBy = !!groupBy,
      func = !!func,
      type_of_threshold = type_of_threshold,
      value_of_threshold = value_of_threshold,
      Catch_group = Catch_group
    )
  ))
  tdf = grouping_result[[1]]
  if (is.null(tdf)) {
    stop('The chosen data set is empty')
  }
  missing_entries = grouping_result[[2]]
  
  # combine dataset with shp
  points_coord %>%  full_join(tdf) %>%  filter(!is.na(!!var)) -> mdf
  
  # add info about records without coordinates <------------------ TO DO
  if (nrow(mdf %>% filter(is.na(ID))) > 0) {
    mdf %>% filter(is.na(ID) & !is.na(!!eval_tidy(quo(UQ(groupBy)[[1]])))) %>% select(pr)-> missing_value
    message(paste("Not all records from the dataset have corresponding entry in the shapefile, ",round(unlist(missing_value)), '% ', sep = '' ))
    missing_caption = paste("\n Not all records from the dataset have corresponding entry in the shapefile, ",round(unlist(missing_value)), '% ', sep = '' )
  } else{
    missing_caption = ''
  }
  

  
  # load world map
  m <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Take only areas with geometry
  mdf %>% filter(!is.na(ID)) -> mdf2
  
  # set the limits
  limits <- st_buffer(mdf2, dist = 1) %>% st_bbox()
  
  time = tdf %>% distinct(!!time)
  
  # Set the plot parameters
  
  # title
  if (func_name %in% c('sum')) {
    title = paste(func_name,
                  ' of ',
                  var_name,
                  ' by ',
                  groupBy_name,
                  ', ',
                  time,
                  sep = '')
  } else{
    title = paste(func_name, ' ', var_name, ' by ',  groupBy_name, ', ', time, sep = '')
  }
  
  # If Catch_group is known
  if(!is.na(Catch_group) & Catch_group!='NULL'){ title = paste(title, ' (',Catch_group, ')', sep ='')}
  
  
  
  # subtitle - as the information about used thresholds
  if ((type_of_threshold == 'percent' &
       value_of_threshold == 100) | type_of_threshold == 'none') {
    subtitle = 'All data'
  } else if (type_of_threshold == 'percent') {
    subtitle = paste (
      'Including ',
      groupBy_name,
      's accounting for ',
      value_of_threshold,
      '% of ',
      var_name,
      sep = ""
    )
  } else{
    subtitle = paste('Including top ',
                     value_of_threshold,
                     ' ',
                     groupBy_name,
                     's',
                     sep = "")
  }
  
  # caption - as the inromation about any missingnes
  caption = paste(
    ifelse(nrow(missing_entries) > 0, round(missing_entries$pr, 2), 0),
    '% of ',
    var_name,
    ' were reported for missing ',
    groupBy_name,
    missing_caption,
    sep = ''
  )
  
  
  
  
  # Make a map
  ggplot() +
    geom_sf(data = mdf2, aes(fill = !!var) , na.rm = TRUE) +
    geom_sf(data = points_coord,
            fill = NA ,
            na.rm = TRUE) +
    scale_fill_viridis_c(
      option = "viridis",
      trans = "sqrt",
      na.value = "aliceblue",
      begin = 1,
      end = 0
    ) +
    geom_sf(data = m,  fill = "antiquewhite") +
    coord_sf(
      crs = "+init=epsg:4326",
      xlim = c(limits["xmin"], limits["xmax"]),
      ylim = c(limits["ymin"], limits["ymax"]),
      expand = FALSE
    ) +
    labs(
      title = title,
      x = 'Longitude',
      y = 'Latitude',
      subtitle = subtitle,
      caption = caption
    ) +
    theme_classic() +
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
      panel.grid.major = element_line(
        color = gray(.8),
        linetype = 'dashed',
        size = 0.5
      )
    ) -> plot
  
  if (plot_labels == TRUE) {
    #display labels on the plot
    plot +
      geom_text(
        data = points_coord,
        aes(x = X, y = Y, label := !!eval_tidy(quo(UQ(
          groupBy
        )[[1]]))),
        color = 'grey22',
        size = 3,
        fontface = "italic",
        check_overlap = TRUE
      ) -> plot
  }
  
  if (saveResults == TRUE) {
    fileName =   paste(
      outputPath,
      "/choroplethMap_",
      func_name,
      '_',
      var_name,
      '_',
      groupBy_name,
      '_',
      time,
      '_',
      type_of_threshold,
      '_',
      value_of_threshold,
      sep = ''
    )
    if (!is.na(Catch_group) & Catch_group!='NULL') {
      fileName = paste(fileName, Catch_group, sep = "")
    }
    write.table(
      mdf,
      file = paste(fileName, ".txt", sep = ""),
      sep = '\t',
      dec = '.'
    )
    ggsave(
      paste(fileName, ".tiff", sep = ""),
      units = "in",
      width = 15,
      height = 10,
      dpi = 300,
      compression = 'lzw'
    )
  }
  
  return(list(mdf, plot)) #should we return all data (with missing parts) or only the data that were  plotted?
  
  
}

# 
# choroplethMap_func(
#   cl_rcg,
#   var = as.symbol('OfficialLandingCatchWeight'),
#   groupBy = c('Area', 'Year'),
#   func = as.symbol('sum'),
#   type_of_threshold = 'percent',
#   value_of_threshold = 100,
#   points_coord = FAOshp,
#   plot_labels = FALSE,
#   time = as.symbol('Year'),
#   saveResults = FALSE,
#   outputPath = 'D:/WG/RCG/IntersessionalWork/Github/RCGs/RegionalOverviews'
# )
# 
# choroplethMap_func(
#   cl_rcg,
#   var = as.symbol('OfficialLandingCatchWeight'),
#   groupBy = c('StatisticalRectangle', 'Year'),
#   func = as.symbol('sum'),
#   type_of_threshold = 'percent',
#   value_of_threshold = 100,
#   points_coord = StatRectshp,
#   plot_labels = FALSE,
#   time = as.symbol('Year'),
#   saveResults = FALSE,
#   outputPath = 'D:/WG/RCG/IntersessionalWork/Github/RCGs/RegionalOverviews'
# )
# 
# 

