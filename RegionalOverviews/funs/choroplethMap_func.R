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
  # var -  a column to be summmarised e.g. var = as.symbol('OfficialLandingCatchWeight') or var = OfficialLandingCatchWeight
  # groupBy - names of columns, by which the grouping should be carried out. IMPORTANT to write it as groupBy = quos(...) e.g. groupBy = quos(Harbours, HarboursDesc)
  #         - IMPORTANT - on the first place put sth you will be plotting by, eg Harbour
  # func - function summarising the data: sum, n_distinct, e.g. func = sum
  # type_of_threshold - default set to 'none', other options: 'top_n', 'percent'
  # value_of_threshold - set it, if you defined any type_of_threshold
  # points_coord - shapefile, must have the same column name as in the df, e.g. Area if grooupBy = quos(Area...)
  # plot_labels  - TRUE/FALSE - should the labels of e.g. Harbours be displayed on a map?
  # time = name of column describing time, must be also included into the groupBy parameter
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
  var = quo(UQ(var)) # to make it work with RCG_NA_CL_Graphical_details, this one if var is qith quotations
  func = enquo(func)
  groupBy = enquo(groupBy)
  groupBy_name = quo_name(eval_tidy(quo(UQ(groupBy)))[[1]])
  var_name =  quo_text(var)
  func_name = quo_text(func)
  time = enquo(time)
  
  
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
  if (sum(is.na(mdf$geometry)) > 0) {
    message("Not all records from the dataset have corresponding entry in the shapefile")
    missing_caption = 'Not all records from the dataset have corresponding entry in the shapefile'
  } else{
    missing_caption = ''
  }
  
  # set the limits
  limits <- st_buffer(mdf, dist = 1) %>% st_bbox()
  
  # load world map
  m <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Take only areas with geometry
  mdf %>% filter(!is.na(geometry)) -> mdf2
  
  time = tdf %>% distinct(!!time)
  
  # Set the plot parameters
  
  # title
  if (func_name %in% c('sum')) {
    title = paste(func_name,
                  ' of ',
                  var_name,
                  ' per ',
                  groupBy_name,
                  ', ',
                  time,
                  sep = '')
  } else{
    title = paste(func_name, ' ', var_name, ' per ',  groupBy_name, ', ', time, sep = '')
  }
  
  # If Catch_group is known
  if (!is.na(Catch_group)) {
    title = paste(title, ' (', Catch_group, ')', sep = '')
  }
  
  
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
    scale_alpha_continuous(range = c(0.35, 0.9))+
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
    if (!is.na(Catch_group)) {
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
  
  return(list(mdf, plot))
  
  
}


# choroplethMap_func(
#   cl_rcg,
#   var = as.symbol('OfficialLandingCatchWeight'),
#   groupBy = quos(Area, Year),
#   func = sum,
#   type_of_threshold = 'percent',
#   value_of_threshold = 100,
#   points_coord = FAOshp,
#   plot_labels = FALSE,
#   time = Year,
#   saveResults = FALSE,
#   outputPath = 'D:/WG/RCG/IntersessionalWork/Github/RCGs/RegionalOverviews'
# )



