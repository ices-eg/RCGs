scatterpieMap_func = function(df,
                              var,
                              groupBy,
                              plotBy,
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
  # plotBy - name of column by which a pie should be divided. e. g. FlagCountry
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
  
  groupplotBy = parse_quos(c(groupBy, plotBy), env = caller_env())
  groupplotBy = enquo(groupplotBy)
  
  plotBy =parse_quos(plotBy, env = caller_env())
  
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
  
  # adding info about plotBy
  plotby_result =  eval_tidy(quo(
    UQ(group_func)(
      df = df,
      var = !!var,
      groupBy = !!groupplotBy,
      func = !!func,
      type_of_threshold = 'none',
      Catch_group = Catch_group
    )
  ))
  
  tdf2 = plotby_result[[1]]
  if (is.null(tdf2)) {
    stop('The chosen data set is empty')
  }
  missing_entries2 = plotby_result[[2]]

  tdf %>% select(-!!var) %>% left_join(tdf2) %>%   left_join(points_coord)->mdf
 
  # add info about records without coordinates
  if (sum(is.na(mdf$lat)) != 0 | sum(is.na(mdf$lon)) != 0) {
    mdf %>% filter(is.na(lat) | is.na(lon)) -> missing
    missing %>% summarise(!!var := sum(!!var)) -> missing_value
    mdf %>% summarise(!!var := sum(!!var)) -> value
    missing %>% select(!!eval_tidy(quo(UQ(groupBy)))[[1]]) %>% distinct() %>% unlist() -> missing_names
    
    missing_caption = paste(
      '\n',
      length(missing_names),
      ' top ',
      groupBy_name,
      # 's (',
      #  paste0(missing_names, collapse = ' , ') ,
      ' with missing coordinates were not presented on the map. This accounted for ',
      round(missing_value / value * 100),
      '% of ',
      var_name,
      ' of top ',
      groupBy_name ,
      's',
      sep = ''
    )
    message(missing_caption)
    
    if (length(missing_names) > 10) {
      missing_caption = paste(
        '\n',
        length(missing_names),
        ' top ',
        groupBy_name,
        's with missing coordinates were not presented on the map. This accounted for ',
        round(missing_value / value * 100),
        '% of ',
        var_name,
        ' of top ',
        groupBy_name ,
        's',
        sep = ''
      )
    }
    
  } else{
    missing_caption = ''
  }
  
  
  # set the limits
  xlim = range(mdf[!is.na(mdf$lat) &
                     !is.na(mdf$lon),]$lon)+ c(-5, 5)
  ylim = range(mdf[!is.na(mdf$lat) & !is.na(mdf$lon),]$lat) + c(-4,+4)
  
  # Take only rows with coordinates
  mdf %>%  filter(!is.na(lat) & !is.na(lon)) %>% filter(lon>=-180 & lon <= 180 & lat >= -90 & lat <= 90)-> mdf2
  time = mdf2 %>% distinct(!!time)  
  
  # load world map
  m <- ne_countries(scale = "medium", returnclass = "sf")
  
  
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
  
  
  # make a map
  
  # How to avoid pies distortions?
  #https://stackoverflow.com/questions/51398344/r-pie-charts-distorted-when-adding-to-projected-map-using-ggplot
  
  
  
  unique_bys = mdf2  %>%  distinct(!!eval_tidy(quo(UQ(plotBy)))[[1]]) %>% nrow()

  radius =0.3
  radiusMultiply = ifelse(groupBy_name %in% c('Area'), 4, ifelse(groupBy_name %in% c('Harbour', 'LandingCountry', 'FlagCountry'), 3, 1.5 ))
  pie.list <- mdf2%>%
    select(lon, lat, !!eval_tidy(quo(UQ(groupBy)))[[1]], !!eval_tidy(quo(UQ(plotBy)))[[1]], !!eval_tidy(quo(UQ(groupBy)))[[2]], !!var) %>%  
    spread(!!eval_tidy(quo(UQ(plotBy)))[[1]], !!var, fill =0) %>%
  mutate(Total = rowSums(.[(ncol(.)-unique_bys+1):ncol(.)]))  %>%
  gather(!!eval_tidy(quo(UQ(plotBy)))[[1]], !!var, -lon, -lat, -!!eval_tidy(quo(UQ(groupBy)))[[1]],-!!eval_tidy(quo(UQ(groupBy)))[[2]],  -Total) %>%
    tidyr::nest(!!eval_tidy(quo(UQ(plotBy)))[[1]], !!var) %>%
    # make a pie chart from each row, & convert to grob
    mutate(pie.grob = purrr::map(data,
                                 function(d) ggplotGrob(ggplot(d,
                                                               aes(x = 1, y = !!var, fill = !!eval_tidy(quo(UQ(plotBy)))[[1]])) +
                                                          # scale_fill_manual(values = c("BEL" = "#A6CEE3", "CHA"="#1F78B4","DEU" =  "#B2DF8A","DNK" =  "#33A02C","ENG" =  "#FB9A99","ESP" =  "#4000FF", 
                                                          #                                "EST"="#FDBF6F","FIN" =  "#FF7F00","FRA" =  "#CAB2D6","FRO" =  "#6A3D9A","GBR" =  "#E5C494","IRL" =  "#B15928", 
                                                          #                                "ISL" =  "#FDDAEC","LTU" =  "#E7298A","LVA" =  "#FFFFCC","MAR" =  "#FFED6F","NIR" =  "#F2F2F2","NLD" =  "#AAAAAA", 
                                                          #                                "NOR" = "#666666", "POL" = "#FF0000", "PRT" = "#FFFF00", "SCT" = "#00FFFF", "SWE" = "#8000FF", "WLS" = "#00FF40"))+
                                                          geom_col(color = "black",
                                                                   show.legend = FALSE) +
                                                          coord_polar(theta = "y") +
                                                          theme_void()))) %>%
    # convert each grob to an annotation_custom layer. I've also adjusted the radius
    # value to a reasonable size (based on my screen resolutions).
    mutate(radius = Total/max(Total, na.rm = TRUE)*radiusMultiply) %>%
    rowwise() %>%
    #mutate(radius = radius*4) %>%
    mutate(subgrob = list(annotation_custom(grob = pie.grob,
                                            xmin = lon - radius, xmax = lon + radius,
                                            ymin = lat - radius, ymax = lat + radius))) 
  
if(groupBy_name=='Area'){
  ggplot()+
     geom_sf(data = points_coord, fill = NA , na.rm = TRUE)->p
}else{
  ggplot()->p
}
  
    p+
      geom_sf(data = m,  fill = "antiquewhite")+
      #geom_sf(data = st_as_sf(mdf), aes(fill = !!eval_tidy(quo(UQ(groupBy)))[[1]]) , na.rm = TRUE)+
    coord_sf( crs = "+init=epsg:4326",
              xlim =xlim,
              ylim = ylim,
              expand = FALSE)+
      # scale_fill_manual(values = c("BEL" = "#A6CEE3", "CHA"="#1F78B4","DEU" =  "#B2DF8A","DNK" =  "#33A02C","ENG" =  "#FB9A99","ESP" =  "#4000FF", 
      #                                "EST"="#FDBF6F","FIN" =  "#FF7F00","FRA" =  "#CAB2D6","FRO" =  "#6A3D9A","GBR" =  "#E5C494","IRL" =  "#B15928", 
      #                                "ISL" =  "#FDDAEC","LTU" =  "#E7298A","LVA" =  "#FFFFCC","MAR" =  "#FFED6F","NIR" =  "#F2F2F2","NLD" =  "#AAAAAA", 
      #                                "NOR" = "#666666", "POL" = "#FF0000", "PRT" = "#FFFF00", "SCT" = "#00FFFF", "SWE" = "#8000FF", "WLS" = "#00FF40"), name = "")+
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
    )->p


  p +
    #Optional. this hides some tiles of the corresponding color scale BEHIND the
    #pie charts, in order to create a legend for them
    geom_tile(data = mdf2%>%
                select(lon, lat, !!eval_tidy(quo(UQ(groupBy)))[[1]], !!eval_tidy(quo(UQ(plotBy)))[[1]], !!var) %>%
                spread(!!eval_tidy(quo(UQ(plotBy)))[[1]], !!var, fill =0) %>%
                gather(!!eval_tidy(quo(UQ(plotBy)))[[1]], !!var, -lon, -lat, -!!eval_tidy(quo(UQ(groupBy)))[[1]]),
              aes(x = lon,  y = lat, fill = !!eval_tidy(quo(UQ(plotBy)))[[1]]),
              color = "black", width = 0.01, height = 0.01,
              inherit.aes = FALSE) +

    pie.list$subgrob -> plot

  if (plot_labels == TRUE) {
    #display labels on the plot
    plot +
      geom_text(
        data = mdf2, # zdecydowac co tu wstawic, jak mdf2 tomusi byc unique, jak points_coord - to dla portow nie bedzie dzialalo
        aes(x = lon, y = lat, label := !!eval_tidy(quo(UQ(
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

  return(list(mdf2, plot))
}

# scatterpieMap_func(
#   cl_rcg,
#   var = as.symbol('OfficialLandingCatchWeight'),
#   groupBy = c('Harbour', 'Year'),
#   plotBy = c('FlagCountry'),
#   func = as.symbol('sum'),
#   type_of_threshold = 'percent',
#   value_of_threshold = 90,
#   points_coord = Harbours,
#   plot_labels = FALSE,
#   time = as.symbol('Year'),
#   saveResults = FALSE,
#   outputPath = 'D:/WG/RCG/IntersessionalWork/Github/RCGs/RegionalOverviews'
# )

# scatterpieMap_func(
#   cl_rcg,
#   var = as.symbol('OfficialLandingCatchWeight'),
#   groupBy = c('Area', 'Year'),
#   plotBy = c('FlagCountry'),
#   func = as.symbol('sum'),
#   type_of_threshold = 'percent',
#   value_of_threshold = 90,
#   points_coord = FAOshp,
#   plot_labels = FALSE,
#   time = as.symbol('Year'),
#   saveResults = FALSE,
#   outputPath = 'D:/WG/RCG/IntersessionalWork/Github/RCGs/RegionalOverviews'
# )