# Working version

pointsMap_func = function(df,
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
  # var -  a column to be summmarised e.g. var = OfficialLandingCatchWeight
  # groupBy - names of columns, by which the grouping should be carried out. IMPORTANT to write it as groupBy = quos(...) e.g. groupBy = quos(Harbours, HarboursDesc)
  #         - IMPORTANT - on the first place put sth you will be plotting by, eg Harbour
  # func - function summarising the data: sum, n_distinct, e.g. func = sum
  # type_of_threshold - default set to 'none', other options: 'top_n', 'percent'
  # value_of_threshold - set it, if you defined any type_of_threshold
  # points_coord - dataset with coordinates of a variable that was listed first in groupBy parameter, eg Harbour. Must have at least columns called lat, lon and column named the same as the appropriate column in the df
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
  require(mapdata)
  
  source('group_func.R')
  
  # parameters
  var = enquo(var)
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
  
  # add coordinates info
  tdf %>%
    left_join(points_coord) -> mdf
  
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
                     !is.na(mdf$lon),]$lon)+ c(-1, 1)
  ylim = range(mdf[!is.na(mdf$lat) & !is.na(mdf$lon),]$lat) + c(-0.5,+0.5)
  
  # load world map
  require("rnaturalearth")
  m <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Take only rows with coordinates
  mdf %>% filter(!is.na(lon) & !is.na(lat)) -> mdf2
  
  time = mdf2 %>% distinct(!!time)
  
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
  if(!is.na(Catch_group)){ title = paste(title, ' (',Catch_group, ')', sep ='')}
  
  
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
    ifelse(nrow(missing_entries)>0, round(missing_entries$pr, 2),0),
    '% of ',
    var_name,
    ' were reported for missing ',
    groupBy_name,
    missing_caption,
    sep = ''
  )
  
  
  
  
  # make a map
  mdf2 %>%
    arrange(!!var) %>%
    mutate(name = factor(!!eval_tidy(quo(UQ(
      groupBy
    )[[1]])), unique(!!eval_tidy(quo(
      UQ(groupBy)[[1]]
    ))))) %>%
    ggplot() +
    geom_sf(data = m, fill = "antiquewhite") +
    coord_sf( crs = "+init=epsg:4326", xlim =xlim, ylim = ylim)+
    geom_point(
      aes(lon, lat, fill := !!var, size := !!var),
      stroke = FALSE,
      colour = 'black',
      #size = 4,
      shape = 21,
      alpha = 0.8
    ) +
    scale_size(range = c(0, 10), guide = FALSE) +
    viridis::scale_fill_viridis(
      option = "magma",
      # trans = "log",
      begin = 0.8,
      end = 0,
      name = var_name
    )+
    #guides(colour = guide_legend())+
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
     panel.grid.major = element_line(color = gray(.5), linetype ='dashed', size = 0.5)
    ) -> plot
  
  if (plot_labels == TRUE) {
    #display labels on the plot
    plot +
      ggrepel::geom_text_repel(
        data = mdf2,
        aes(lon, lat, label := !!eval_tidy(quo(UQ(
          groupBy
        )[[1]]))),
        box.padding = unit(0.2, "lines"),
        point.padding = unit(0.2, "lines"),
        color = 'black',
        size = 2,
        fontface = 'bold'

      ) -> plot
  }
  
  if(saveResults==TRUE){
  fileName =   paste(outputPath, "/pointsMap_", func_name,'_', var_name, '_', groupBy_name,'_', time, '_',type_of_threshold, '_',value_of_threshold, sep = '')
  if(!is.na(Catch_group)){
    fileName = paste(fileName, Catch_group, sep = "")
  }
  write.table(mdf, file = paste(fileName, ".txt", sep = ""), sep = '\t', dec = '.')
  ggsave(paste(fileName, ".tiff", sep = ""), units="in", width=15, height=10, dpi=300, compression = 'lzw')
  }
  
  return(list(mdf, plot))
}



# TO DO:
# points_coords - should it be a dataset, or a path to a dataset?
# add warning in case of enormous number of points to plot
# df - should it be prepared inside the function, or before running the func?
# add type of plot (by harbour, by (what else could be?))
# assumption that the first parameter in groupBy will be plotted
# add facets (yearly)
# zoom options
# add check for joining df with points_coord - check if in both dataset there is a column with the same name
# where to get Harbours coordinates from?
# ... as the parameter to ggplot
# dopisac sciezke do shapefile
# if saveResults = TRUE -> outputPath musst be known - add check
# xlim and ylim= adjusted to the data, or whole rcg region??