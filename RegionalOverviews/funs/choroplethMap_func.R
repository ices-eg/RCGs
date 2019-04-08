choroplethMap_func = function(df,
                              var,
                              groupBy,
                              facet,
                              func,
                              type_of_threshold = 'none',
                              value_of_threshold = NA,
                              points_coord,
                              plot_labels = TRUE,
                              saveResults = FALSE,
                              outputPath,
                              Catch_group = NA,
                              displayInR = TRUE) {
  # df - a data frame
  # var -  a column to be summmarised e.g. var = 'OfficialLandingCatchWeight'
  # groupBy - name of column, by which the grouping should be carried out. e.g. groupBy = 'Area'
  # facet - will be used for facet_wrap, e.g. facet = 'Year'
  # func - function summarising the data: sum,  n_distinct,  e.g. func = 'sum'
  # type_of_threshold - default set to 'none', other options: 'top_n', 'percent'
  # value_of_threshold - set it, if you defined any type_of_threshold
  # Catch_group - if NA then all species will be included, other options: demersal/flatfish/smallpelagic/largepelagic
  # points_coord - dataset with coordinates of a variable that was listed first in groupBy parameter, eg Harbour. Must have at least columns called lat, lon and column named the same as the appropriate column in the df
  # plot_labels  - TRUE/FALSE - should the labels of e.g. Harbours be displayed on a map?
  # saveResults - TRUE/FALSE - do you want to save the results?
  # outputPath - path for saving plots and tables
  # displayInR - TRUE/FALSE - should the results be displayed in R (warning: in case of big plots, it takes a longer time to display sth in R, rather than save it into a diretcory)
  #           - so if yout saveResults = TRUE, then it's recommended to set displayInR = FALSE
  
  # Marta Suska
  # NMFRI
  # msuska@mir.gdynia.pl
  
  ################################################################################
  
  require(rlang)
  require(ggplot2)
  require(sf)
  require(rnaturalearth)
  
  source('funs/group_func.R')
  
  # rename the columns
  var_name = var
  var <- as.symbol(var_name)
  groupBy_name = groupBy
  groupBy <- as.symbol(groupBy_name)
  if(!is.na(facet)){
    facet_name <- facet
    facet = as.symbol(facet)  
  }else{
    facet_name = NA
    facet = NA
  }
  
  func_name = func
  func = eval_tidy(as.symbol(func))
  
  # creating the groupped df
  
  grouping_result = group_func(df, var_name, groupBy_name,groupBy2 = NA, facet_name, func_name, type_of_threshold = type_of_threshold, 
                               value_of_threshold =  value_of_threshold, Catch_group = Catch_group)  
  tdf = grouping_result[[1]]
  if (is.null(tdf)) {
    stop('The chosen data set is empty')
  }
  missing_entries = grouping_result[[2]]
  ############################################################################
  
  # combine dataset with shp
  points_coord %>%  full_join(tdf) -> mdf
  
  mdf %>% mutate(var = !!var,
                 groupBy = !!groupBy,
                 facet = !!facet) %>% 
    filter(!is.na(var))-> mdf
  
  
  
  # add info about records without coordinates <------------------ TO DO
  if (nrow(mdf %>% filter(is.na(ID))) > 0) {
    mdf %>% filter(is.na(ID) & !is.na(groupBy)) %>% select(pr)-> missing_value
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
  
  # Set the plot parameters
  
  # title
  if (func_name %in% c('sum')) {
    title = paste(func_name,
                  ' of ',
                  var_name,
                  ' by ',
                  groupBy_name,
                  sep = '')
  } else{
    title = paste(func_name, ' ', var_name, ' by ',  groupBy_name, sep = '')
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
    geom_sf(data = mdf2, aes(fill = var) , na.rm = TRUE,
            size = 0.05, color =gray(.8) 
            ) +
    geom_sf(data = points_coord,
            fill = NA ,
            na.rm = TRUE,
            size = 0.05, color =gray(.8)
            ) +
    scale_fill_viridis_c(
      option = "viridis",
      trans = "sqrt",
      na.value = "aliceblue",
      begin = 1,
      end = 0,
      name = var_name
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
    facet_wrap(~facet)+
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
        data = mdf2,
        aes(x = X, y = Y, label =groupBy),
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
      ifelse(is.na(facet_name),'', paste(', ', paste0(unique(mdf2$facet), collapse = ","), sep= '')),
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
  
  mdf %>%  select(-var, - groupBy, -facet)-> mdf
  
  if(displayInR==TRUE){
  return(list(mdf, plot)) #should we return all data (with missing parts) or only the data that were  plotted?
  }
  
}

# choroplethMap_func(cl_rcg %>% filter(Year %in% c(2016, 2017)),
#                    'OfficialLandingCatchWeight',
#                    'Area',
#                    facet = 'Year',
#                    func = 'sum',
#                    type_of_threshold = 'none',
#                    value_of_threshold = NA,
#                    Catch_group = NA,
#                    points_coord = FAOshp,
#                    plot_labels = TRUE)

