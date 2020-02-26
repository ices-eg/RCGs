choroplethMap_func = function(df,
                              var,
                              groupBy,
                              facet,
                              func,
                              type_of_threshold = 'none',
                              value_of_threshold = NA,
                              points_coord,
                              plot_labels = FALSE,
                              saveResults = FALSE,
                              outputPath,
                              Catch_group_name = NA,
                              displayInR = TRUE,
                              addExtraShp = FALSE,
                              extraShp = NA,
                              newVarName = NA,
                              addToTitle = NA,
                              filter_ON = FALSE,
                              filter_column  = NA,
                              filter_type = NA,
                              filter_threshold = NA,
                              filter_func = NA,
                              filter_facet = NA) {
  # df - a data frame
  # var -  a column to be summmarised e.g. var = 'OfficialLandingCatchWeight'
  # groupBy - name of column, by which the grouping should be carried out. e.g. groupBy = 'Area'
  # facet - will be used for facet_wrap, e.g. facet = 'Year'
  # func - function summarising the data: sum,  n_distinct,  e.g. func = 'sum'
  # type_of_threshold - default set to 'none', other options: 'top_n', 'percent'
  # value_of_threshold - set it, if you defined any type_of_threshold
  # Catch_group_name - if NA then all species will be included, other options: demersal/flatfish/smallpelagic/largepelagic
  # points_coord - dataset with coordinates of a variable that was listed first in groupBy parameter, eg Harbour. Must have at least columns called lat, lon and column named the same as the appropriate column in the df
  # plot_labels  - TRUE/FALSE - should the labels of e.g. Harbours be displayed on a map?
  # saveResults - TRUE/FALSE - do you want to save the results?
  # outputPath - path for saving plots and tables
  # displayInR - TRUE/FALSE - should the results be displayed in R (warning: in case of big plots, it takes a longer time to display sth in R, rather than save it into a diretcory)
  #           - so if yout saveResults = TRUE, then it's recommended to set displayInR = FALSE
  # addExtraShp - TRUE/FALSE, TRUE - if you want to use extra layer for your map, e.g. display FAOares on a map of StatisticalRectangles
  # extraShp - set it if you want to use extra layer for your map, e.g. display FAOares on a map of StatisticalRectangles
  # newVarName - set it if you want to rename the var e.g. OfficialLandingCatchWeight -> Landings
  # addToTitle - additional information to the title (e.g. for effort information about filtering vessels <10 or >10)
  # filter_... - used when you want to limit your dataset to e.g. top 5 metiers
  
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
  
  if(filter_ON == TRUE){ # <------- ADD SOME MORE CHECKS
    # this loop is needed - if you want to make a scatterpie for e.g. top 5 metiers. - for top sth different from spatial variable (!= Area, StatisticalRectangle, Harbour,...)
    filter_column_name = filter_column
    filter_column <- as.symbol(filter_column_name)
    #top filtered
    group_func(df,
               var = var_name,
               groupBy = filter_column_name,
               facet = filter_facet,
               func = filter_func,
               type_of_threshold = filter_type,
               value_of_threshold = filter_threshold)-> filteredVariable
    df %>% filter(!!filter_column %in% (filteredVariable[[1]] %>% pull(!!filter_column))) %>% droplevels() -> df 
    
  }
  
  # creating the groupped df
  
  grouping_result = group_func(df, var_name, groupBy_name,groupBy2 = NA, facet = facet_name, func = func_name, type_of_threshold = type_of_threshold, 
                               value_of_threshold =  value_of_threshold, Catch_group_name = Catch_group_name)  
  
  tdf = grouping_result[[1]]
  if (is.null(tdf)) {
    stop('The chosen data set is empty')
  }
  missing_entries = grouping_result[[2]]
  missing_entries2 = missing_entries %>% select(facet, prMissing =pr)
  ############################################################################
  
  # combine dataset with shp
  points_coord %>%  full_join(tdf) -> mdf
  
  mdf %>% mutate(var = !!var,
                 groupBy = !!groupBy,
                 facet = !!facet) %>% 
    filter(!is.na(var))-> mdf
  
  
  
  # add info about records without coordinates 
  mdf %>% as.data.frame() %>% filter(is.na(ID) & !is.na(groupBy)) %>%group_by(facet) %>%  
    summarise(pr = sum(pr), n = n_distinct(groupBy)) %>% select(facet, pr, n)-> missing_value
    missing_value2 = missing_value %>% select(facet, prMissingValue = pr, nMissingValue = n)


  
  # load world map
  m <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Take only areas with geometry
  mdf %>% filter(!is.na(ID)) -> mdf2
  
  # set the limits
  # x/y =  3/2
  limits <- st_buffer(mdf2, dist = 1) %>% st_bbox()
  if(unique(df$Region)!='NSEA'){
  if(abs(limits["xmax"]-limits["xmin"])>(3/2)*abs(limits["ymax"]-limits["ymin"])){
    diff = (2/3*abs(limits["xmax"]-limits["xmin"])-abs(limits["ymax"]-limits["ymin"]))/2
    limits["ymin"]=limits["ymin"]-diff
    limits["ymax"]=limits["ymax"]+diff
  }else if((2/3)*abs(limits["xmax"]-limits["xmin"])<abs(limits["ymax"]-limits["ymin"])){
    diff = (3/2*abs(limits["ymax"]-limits["ymin"])-abs(limits["xmax"]-limits["xmin"]))/2
    limits["xmin"]=limits["xmin"]-diff
    limits["xmax"]=limits["xmax"]+diff 
    }
  }else{
    if(abs(limits["xmax"]-limits["xmin"])>(5/2)*abs(limits["ymax"]-limits["ymin"])){
      diff = (2/5*abs(limits["xmax"]-limits["xmin"])-abs(limits["ymax"]-limits["ymin"]))/2
      limits["ymin"]=limits["ymin"]-diff
      limits["ymax"]=limits["ymax"]+diff
    }else if((2/5)*abs(limits["xmax"]-limits["xmin"])<abs(limits["ymax"]-limits["ymin"])){
      diff = (5/2*abs(limits["ymax"]-limits["ymin"])-abs(limits["xmax"]-limits["xmin"]))/2
      limits["xmin"]=limits["xmin"]-diff
      limits["xmax"]=limits["xmax"]+diff 
    }
  }
  # Set the plot parameters
  
  # title
  if (func_name %in% c('sum')) {
    title = paste(func_name,
                  ' of ',
                  ifelse(is.na(newVarName), var_name, newVarName),
                  ' by ',
                  groupBy_name,
                  sep = '')
  } else{
    title = paste(func_name, ' ', ifelse(is.na(newVarName), var_name, newVarName), ' by ',  groupBy_name, sep = '')
  }
  
  # If Catch_group_name is known
  if(!is.na(Catch_group_name) & Catch_group_name!='NULL'){ title = paste(title, ' (',Catch_group_name, ')', sep ='')}
  # If there is any additional information to the title
  if(!is.na(addToTitle)){ title = paste(title, ' (',addToTitle, ')', sep ='')}
  
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
      ifelse(is.na(newVarName), var_name, newVarName),
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
  if(length(unique(mdf2$facet))==1){ # if only one plot ->  caption under the plot, if more than one -> seperate caption added to the title of each facet
  
    if(nrow(missing_value)>0){
      missing_caption = paste(
        '\n',
        missing_value$n,
        ' ',
        groupBy_name,
        # 's (',
        #  paste0(missing_names, collapse = ' , ') , # add names of units without coordinates
        ' with missing coordinates (', 
        ifelse(missing_value$pr<=0.005 & missing_value$pr >0, '~0',round(missing_value$pr, 2)),
        '% of ',
        ifelse(is.na(newVarName), var_name, newVarName),
        ') - not presented on the map.',
        sep = ''
      )
      message(missing_caption)
    }else{
      missing_caption = ''
    }
    
   
      caption = paste(
      ifelse(nrow(missing_entries)>0, ifelse(missing_entries$pr<=0.005 &missing_entries$pr>0,'~0',round(missing_entries$pr, 2)),0),
      '% of ',
      ifelse(is.na(newVarName), var_name, newVarName),
      ' - reported for missing ',
      groupBy_name, '.',
      missing_caption,
      sep = ''
    )
    
  }else{
    caption = ''

    mdf2 %>% 
      left_join(missing_entries2) %>% 
      left_join(missing_value2) %>% 
      mutate(facet =  paste(facet, 
                            '\n',
                            paste(
                              str_wrap(
                                paste(ifelse(!is.na(prMissing), ifelse(prMissing<=0.005 & prMissing>0, '~0', round(prMissing, 2)),0),
                                      '% of ',
                                      ifelse(is.na(newVarName), var_name, newVarName),
                                      ' - missing ',
                                      groupBy_name, '.',
                                      sep = ''
                                      ),
                                width = 55),
                              sep = '\n'),
                            '\n',
                            ifelse(!is.na(prMissingValue), 
                            paste(
                              str_wrap( 
                                paste(nMissingValue,
                                       ' ',
                                       groupBy_name,
                                       '(', 
                                      ifelse(prMissingValue<=0.005 & prMissingValue>0,'~0',round(prMissingValue, 2)),
                                       '% of ',
                                       ifelse(is.na(newVarName), var_name, newVarName),
                                       ') - missing coordinates',
                                      sep = ''
                                ), 
                                width = 55),
                              sep = '\n'),
                            ''),
                            sep = ' ')
                         ) -> mdf2
  }

  # Make a map
  ggplot() +
    geom_sf(data = mdf2, aes(fill = var) , na.rm = TRUE,
            size = 0.05, color =gray(.8) 
            ) +
    geom_sf(data = points_coord,
            fill = NA ,
            na.rm = TRUE,
            size = ifelse(groupBy_name %in% c('Area','AreaMap', 'FishingGround'),  0.5,  0.02), 
            color =ifelse(groupBy_name %in% c('Area','AreaMap', 'FishingGround'), gray(.3), gray(.8))
            ) +
    scale_fill_viridis_c(
      option = "viridis",
      trans = "sqrt",
      na.value = "aliceblue",
      begin = 1,
      end = 0,
      name = ifelse(is.na(newVarName), var_name, newVarName)
    ) -> plot
  
  if(addExtraShp==TRUE){
    plot+
      geom_sf(data = extraShp,
              fill = NA, 
              na.rm = TRUE,
              size = 0.5,
              color = gray(.3)
      #         )+
      # geom_sf_text(data = extraShp, aes(label = F_CODE),
      #              color = 'grey22',
      #              size = 3,
      #              fontface = "italic",
      #              check_overlap = TRUE
                   )-> plot
  }
    
    plot +
    geom_sf(data = m,  fill = "antiquewhite") +
    coord_sf(
      crs = "+init=epsg:4326",
      xlim = c(unlist(limits["xmin"]), unlist(limits["xmax"])),
      ylim = c(unlist(limits["ymin"]), unlist(limits["ymax"])),
      expand = FALSE
    ) +
    labs(
      title = title,
      x = 'Longitude',
      y = 'Latitude',
      subtitle = subtitle,
      caption = caption
    ) +
    facet_wrap(~facet, ncol =2)+
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
      ),
      strip.text = element_text(size = ifelse(length(unique(mdf2$facet))==1,10,6)),
      axis.text.x = element_text(size = ifelse(length(unique(mdf2$facet))==1,9,7)),
      axis.text.y = element_text(size = ifelse(length(unique(mdf2$facet))==1,9,7))
    ) -> plot
  
  if (plot_labels == TRUE) {
    #display labels on the plot
    plot +
      geom_sf_text(data = mdf2, aes(label = groupBy), 
        color = 'red1',
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
    if (!is.na(Catch_group_name) & Catch_group_name!='NULL') {
      fileName = paste(fileName, Catch_group_name, sep = "")
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
      width = 10,
      height = 10,
      dpi = 300,
      compression = 'lzw'
    )
  }
  
  mdf %>%  select(-var, - groupBy, -facet)-> mdf
  
  if(displayInR==TRUE){
  return(list(mdf, plot)) #should we return all data - mdf (with missing parts) or only the data that were  plotted - mdf2?
  }
  
}

# choroplethMap_func(cl_rcg %>% filter(Year %in% c(2018)),
#                    'OfficialLandingCatchWeight',
#                    'AreaMap',
#                    facet = 'Year',
#                    func = 'sum',
#                    type_of_threshold = 'percent',
#                    value_of_threshold = 100,
#                    Catch_group_name = NA,
#                    points_coord = FAOshp,
#                    plot_labels = TRUE,
#                    newVarName = 'Landings',
#                    addExtraShp = FALSE,
#                    extraShp = FAOshp)
