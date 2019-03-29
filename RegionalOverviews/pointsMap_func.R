# Working version

pointsMap_func = function(df,
                          var,
                          groupBy,
                          func,
                          threshold_type = 'none',
                          threshold = NA,
                          points_coord,
                          plot_labels = TRUE,
                          time 
                          ){
  # df - a data frame
  # var -  a column to be summmarised e.g. var = OfficialLandingCatchWeight
  # groupBy - names of columns, by which the grouping should be carried out. IMPORTANT to write it as groupBy = quos(...) e.g. groupBy = quos(Harbours, HarboursDesc)
  #         - IMPORTANT - on the first place put sth you will be plotting by, eg Harbour
  # func - function summarising the data: sum, n_distinct, e.g. func = sum
  # threshold_type - default set to 'none', other options: 'top_n', 'percent'
  # threshold - set it, if you defined any threshold_type
  # points_coord - dataset with coordinates of a variable that was listed first in groupBy parameter, eg Harbour. Must have at least columns called lat, lon and column named the same as the appropriate column in the df
  # plot_labels  - TRUE/FALSE - should the labels of e.g. Harbours be displayed on a map?
  # time = name of column describing time, must be also included into the groupBy parameter
  
  
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
  grouping_result =  eval_tidy(quo(UQ(group_func)(df = df, var = !!var,  groupBy = !!groupBy, func = !!func, threshold_type = threshold_type, threshold = threshold)))
  tdf =grouping_result[[1]]
  if(is.null(tdf)){
    stop('The chosen data set is empty')
  }
  missing_entries = grouping_result[[2]]

    # add coordinates info
  tdf %>%
    left_join(points_coord)->mdf
  
  # add info about records without coordinates
  if(sum(is.na(mdf$lat))!=0 | sum(is.na(mdf$lon))!=0){
    
   mdf %>% filter(is.na(lat)|is.na(lon)) -> missing
   missing %>% summarise(!!var := sum(!!var)) -> missing_value
   mdf %>% summarise(!!var := sum(!!var)) -> value
   missing %>% select(!!eval_tidy(quo(UQ(groupBy)))[[1]]) %>% distinct() %>% unlist() -> missing_names
   
   missing_caption = paste('\n', missing_names %>% nrow, ' top ',groupBy_name,'s (',
           paste0(missing_names, collapse = ' , ') , 
           ') with missing coordinates were not presented on the map. This accounted for ', 
     round(missing_value/value*100), '% of ', var_name, ' of top ', groupBy_name ,'s', sep = '')
   message(missing_caption)
   
   if(length(missing_names)>10){
     missing_caption = paste('\n', missing_names %>% nrow, ' top ',groupBy_name,'s with missing coordinates were not presented on the map. This accounted for ', 
                             round(missing_value/value*100), '% of ', var_name, ' of top ', groupBy_name ,'s', sep = '')
   }

  }else{
   missing_caption = '' 
  }

  # set the limits
  xlim = range(mdf[!is.na(mdf$lat) &
                    !is.na(mdf$lon), ]$lon)
  ylim = range(mdf[!is.na(mdf$lat) & !is.na(mdf$lon), ]$lat)
  
  # load world map
  m <-
    map_data("worldHires",
             xlim =  xlim + c(-1, 1),
             ylim = ylim + c(-0.5, +0.5))
  
  # Take only rows with coordinates
  mdf %>% filter(!is.na(lon) & !is.na(lat))->mdf
  
  time = mdf %>% distinct(!!time)
  
  # Set the plot parameters 
  
  # title
  if(func_name %in% c('sum')){
    title = paste(func_name, ' of ', var_name, ' per ',  groupBy_name, ', ',time, sep = '')  
  }else{
    title = paste(func_name, ' ', var_name, ' per ',  groupBy_name, ', ',time, sep = '')  
  }
  
  
  # subtitle - as the information about used thresholds
  if((threshold_type=='percent' & threshold==100) | threshold_type=='none'){
    subtitle = 'All data'
  }else if(threshold_type=='percent'){
    subtitle = paste ('Including ',groupBy_name, 's accounting for ', threshold, '% of ', var_name, sep = "")
  }else{
    subtitle = paste('Including top ', threshold,' ', groupBy_name, 's', sep = "")
  }
  
  # caption - as the inromation about any missingnes
    caption = paste(round(missing_entries$pr,2), '% of ', var_name, ' were reported for missing ',groupBy_name,
                    missing_caption, sep = '')
  

  
  
  # make a map
  mdf %>% 
    arrange(!!var) %>% 
    mutate(name =factor(!!eval_tidy(quo(UQ(groupBy)[[1]])), unique(!!eval_tidy(quo(UQ(groupBy)[[1]]))))
           ) %>% 
  ggplot() +
    geom_polygon(data = m,
                 aes(long, lat, group = group),
                 fill = 'white', color = 'grey') +
    coord_quickmap(xlim = xlim, ylim = ylim)+
    geom_point(aes(lon, lat, fill := !!var),
               stroke = FALSE,
               colour = 'black',
               size = 4,
               shape = 21,
               alpha = 0.9
               )+
    guides(colour = guide_legend())+
    labs(title = title,
         x = 'Longitude',
         y = 'Latitude',
         subtitle = subtitle,
         caption = caption)+
    theme_classic() +
    theme(
      text = element_text(color = "#22211d"),
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      legend.background = element_rect(fill = "#ffffff", color = NA),
      panel.border = element_rect(colour = "black", fill=NA, size=1.5)
    )->plot

  if(plot_labels == TRUE){ # display labels on the plot
  plot +
    ggrepel::geom_text_repel(
      data = mdf,
      aes(lon, lat, label := !!eval_tidy(quo(UQ(groupBy)[[1]]))),
      box.padding = unit(0.2, "lines"),
      point.padding = unit(0.2, "lines"),
      size = 3
    )->plot
  }
  
  if(func_name != 'n_distinct'){
    plot = plot+
      viridis::scale_fill_viridis(option = "magma",
                                  trans = "log", 
                                  begin = 1, end =0,
                                  name = "")
  }else{
    plot = plot+
      viridis::scale_fill_viridis(option = "magma",
                                  begin = 1, end =0,
                                  name = "")
  }
 
  
  
  return(list(mdf, plot))
  }




# example
# pointsMap_func(CL_2014_NSEA, var = OfficialLandingCatchWeight,  groupBy=quos(Harbour, Year), func = sum, threshold_type = 'percent',threshold = 95,
#                points_coord = Harbours, plot_labels = FALSE, time = Year)


# TO DO:
# points_coords - should it be a dataset, or a path to a dataset?
# add warning in case of enormous number of points to plot
# df - should it be prepared inside the function, or before running the func?
# add type of plot (by harbour, by (what else could be?))
# add path for saving the maps
# make better legend
# assumption that the first parameter in groupBy will be plotted - how to avoid it
# add facets (yearly)
# zoom options
# add check for joining df with points_coord - check if in both dataset there is a column with the same name
# where to get Harbours coordinates from?
# ... as the parameter to ggplot