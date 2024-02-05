choroplethMap_func2 = function(df,
                              var_name,
                              var_spatial_name,
                              facet_name,
                              spatial_dataset_name,
                              spatial_dataset_var_name,
                              spatial_dataset_labels = FALSE,
                              saveResults = FALSE,
                              outputPath,
                              displayInR = TRUE,
                              addExtraShp = FALSE,
                              extraShp_dir = NA,
                              var_name_new = NA,
                              addToTitle = NA,
                              RCGregion = NA
                              ) {

  
  # Marta Szymańska
  # NMFRI
  # msuska@mir.gdynia.pl
  
  ##############################################################################
  # add here parameters description! <-------------------------------------------------------------------------------------- to do 
  ##############################################################################
  
######################################
# load packages
######################################

  require(rlang)
  require(ggplot2)
  require(sf)
  require(rnaturalearth)

# and necessary datasets
# load world map
ne_countries <- ne_countries(scale = "medium", returnclass = "sf")

######################################
# check if all mandatory parameters are given
######################################

if (is.null(df)) {
  stop('The chosen data set is empty')
}


if(!var_name %in% colnames(df)) {
  stop(paste(
    'The given column --->',
    var_name,
    '<--- is not present in the df dataset'
  ))
}

if(!var_spatial_name %in% colnames(df)) {
  stop(paste(
    'The given column --->',
    var_spatial_name,
    '<--- is not present in the df dataset'
  ))
}

if(!is.na(facet_name) & !facet_name %in% colnames(df)) {
  stop(paste(
    'The given column --->',
    facet_name,
    '<--- is not present in the df dataset'
  ))
}

if(!exists(spatial_dataset_name)){
  stop(paste(
    spatial_dataset_name,
    '<--- was not found'
  ))
}

######################################
# transform names into symbols - to use it inside tidyverse
######################################

  var <- as.symbol(var_name)
  var_spatial <- as.symbol(var_spatial_name)

  spatial_dataset <- eval_tidy(as.symbol(spatial_dataset_name))
  spatial_dataset_var <- as.symbol(spatial_dataset_var_name)
  
  if(!is.na(facet_name)){
    facet = as.symbol(facet_name)  
  }else{
    facet = NA
  }

  
######################################
# percentage of var with missing var_spatial info
######################################/

  df %>% 
    filter(is.na(!!var_spatial) | !!var_spatial==-9) %>% 
    summarise(missing_var_spatial_pr = sum(pr))->missing_var_spatial

######################################
# combine df with shp
######################################
  
  spatial_dataset %>%  full_join(df, join_by(!!spatial_dataset_var == !!var_spatial), keep =TRUE) -> df_spatial
  
  # rename variables so that it works for any parameters
  df_spatial %>% rename(var = !!var,
                 var_spatial = !!var_spatial,
                 facet = !!facet) %>%
    mutate(var_name = var_name,
           var_spatial_name = var_spatial_name,
           facet_name = facet_name) %>% 
    filter(!is.na(var))-> df_spatial # take only rows with given var (e.g CLscientificWeight). 
  # The information about number of rows with missing var is given on the data prep lvl
  
  
  # add info about records without with given var_spatial but shp  missing 
  df_spatial %>%  filter((st_is_empty(.) | is.na(geometry)) & !is.na(var_spatial)) %>% as.data.frame() %>%group_by(facet) %>%  
    summarise(missing_shp_pr = sum(pr), missing_shp_n_distinct = n_distinct(var_spatial)) %>% 
    select(facet, missing_shp_pr, missing_shp_n_distinct)-> missing_shp
  # should we inform the users which var_spatial were not found in the shp?

######################################
# prepare the dataset to make a map
######################################
  
  # Take only areas with geometry
  df_spatial %>%  filter(!st_is_empty(.) & !is.na(geometry))  -> df_spatial_toMap
  
######################################
# set the limits, to have appropriate map dimension
######################################
  
  limits <- st_buffer(df_spatial_toMap, dist = 1) %>% st_bbox()
  
  if(!is.na(RCGregion) & RCGregion =='NSEA'){
    if(abs(limits["xmax"]-limits["xmin"])>(5/2)*abs(limits["ymax"]-limits["ymin"])){
      diff = (2/5*abs(limits["xmax"]-limits["xmin"])-abs(limits["ymax"]-limits["ymin"]))/2
      limits["ymin"]=limits["ymin"]-diff
      limits["ymax"]=limits["ymax"]+diff
    }else if((2/5)*abs(limits["xmax"]-limits["xmin"])<abs(limits["ymax"]-limits["ymin"])){
      diff = (5/2*abs(limits["ymax"]-limits["ymin"])-abs(limits["xmax"]-limits["xmin"]))/2
      limits["xmin"]=limits["xmin"]-diff
      limits["xmax"]=limits["xmax"]+diff 
    }
  }else{   # x/y =  3/2
    if(abs(limits["xmax"]-limits["xmin"])>(3/2)*abs(limits["ymax"]-limits["ymin"])){
      diff = (2/3*abs(limits["xmax"]-limits["xmin"])-abs(limits["ymax"]-limits["ymin"]))/2
      limits["ymin"]=limits["ymin"]-diff
      limits["ymax"]=limits["ymax"]+diff
    }else if((2/3)*abs(limits["xmax"]-limits["xmin"])<abs(limits["ymax"]-limits["ymin"])){
      diff = (3/2*abs(limits["ymax"]-limits["ymin"])-abs(limits["xmax"]-limits["xmin"]))/2
      limits["xmin"]=limits["xmin"]-diff
      limits["xmax"]=limits["xmax"]+diff 
    }
  }

######################################
# set the map parameters
######################################  
# now there is only one renaming parameter in the function var_name_new
# if you need more to e.g. instead of 'CLstatisticalRectangle' to have 'Statistical rectangle'
# it should be  added as a new parameter to the function. It was not done yet <-------------------------- to do
  
# TITLE
  
  title = paste(ifelse(!is.na(var_name_new), var_name_new, var_name),
                ' by ',
                var_spatial_name,
                sep = ''
                )
  
  if (unique(df_spatial_toMap$analysis_type) == 'sum') {
    title  = paste('Sum',
                   ' of '
                   , title) 
  }else{
    title = paste(unique(df_spatial_toMap$analysis_type) ,
                  ' ',
                  title)
  }
  
  # If there is any additional information to the title
  if(!is.na(addToTitle)){ title = paste(title, ' (',addToTitle, ')', sep ='')}

# SUBTITLE  
# subtitle - as the information about used thresholds <---------------------------------------------------to do
  subtitle = NA
 
# CAPTION 
# caption - as the information about any missing variables
# if only one plot ->  caption under the plot, if more than one, seperate caption added to the title of each facet
  
  if(length(unique(df_spatial_toMap$facet))==1){ 
    
    if(nrow(missing_shp)>0){
      caption_missing_shp = paste(
        '\n',
        missing_shp$missing_shp_n_distinct,
        ' ',
        var_spatial_name,
        # 's (',
        #  paste0(missing_names, collapse = ' , ') , # add names of units without coordinates
        ' with missing coordinates (', 
        ifelse(missing_shp$missing_shp_pr<=0.005 & missing_shp$missing_shp_pr >0, '~0',round(missing_shp$missing_shp_pr, 2)),
        '% of ',
        ifelse(is.na(var_name_new), var_name, var_name_new),
        ') - not presented on the map.',
        sep = ''
      )
      message(caption_missing_shp)
    }else{
      caption_missing_shp = ''
    }
    
    
    caption = paste(#title, '. ',
                    #ifelse(!is.na(subtitle), paste(subtitle, '. ' , sep = ''),''),
                    ifelse(nrow(missing_var_spatial)>0, ifelse(missing_var_spatial$missing_var_spatial_pr<=0.005 &
                                                                 missing_var_spatial$missing_var_spatial_pr>0,'~0',
                                                               round(missing_var_spatial$missing_var_spatial_pr, 2)),0),
                    '% of ',
                    ifelse(is.na(var_name_new), var_name, var_name_new),
                    ' - reported for missing ',
                    var_spatial_name, '.',
                    caption_missing_shp,
                    sep = ''
    )
    
  }else{
    caption = paste(title, '. ', sep = '') # <---------------------------------------------------------------------------------------to do 
    # 
    # mdf2 %>% 
    #   left_join(missing_entries2) %>% 
    #   left_join(missing_value2) %>% 
    #   mutate(facet =  paste(facet, 
    #                         '\n',
    #                         paste(
    #                           str_wrap(
    #                             paste(ifelse(!is.na(prMissing), ifelse(prMissing<=0.005 & prMissing>0, '~0', round(prMissing, 2)),0),
    #                                   '% of ',
    #                                   ifelse(is.na(newVarName), var_name, newVarName),
    #                                   ' - missing ',
    #                                   groupBy_name, '.',
    #                                   sep = ''
    #                             ),
    #                             width = 55),
    #                           sep = '\n'),
    #                         '\n',
    #                         ifelse(!is.na(prMissingValue), 
    #                                paste(
    #                                  str_wrap( 
    #                                    paste(nMissingValue,
    #                                          ' ',
    #                                          groupBy_name,
    #                                          '(', 
    #                                          ifelse(prMissingValue<=0.005 & prMissingValue>0,'~0',round(prMissingValue, 2)),
    #                                          '% of ',
    #                                          ifelse(is.na(newVarName), var_name, newVarName),
    #                                          ') - missing coordinates',
    #                                          sep = ''
    #                                    ), 
    #                                    width = 55),
    #                                  sep = '\n'),
    #                                ''),
    #                         sep = ' ')
    #   ) -> mdf2
  }

######################################
# produce a map
###################################### 
  
  ggplot() +
    geom_sf(data = df_spatial_toMap, aes(fill = var) , na.rm = TRUE,
            size = 0.05, color =gray(.8) 
    ) +
    geom_sf(data = spatial_dataset,
            fill = NA ,
            na.rm = TRUE,
            size = 0.02, 
            color =gray(.8)
    ) +
    scale_fill_viridis_c(
      option = "viridis",
      #trans = "sqrt",
      na.value = "aliceblue",
      begin = 1,
      end = 0,
      name = ifelse(is.na(var_name_new), var_name, var_name_new)
    ) -> base_choropleth_map
  
  # if(addExtraShp==TRUE){ <-------------------------------------------------------------------------------------- to do
  #   plot+
  #     geom_sf(data = extraShp,
  #             fill = NA, 
  #             na.rm = TRUE,
  #             size = 0.5,
  #             color = gray(.3)
  #             #         )+
  #             # geom_sf_text(data = extraShp, aes(label = F_CODE),
  #             #              color = 'grey22',
  #             #              size = 3,
  #             #              fontface = "italic",
  #             #              check_overlap = TRUE
  #     )-> plot
  # }
  
  base_choropleth_map +
    geom_sf(data = ne_countries,  fill = "antiquewhite") +
    coord_sf(
      crs = "+init=epsg:4326",
      xlim = c(unlist(limits["xmin"]), unlist(limits["xmax"])),
      ylim = c(unlist(limits["ymin"]), unlist(limits["ymax"])),
      expand = FALSE
    ) +
    labs(
      title = title,
      x = 'Longitude',
      y = 'Latitude'
      # subtitle = subtitle <-------------------------------------------------------------------------------to do
      , caption = caption
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
      strip.text = element_text(size = ifelse(length(unique(df_spatial_toMap$facet))==1,10,6)),
      axis.text.x = element_text(size = ifelse(length(unique(df_spatial_toMap$facet))==1,9,7)),
      axis.text.y = element_text(size = ifelse(length(unique(df_spatial_toMap$facet))==1,9,7)),
      plot.caption = element_text(size=5)
    ) -> choropleth_map
  
  # if (plot_labels == TRUE) { <--------------------------------------------------------------------------------------- to do
  #   #display labels on the plot
  #   plot +
  #     geom_sf_text(data = mdf2, aes(label = groupBy), 
  #                  color = 'red1',
  #                  size = 3,
  #                  fontface = "italic",
  #                  check_overlap = TRUE
  #     ) -> plot
  # }
  # if(ICESRectangle==TRUE){
  #   plot+
  #     geom_text(aes(x = seq(8.5, 27.5, by = 1), y  = unlist(limits["ymin"])+1/4, 
  #                   label = c('F8','F9','G0','G1','G2','G3','G4','G5','G6','G7','G8','G9','H0','H1','H2','H3','H4','H5','H6','H7')))+
  #     geom_text(aes(x =  unlist(limits["xmax"])-1/4, y  = seq(53+3/4, 66+1/4, by = 1/2), 
  #                   label = c(36:61))
  #     )-> plot
  # }
  # if (saveResults == TRUE) {
  #   fileName =   paste(
  #     outputPath,
  #     "/choroplethMap_",
  #     func_name,
  #     '_',
  #     var_name,
  #     '_',
  #     groupBy_name,
  #     ifelse(is.na(facet_name),'', paste(', ', paste0(unique(mdf2$facet), collapse = ","), sep= '')),
  #     '_',
  #     type_of_threshold,
  #     '_',
  #     value_of_threshold,
  #     sep = ''
  #   )
  #   if (!is.na(Catch_group_name) & Catch_group_name!='NULL') {
  #     fileName = paste(fileName, Catch_group_name, sep = "")
  #   }
  #   write.table(
  #     mdf,
  #     file = paste(fileName, ".txt", sep = ""),
  #     sep = '\t',
  #     dec = '.'
  #   )
  #   ggsave(
  #     paste(fileName, ".tiff", sep = ""),
  #     units = "in",
  #     width = 10,
  #     height = 10,
  #     dpi = 300,
  #     compression = 'lzw'
  #   )
  #}
  # 
  # mdf %>%  select(-var, - groupBy, -facet)-> mdf
  # 
  # if(displayInR==TRUE){
  #   caption -> caption
  #   return(list(mdf, plot, caption)) #should we return all data - mdf (with missing parts) or only the data that were  plotted - mdf2?
  # }
  
  return(list(choropleth_map))
}



