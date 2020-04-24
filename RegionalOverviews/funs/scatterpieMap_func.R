scatterpieMap_func = function(df,
                              var,
                              groupBy,
                              groupBy2,
                              facet,
                              func,
                              type_of_threshold = 'none',
                              value_of_threshold = NA,
                              points_coord,
                              plot_labels = FALSE,
                              saveResults = FALSE,
                              outputPath,
                              Catch_group_name = NA,
                              addExtraShp = FALSE,
                              extraShp = NA,
                              newVarName = NA,
                              addToTitle = NA,
                              color_palette = NA,
                              filter_ON = FALSE,
                              filter_column  = NA,
                              filter_type = NA,
                              filter_threshold = NA,
                              filter_func = NA) {
  # df - a data frame
  # var -  a column to be summmarised e.g. var = 'OfficialLandingCatchWeight'
  # groupBy - name of column, by which the grouping should be carried out. e.g. groupBy = 'Area'
  # groupBy2 - name of column by which a pie should be divided. e. g. 'FlagCountry'
  # facet - will be used for facet_wrap, e.g. facet = 'Year'
  # func - function summarising the data: sum,  n_distinct,  e.g. func = 'sum'
  # type_of_threshold - default set to 'none', other options: 'top_n', 'percent'
  # value_of_threshold - set it, if you defined any type_of_threshold
  # Catch_group_name - if NA then all species will be included, other options: demersal/flatfish/smallpelagic/largepelagic
  # points_coord - dataset with coordinates of a variable that was listed first in groupBy parameter, eg Harbour. Must have at least columns called lat, lon and column named the same as the appropriate column in the df
  # plot_labels  - TRUE/FALSE - should the labels of e.g. Harbours be displayed on a map?
  # saveResults - TRUE/FALSE - do you want to save the results?
  # outputPath - path for saving plots and tables
  # addExtraShp - TRUE/FALSE, TRUE - if you want to use extra layer for your map, e.g. display FAOares on a map of StatisticalRectangles
  # extraShp - set it if you want to use extra layer for your map, e.g. display FAOares on a map of StatisticalRectangles
  # newVarName - set it if you want to rename the var e.g. OfficialLandingCatchWeight -> Landings
  # addToTitle - additional information to the title (e.g. for effort information about filtering vessels <10 or >10)
  # color_palette - set it, if you want to use the same color for groupBy2. it should have a form of a named vector. 
  # filter_... - used when you want to limit your dataset to e.g. top 5 metiers
  
  # Marta Suska
  # NMFRI
  # msuska@mir.gdynia.pl
  
  ################################################################################
  require(rlang)
  require(ggplot2)
  require(sf)
  require(rnaturalearth)
  
  source('../../funs/group_func.R')
  
  # rename the columns
  var_name = var
  var <- as.symbol(var_name)
  groupBy_name = groupBy
  groupBy <- as.symbol(groupBy_name)
  groupBy2_name = groupBy2
  groupBy2 <- as.symbol(groupBy2_name)
  
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
               facet = facet_name,
               func = filter_func,
               type_of_threshold = filter_type,
               value_of_threshold = filter_threshold)-> filteredVariable
    df %>% filter(!!filter_column %in% (filteredVariable[[1]] %>% pull(!!filter_column))) %>% droplevels() -> df 
    
  }
  
  # creating the groupped df
  grouping_result = group_func(df, var_name, groupBy_name,groupBy2 = groupBy2_name, facet_name, func_name, type_of_threshold = type_of_threshold, 
                               value_of_threshold =  value_of_threshold, Catch_group_name = Catch_group_name, groupBy2spread = TRUE)  
  tdf = grouping_result[[1]]
  if (is.null(tdf)) {
    stop('The chosen data set is empty')
  }
  missing_entries = grouping_result[[2]]
  
  tdf %>% left_join(points_coord)  -> mdf
  
  # adding info about second variable
  #plotby_result = group_func(df, var_name, groupBy_name,groupBy2 = groupBy2_name, facet_name, func_name, type_of_threshold = 'none', 
  #                            Catch_group_name = Catch_group_name)  
  
  #tdf2 = plotby_result[[1]] %>% select(-pr)
  #if (is.null(tdf2)) {
  #  stop('The chosen data set is empty')
  #}
  #missing_entries2 = plotby_result[[2]]

  #mdf%>% select(-!!var) -> mdf
  #mdf %>% left_join(tdf2) ->mdf
  mdf%>% mutate(var = !!var,
                groupBy = !!groupBy,
                groupBy2 = !!groupBy2,
                facet = !!facet)  -> mdf
  
  # add info about records without coordinates
  mdf %>% filter((is.na(lat) | is.na(lon)) & !is.na(groupBy)) %>% distinct(groupBy, pr) %>% summarise(pr = sum(pr), n = n_distinct(groupBy)) %>% 
    as.data.frame() %>% select(pr, n)-> missing_value
  
  if (nrow(missing_value)>0 & (missing_value$pr!=0 & missing_value$n !=0 )) {
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
  } else{
    missing_caption = ''
  }
  
  
  
  # set the limits
  xlim = range(mdf[!is.na(mdf$lat) &
                     !is.na(mdf$lon),]$lon)+ c(-5, 5)
  ylim = range(mdf[!is.na(mdf$lat) & !is.na(mdf$lon),]$lat) + c(-4,+4)
  
  # x/y =  3/2
  if(unique(df$Region)!='NSEA'){
  if(abs(xlim[2]-xlim[1])>(3/2)*abs(ylim[2]-ylim[1])){
    diff = (2/3*abs(xlim[2]-xlim[1])-abs(ylim[2]-ylim[1]))/2
    ylim[1]=ylim[1]-diff
    ylim[2]=ylim[2]+diff
  }else if((2/3)*abs(xlim[2]-xlim[1])<abs(ylim[2]-ylim[1])){
    diff = (3/2*abs(ylim[2]-ylim[1])-abs(xlim[2]-xlim[1]))/2
    xlim[1]=xlim[1]-diff
    xlim[2]=xlim[2]+diff 
  }
  }else{
    if(abs(xlim[2]-xlim[1])>(5/2)*abs(ylim[2]-ylim[1])){
      diff = (2/5*abs(xlim[2]-xlim[1])-abs(ylim[2]-ylim[1]))/2
      ylim[1]=ylim[1]-diff
      ylim[2]=ylim[2]+diff
    }else if((2/5)*abs(xlim[2]-xlim[1])<abs(ylim[2]-ylim[1])){
      diff = (5/2*abs(ylim[2]-ylim[1])-abs(xlim[2]-xlim[1]))/2
      xlim[1]=xlim[1]-diff
      xlim[2]=xlim[2]+diff 
    } 
  }
  
  # Take only rows with coordinates
  mdf %>%  filter(!is.na(lat) & !is.na(lon)) %>% filter(lon>=-180 & lon <= 180 & lat >= -90 & lat <= 90)-> mdf2

  # load world map
  m <- ne_countries(scale = "medium", returnclass = "sf")
  
  
  # Set the plot parameters
  if (groupBy_name=="StatisticalRectangle"){groupBy_name="Statistical Rectangle"}
  if (groupBy_name=="AreaMap"){groupBy_name="Area Map"} 
  # title
  if (func_name %in% c('sum')) {
    title = paste(func_name,
                  ' of ',
                  ifelse(is.na(newVarName), var_name, newVarName),
                  ' by ',
                  groupBy_name,
                  # ifelse(is.na(facet_name),'', paste(', ', paste0(unique(mdf2$facet), collapse = ","), sep= '')),
                  sep = '')
  } else{
    title = paste(func_name, ' ',   ifelse(is.na(newVarName), var_name, newVarName), ' by ', groupBy_name,#  ifelse(is.na(facet_name),'', paste(', ', paste0(unique(mdf2$facet), collapse = ","), sep= '')),
                  sep = '')
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
  caption = paste(
    ifelse(nrow(missing_entries)>0, ifelse(missing_entries$pr<=0.005 & missing_entries$pr>0,'~0',round(missing_entries$pr, 2)),0),
    '% of ',
    ifelse(is.na(newVarName), var_name, newVarName),
    ' - reported for missing ',
    groupBy_name, '.',
    missing_caption,
    sep = ''
  )
  
  
  # make a map
  
  # How to avoid pies distortions?
  #https://stackoverflow.com/questions/51398344/r-pie-charts-distorted-when-adding-to-projected-map-using-ggplot
  

  unique_bys = mdf2  %>%  distinct(groupBy2) %>% nrow()
  if(is.na(color_palette)){ # if there is no color_palette given, use the random one
    color_palette = scales::hue_pal()(unique_bys)
  }

  radius =0.3
  radiusMultiply = ifelse(groupBy_name %in% c('Area', 'AreaMap','FishingGround'), 4, ifelse(groupBy_name %in% c('Harbour', 'LandingCountry', 'FlagCountry'), 3, 1 ))
  if(unique(df$Region)=='BS'){radiusMultiply =radiusMultiply*2/3}
  pie.list <- mdf2%>%
    select(lon, lat, groupBy, groupBy2, facet, var) %>%  
    spread(groupBy2, var, fill =0) %>%
  mutate(Total = rowSums(.[(ncol(.)-unique_bys+1):ncol(.)]))  %>% 
  gather(groupBy2, var, -lon, -lat, -groupBy,-facet,  -Total) %>%
    tidyr::nest(groupBy2, var) %>%  
    # make a pie chart from each row, & convert to grob
    mutate(pie.grob = purrr::map(data,
                                 function(d) ggplotGrob(ggplot(d,
                                                               aes(x = 1, y = var, fill = groupBy2))+
                                                          scale_fill_manual(values = color_palette)+
                                                        geom_col(color = "black", size = 0.3,
                                                                   show.legend = FALSE) +
                                                          coord_polar(theta = "y") +
                                                          theme_void() ))) %>% 
    # convert each grob to an annotation_custom layer. I've also adjusted the radius
    # value to a reasonable size (based on my screen resolutions).
    mutate(radius = Total/max(Total, na.rm = TRUE)*radiusMultiply) %>% 
    rowwise() %>%
    #mutate(radius = radius*4) %>%
    mutate(subgrob = list(annotation_custom(grob = pie.grob,
                                            xmin = lon - radius, xmax = lon + radius,
                                            ymin = lat - radius, ymax = lat + radius))) 

if(groupBy_name %in% c('Area','AreaMap', 'FishingGround')){
  ggplot()+
     geom_sf(data = points_coord, fill = NA , na.rm = TRUE, size = 0.5, color = gray(.3))->p
}else{
  ggplot()->p
}
  
  if(addExtraShp==TRUE){
    p+
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
      )-> p
  }
  
    p+
      geom_sf(data = m,  fill = "antiquewhite")->p
    
    if(groupBy_name == 'LandingCountry' & groupBy2_name == 'FlagCountry'){
      p+
      geom_sf(data = st_as_sf(mdf), aes(fill = groupBy) , na.rm = TRUE)->p # for foreign part
    }
    
    p+
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
      subtitle = subtitle
      #,caption = caption
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
    geom_tile(data =  mdf2%>%
                select(lon, lat, groupBy, groupBy2, facet, var) %>%  
                spread(groupBy2, var, fill =0) %>%
                gather(groupBy2, var, -lon, -lat, -groupBy,-facet),
                aes(x = lon,  y = lat, fill = groupBy2),
              color = "black", width = 0.01, height = 0.01,
              inherit.aes = FALSE)+
      scale_fill_manual(values = color_palette)+
    pie.list$subgrob+
      facet_wrap(~facet)+
      guides(fill=guide_legend(title=groupBy2_name))-> plot
    
      # facet_wrap(~facet) # <----------------------------------------------------- GDZIE TO WSTAWIC, ZEBY DOBRZE DZIALALY FACETS
  
    
    if (plot_labels == TRUE) {
      #display labels on the plot
      plot +
        geom_sf_text(data = mdf2, aes(label = groupBy, geometry = geometry), 
                     color = 'grey22',
                     size = 3,
                     fontface = "italic",
                     check_overlap = TRUE
        ) -> plot
    }

  if (saveResults == TRUE) {
    fileName =   paste(
      outputPath,
      "/scatterpieMap_",
      func_name,
      '_',
      var_name,
      '_',
      groupBy_name,
      groupBy2_name,
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
  
 mdf %>%  select( -var, - groupBy, -groupBy2, -facet)-> mdf
 caption -> caption
  return(list(mdf, plot, caption))
}

# scatterpieMap_func(
#   cl_rcg %>% filter(Year == 2017),
#   var = 'OfficialLandingCatchWeight',
#   groupBy = 'Area',
#   groupBy2 = 'FlagCountry',
#   facet = 'Year',
#   func = 'sum',
#   type_of_threshold = 'percent',
#   value_of_threshold = 100,
#   points_coord = FAOshp,
#   plot_labels = TRUE,
#   saveResults = FALSE,
#   outputPath = 'D:/WG/RCG/IntersessionalWork/Github/RCGs/RegionalOverviews',
#   newVarName = 'Landings',
#   addExtraShp = FALSE,
#   extraShp = FAOshp,
#   color_palette = NA
# )


 
# scatterpieMap_func(
#   cl_rcg %>% filter(Year == 2017),
#   var = 'OfficialLandingCatchWeight',
#   groupBy = 'Area',
#   groupBy2 = 'FlagCountry',
#   facet = 'Year',
#   func = 'sum',
#   type_of_threshold = 'percent',
#   value_of_threshold = 90,
#   points_coord = FAOshp,
#   plot_labels = FALSE,
#   saveResults = FALSE,
#   outputPath = 'D:/WG/RCG/IntersessionalWork/Github/RCGs/RegionalOverviews'
# )
