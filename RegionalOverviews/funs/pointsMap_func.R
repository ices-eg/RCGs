pointsMap_func = function(df,
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
                          Catch_group_name = NA,
                          addExtraShp = FALSE,
                          extraShp = NA,
                          newVarName = NA,
                          addToTitle = NA) {
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
  # addExtraShp - TRUE/FALSE, TRUE - if you want to use extra layer for your map, e.g. display FAOares on a map of StatisticalRectangles
  # extraShp - set it if you want to use extra layer for your map, e.g. display FAOares on a map of StatisticalRectangles
  # newVarName - set it if you want to rename the var e.g. OfficialLandingCatchWeight -> Landings
  # addToTitle - additional information to the title (e.g. for effort information about filtering vessels <10 or >10)
  
  # Marta Suska
  # NMFRI
  # msuska@mir.gdynia.pl

  ################################################################################
  
  require(rlang)
  require(ggplot2)
  require(sf)

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
  
  grouping_result = group_func(df, var_name, groupBy_name, groupBy2 = NA, facet_name, func_name, type_of_threshold = type_of_threshold, 
                               value_of_threshold =  value_of_threshold, Catch_group_name = Catch_group_name)
  
  tdf = grouping_result[[1]]
  if (is.null(tdf)) {
    stop('The chosen data set is empty')
  }
  missing_entries = grouping_result[[2]]

  ################################################################################

  # add coordinates info
  tdf %>%
    left_join(points_coord) -> mdf

  
  mdf %>% mutate(var = !!var,
                 groupBy = !!groupBy,
                 facet = !!facet) -> mdf
  
  # add info about records without coordinates
  if (sum(is.na(mdf$lat)) != 0 | sum(is.na(mdf$lon)) != 0) {
    mdf %>% filter((is.na(lat) | is.na(lon)) & !is.na(groupBy)) %>% summarise(pr = round(sum(pr), 2), n = n_distinct(groupBy)) %>% 
      as.data.frame() %>% select(pr, n)-> missing_value
    
    missing_caption = paste(
      '\n',
      missing_value$n,
      ' ',
      groupBy_name,
      # 's (',
      #  paste0(missing_names, collapse = ' , ') , # add names of units without coordinates
      ' with missing coordinates (', 
      missing_value$pr,
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
                     !is.na(mdf$lon),]$lon)+ c(-1, 1)
  ylim = range(mdf[!is.na(mdf$lat) & !is.na(mdf$lon),]$lat) + c(-0.5,+0.5)
  
  # x/y =  3/2
  if(abs(xlim[2]-xlim[1])>(3/2)*abs(ylim[2]-ylim[1])){
    diff = (2/3*abs(xlim[2]-xlim[1])-abs(ylim[2]-ylim[1]))/2
    ylim[1]=ylim[1]-diff
    ylim[2]=ylim[2]+diff
  }else if((2/3)*abs(xlim[2]-xlim[1])<abs(ylim[2]-ylim[1])){
    diff = (3/2*abs(ylim[2]-ylim[1])-abs(xlim[2]-xlim[1]))/2
    xlim[1]=xlim[1]-diff
    xlim[2]=xlim[2]+diff 
  }
  
  # load world map
  m <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Take only rows with coordinates
  mdf %>% filter(!is.na(lon) & !is.na(lat)) -> mdf2
  
  
  # Set the plot parameters
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
    title = paste(func_name, ' ', ifelse(is.na(newVarName), var_name, newVarName), ' by ', # groupBy_name,  ifelse(is.na(facet_name),'', paste(', ', paste0(unique(mdf2$facet), collapse = ","), sep= '')),
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
    ifelse(nrow(missing_entries)>0, round(missing_entries$pr, 2),0),
    '% of ',
    ifelse(is.na(newVarName), var_name, newVarName),
    ' - reported for missing ',
    groupBy_name,
    missing_caption,
    sep = ''
  )




  # make a map
  mdf2 %>%
    ungroup() %>% 
    arrange(var) %>%
    #mutate(name = groupBy) %>%
    ggplot() -> plot
  
  if(addExtraShp==TRUE){
    plot+
      geom_sf(data = extraShp,
              fill = NA, 
              na.rm = TRUE,
              size = 0.5,
              color = gray(.3)
                      )+
              geom_sf_text(data = extraShp, aes(label = F_CODE),
                           color = 'grey22',
                           size = 2,
                           fontface = "italic",
                           check_overlap = TRUE
      )-> plot
  }
  
    plot +
      geom_sf(data = m, fill = "antiquewhite") +
    coord_sf( crs = "+init=epsg:4326", xlim =xlim, ylim = ylim)+
    geom_point(
      aes(lon, lat, fill = var, size = var),
      stroke = FALSE,
      colour = 'black',
      #size = 4,
      shape = 21,
      alpha = 0.8
    ) +
    scale_size(range = c(0, 20), guide = FALSE) +
    viridis::scale_fill_viridis(
      option = "viridis",
      # trans = "log",
      begin = 1,
      end = 0,
      name = ifelse(is.na(newVarName), var_name, newVarName)
    )+
    #guides(colour = guide_legend())+
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
     panel.grid.major = element_line(color = gray(.8), linetype ='dashed', size = 0.5)
    ) -> plot

  if (plot_labels == TRUE) {
    #display labels on the plot
    plot +
      ggrepel::geom_label_repel(
        data = mdf2,
        aes(lon, lat, label = groupBy),
        box.padding = unit(0.2, "lines"),
        point.padding = unit(0.2, "lines"),
        color = 'black',
        segment.color = gray(0.3),
        size = 2,
        fontface = 'bold',
        arrow = arrow(length = unit(0.02, "npc"))
      ) -> plot
  }
  
      mdf %>%  select(-var, - groupBy, -facet)-> mdf


  if(saveResults==TRUE){
  fileName =   paste(outputPath, "/pointsMap_", func_name,'_', var_name, '_', groupBy_name,'_',  
                     ifelse(is.na(facet_name),'', paste(', ', paste0(unique(mdf2$facet), collapse = ","), sep= '')),
                     '_',type_of_threshold, '_',value_of_threshold, sep = '')
  if(!is.na(Catch_group_name) & Catch_group_name!='NULL'){
    fileName = paste(fileName, Catch_group_name, sep = "")
  }
  write.table(mdf, file = paste(fileName, ".txt", sep = ""), sep = '\t', dec = '.')
  ggsave(paste(fileName, ".tiff", sep = ""), units="in", width=10, height=10, dpi=300, compression = 'lzw')
  }

  return(list(mdf, plot))
}

# pointsMap_func(cl_rcg %>% filter(Year %in% c(2017)),
#                'OfficialLandingCatchWeight',
#                'Harbour',
#                facet = 'Year',
#                func = 'sum',
#                type_of_threshold = 'top_n',
#                value_of_threshold = 20,
#                Catch_group_name = NA,
#                points_coord = Harbours,
#                plot_labels = TRUE,
#                saveResults = FALSE,
#                outputPath = 'D:/WG/RCG/IntersessionalWork/Github/RCGs/RegionalOverviews',
#                  newVarName = 'Landings',
#                  addExtraShp = TRUE,
#                  extraShp = FAOshp
#                )
# 

# TO DO:
# points_coords - should it be a dataset, or a path to a dataset?
# add warning in case of enormous number of points to plot
# add type of plot (by harbour, by (what else could be?))
# assumption that the first parameter in groupBy will be plotted
# zoom options
# add check for joining df with points_coord - check if in both dataset there is a column with the same name
# where to get Harbours coordinates from?
# ... as the parameter to ggplot
# dopisac sciezke do shapefile
# if saveResults = TRUE -> outputPath musst be known - add check
# CHECK OF COORDINATES -  OUTSIDE THE POSSIBLE LIMITS