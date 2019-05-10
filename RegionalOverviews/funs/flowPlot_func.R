flowPlot_func = function(df,
                         var1,
                         var2,
                         Var,
                         value_of_threshold = 0.001,
                         color_palette = NA,
                         saveResults = FALSE,
                         outputPath,
                         Catch_group_name = NA,
                         newVarName = NA,
                         addToTitle = NA) {
  # df - a data frame
  # var1 - left side of the flow chart, e.g. 'FlagCountry'
  # var2 - right side of the flow chart, e.g. 'LandingCountry'
  # Var -  a column to be summmarised e.g. var = 'OfficialLandingCatchWeight'
  # value_of_threshold - set it, if you don't want to plot minor flows
  # Catch_group_name - if NA then all species will be included, other options: demersal/flatfish/smallpelagic/largepelagic
  # saveResults - TRUE/FALSE - do you want to save the results?
  # outputPath - path for saving plots and tables
  # newVarName - set it if you want to rename the var e.g. OfficialLandingCatchWeight -> Landings
  # addToTitle - additional information to the title (e.g. for effort information about filtering vessels <10 or >10)
  # color_palette - set it, if you want to use any specific color palette. it should have a form of a named vector. 
  
  # Marta Suska (based on the Riverplot.r by Hans Gerritsen)
  # NMFRI
  # msuska@mir.gdynia.pl
  
  ################################################################################
  
  library(tidyverse)
  library(ggalluvial)
  # rename
  var1_name = var1
  var1 <- as.symbol(var1_name)
  var2_name = var2
  var2 <- as.symbol(var2_name)
  Var_name = Var
  Var <- as.symbol(Var_name)
  
  df %>% mutate(var1 = !!var1,
                var2 = !!var2,
                Var = !!Var) %>% 
    mutate(var1 = ifelse(is.na(var1), 'NA', as.character(var1)),
           var2 = ifelse(is.na(var2), 'NA', as.character(var2)))-> mdf

  # prepare the dataframe
  mdf %>%
    select(var1, var2, Var)  %>%
    group_by(var1, var2) %>%
    summarise(value = sum(Var, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(flow_number = 1:n()) %>%
    gather(x, stratum, -value, -flow_number) %>%
    mutate(threshold = value / sum(value))-> mdf

    mdf%>%
    filter(threshold > value_of_threshold) %>%
    select(-threshold) -> mdf2
  
  # set the graph parameters
  title = paste(ifelse(is.na(newVarName), Var_name, newVarName), ' by ', var1_name, ' and ', var2_name, sep = '')
  if(!is.na(addToTitle)){ title = paste(title, ' (',addToTitle, ')', sep ='')}
  
  if(!is.na(value_of_threshold)){
  subtitle = paste('Edges <',round(100*value_of_threshold, 2), '% removed' , sep  = '')
  }else{
    subtitle = ''
  }

  
  caption = ''

  if(is.na(color_palette)){ # if there is no color_palette given, use the random one
    unique_vars = mdf2  %>%  distinct(stratum) %>% nrow()
    color_palette = scales::hue_pal()(unique_vars)
  } 
  

  if(length(setdiff((mdf2 %>% distinct(stratum) %>% pull()), names(color_palette)))>0){# if there are missing countries in the color palette
    missing = setdiff((mdf2 %>% distinct(stratum) %>% pull()), names(color_palette))
    missing_colors = scales::hue_pal()(length(missing))
    names(missing_colors) = missing
    
    color_palette = c(color_palette, missing_colors)
  }
  
  

  # make the plot
  ggplot(
    data = as.data.frame(mdf2),
    aes(
      x = x,
      stratum = stratum,
      alluvium = flow_number,
      y = value,
      fill = stratum,
      label = stratum
    )
  ) +
    scale_fill_manual(values = color_palette)+
    scale_x_discrete(expand = c(.1, .1),
                     labels=c(var1_name, var2_name)) +
    geom_flow(color = "darkgray",
              alpha = .8,
              width = 1 / 10)+
    geom_stratum(alpha = 1, width = 1 / 10) +
    theme(legend.position = "none") +
    ggtitle("geom_flow") +
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption
    )+
    theme_classic()+
    theme(axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.line = element_blank(),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1))-> plot

  mdf %>%
    spread(x, stratum) %>%
    select(-flow_number) %>%
    select(var1, var2, value) %>%
    arrange(var1, var2) %>%
    rename(!!var1_name :=var1, !!var2_name :=var2, !!Var_name:= value) -> mdf

  return(list(mdf, plot))
  # to do:
  # info about missing values
  # cope with NAs
  # /sum - case when =0
  # catch group
  # save results
  # if not all countries have a color

}

# flowPlot_func(
#   cl_rcg,
#   'FlagCountry',
#   'LandingCountry',
#   'OfficialLandingCatchWeight_ton',
#   color_palette = aux_colours_ggplot,
#   newVarName = 'Landings (t)'
# )
