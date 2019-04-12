group_func = function(df,
                      var,
                      groupBy,
                      groupBy2 = NA,
                      facet = NA,
                      func,
                      type_of_threshold = 'none',
                      value_of_threshold = NA,
                      Catch_group_name = NA) {
  # df - a data frame
  # var -  a column to be summmarised e.g. var = 'OfficialLandingCatchWeight'
  # groupBy - name of column, by which the grouping should be carried out. e.g. groupBy = 'Area'
  # groupBy2 - name of second column for grouping, makes sense only for scatterpie maps. e.g. group2 = 'FlagCountry'
  # facet - will be used for facet_wrap, e.g. facet = 'Year'
  # func - function summarising the data: sum,  n_distinct,  e.g. func = 'sum'
  # type_of_threshold - default set to 'none', other options: 'top_n', 'percent'
  # value_of_threshold - set it, if you defined any type_of_threshold
  # Catch_group_name - if NA then all species will be included, other options: demersal/flatfish/smallpelagic/largepelagic
  
  # Marta Suska
  # NMFRI
  # msuska@mir.gdynia.pl
  
  ################################################################################
  require(tidyverse)
  require(rlang)

  
  # rename the columns
  var_name = var
  var <- as.symbol(var_name)
  groupBy_name = groupBy
  groupBy <- as.symbol(groupBy_name)
  if(!is.na(facet)){
    facet_name <- facet
    facet = as.symbol(facet)  
  }else{
    facet = NA
    facet_name = NA
  }
  if(!is.na(groupBy2)){
    groupBy2_name <- groupBy2
    groupBy2 = as.symbol(groupBy2_name)  
  }else{
    groupBy2 = NA
    groupBy2_name = NA
  }
  
  
  func_name = func
  func = eval_tidy(as.symbol(func))
  # # CHECK the data
  # Check if the given groupBy variabl is present in the dataset
  
  if(!groupBy_name %in% colnames(df)) {
    stop(paste(
      'The given column --->',
      groupBy_name,
      '<--- is not present in the df dataset'
    ))
  }
  
  if(!is.na(facet) & !facet_name %in% colnames(df)) {
    stop(paste(
      'The given column --->',
      facet_name,
      '<--- is not present in the df dataset'
    ))
  }
  
  
  # Check if the given var variable is present in the dataset
  if (!var_name %in% colnames(df)) {
    stop(paste(
      'The given column --->',
      var_name,
      '<--- is not present in the df dataset'
    ))
  }
  
  # Check if the func name is valid
  if (!func_name %in% c('sum',  'n_distinct')) {
    stop(paste(
      'The given function name ---> ',
      func_name ,
      '<--- is not defined'
    ))
  }
  
  ################################################################################
  
  df %>% mutate(var = !!var,
                groupBy = !!groupBy,
                groupBy2 = !!groupBy2,
                facet = !!facet) -> df
  
  ################################################################################
  
  # If  func = sum-> var should be numeric. if it's not -> automatic conversion
  if (func_name %in% c('sum')) {
    sapply(df, is.numeric) %>% which %>% names %>%  intersect(var_name) %>% length -> isNumeric
    if (isNumeric != 1) {
      warning(
        paste(
          'The column var: ',
          var_name,
          ' should be numeric if you want to use func:',
          func_name,
          '.
          It was automatically converted into numeric'
        )
        )
      df %>% mutate(var = as.numeric(var)) -> df
    }
    df %>% mutate(var = as.numeric(var)) -> df # put it in both places to avoid error with integer overflow
    
  }
  
  ################################################################################
  
  # Inform about missing values
  df %>% filter(is.na(var)) -> warn
  if (nrow(warn) > 0) {
    message(paste('There are', nrow(warn), 'rows with missing', var_name, sep = " "))
  }
  # To do: check if everything is ok with NAs now
  
  # Check if the threshold is defined properly
  if(type_of_threshold %in% c('top_n', 'percent')){
    if(is.na(value_of_threshold)){
      stop('value_of_threshold is missing')
    } else if(!is.numeric(value_of_threshold)){
      stop('value_of_threshold must be a number')
    }
  }else if(type_of_threshold!='none'){
    stop(paste('No method  defined for threshold type = ', type_of_threshold))
  }
  ################################################################################
  # SUMMARISE
  
  # If catch group is defined
  if(!is.na(Catch_group_name) & Catch_group_name!='NULL'){
    if(Catch_group_name %in% c('demersal', 'small pelagic', 'flatfish', 'largepelagic')){
      df = df %>% filter(Catch_group==Catch_group_name)
    }else{
      stop('Not defined catch group')
    }
  }
  df  %>% group_by(groupBy, groupBy2, facet) %>%  summarise(var = func(var, na.rm = TRUE),
                                                  analysis_type = func_name) -> gdf
  
  
  #save info about missing groupBy entries
  gdf %>% ungroup() %>%
    group_by(facet) %>% 
    arrange(desc(var)) %>%
    mutate(pr := var / sum(var, na.rm = TRUE) * 100)-> gdf
  
  gdf  %>% filter(is.na(groupBy))->missing_entries
  
  # apply the threshold
  if (type_of_threshold == 'percent') {
   tdf = gdf %>% 
      mutate(cum_pr = cumsum(pr)) %>%
      filter(cum_pr <= value_of_threshold) %>%
      mutate(type_of_threshold = type_of_threshold,
             value_of_threshold = value_of_threshold)
    
  } else if (type_of_threshold == 'top_n') {
    tdf = gdf %>% ungroup() %>%group_by(facet) %>%  arrange(desc(var)) %>% top_n(value_of_threshold,var)%>%
      mutate(type_of_threshold = type_of_threshold,
             value_of_threshold = value_of_threshold)
  } else if (type_of_threshold == 'none') {
    tdf = gdf %>%  ungroup()%>% group_by(facet) %>% arrange(desc(var))
  } else{
    message(paste('No method  defined for threshold type = ', type_of_threshold))
    tdf = NULL
  }
  
  if(!is.na(Catch_group_name)){
    tdf = tdf %>% mutate(Catch_group = Catch_group_name)
  }
  
  if(is.na(facet)){
    tdf %>% select(-facet) %>% rename(!!var_name :=var, !!groupBy_name :=groupBy) %>% ungroup()-> tdf  
  }else{
    tdf %>% rename(!!var_name :=var, !!groupBy_name :=groupBy, !!facet_name := facet) %>% ungroup()-> tdf  
  }
  
  if(is.na(groupBy2)){
    tdf %>% select(-groupBy2)-> tdf
  }else{
    tdf %>% rename(!!groupBy2_name := groupBy2)-> tdf
  }
  
  return(list(tdf = tdf, percent_missing =  missing_entries))
  
  }

# # # example
# group_func(cl_rcg %>% filter(Year %in% c(2016, 2017)),
#            'OfficialLandingCatchWeight',
#            'Harbour',
#            groupBy2 = NA,
#            facet = 'Year',
#            func = 'sum',
#            type_of_threshold = 'top_n',
#            value_of_threshold = 20,
#            Catch_group_name = NA)


