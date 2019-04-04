group_func = function(df,
                      var,
                      groupBy,
                      func,
                      type_of_threshold = 'none',
                      value_of_threshold = NA,
                      Catch_group = NA) {
  # df - a data frame
  # var -  a column to be summmarised e.g. var = OfficialLandingCatchWeight
  # groupBy - names of columns, by which the grouping should be carried out. IMPORTANT to write it as groupBy = quos(...) e.g. groupBy = quos(Harbours, HarboursDesc)
  # func - function summarising the data: sum,  n_distinct,  e.g. func = sum
  # type_of_threshold - default set to 'none', other options: 'top_n', 'percent'
  # value_of_threshold - set it, if you defined any type_of_threshold
  # Catch_group - if NA then all species will be included, other options: demersal/flatfish/smallpelagic/largepelagic
  
  # Marta Suska
  # NMFRI
  # msuska@mir.gdynia.pl
  
  ################################################################################
  require(tidyverse)
  require(rlang)
  
  var <- enquo(var)
  
  func_name = quo_name(enquo(func))
  var_name <- quo_name(var)
  #col_name <- paste0(func_name, '_', var_name)
  
  ################################################################################
  # CHECK the data
  
  # Check if the given groupBy variables are present in the dataset
  lapply(groupBy, function(x) {
    if (!quo_name(x) %in% colnames(df)) {
      stop(paste(
        'The given column --->',
        quo_name,
        '<--- is not present in the df dataset'
      ))
    }
  })
  
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
      df %>% mutate(!!var_name := as.numeric(!!var)) -> df
    }
    df %>% mutate(!!var_name := as.numeric(!!var)) -> df # put it in both places to avoid error with integer overflow
    
  }
  
  # Inform about missing values
  df %>% filter(is.na(!!var)) -> warn
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
  # if(!is.na(Catch_group)){
  #   if(Catch_group %in% c('demersal', 'smallpelagic', 'flatfish', 'largepelagic')){
  #     df = df %>% filter(Catch_group==Catch_group)
  #   }else{
  #     stop('Not defined catch group')
  #   }
  # }
  
  # TEMPORARY SOLUTION AS I DON"T HAVE SPECIES GROUPS FUNCTION FROM FISH PI
  if(!is.na(Catch_group)){
      df = df %>% filter(Species==Catch_group)
  }
  
  
  df  %>% group_by(!!!groupBy) %>%  summarise(!!var := func(!!var, na.rm = TRUE),
                                              analysis_type = func_name) -> gdf
  
  # save info about missing groupBy entries
    gdf %>% ungroup() %>% mutate(pr = !!var/sum(!!var, na.rm = TRUE)*100) %>% filter(is.na(!!groupBy[[1]]))->missing_entries


  # apply the threshold
  if (type_of_threshold == 'percent') {
    tdf = gdf %>% ungroup() %>%
      arrange(desc(!!var)) %>%
      mutate(pr := !!var / sum(!!var) * 100) %>%
      mutate(cum_pr = cumsum(pr)) %>%
      filter(cum_pr <= value_of_threshold) %>% 
      mutate(type_of_threshold = type_of_threshold,
             value_of_threshold = value_of_threshold)
    
  } else if (type_of_threshold == 'top_n') {
    tdf = gdf %>% ungroup() %>% arrange(desc(!!var)) %>% top_n(value_of_threshold,!!var)%>% 
      mutate(type_of_threshold = type_of_threshold,
             value_of_threshold = value_of_threshold)
  } else if (type_of_threshold == 'none') {
    tdf = gdf %>%  ungroup()%>% arrange(desc(!!var))
  } else{
    message(paste('No method  defined for threshold type = ', type_of_threshold))
    tdf = NULL
  }
  
  if(!is.na(Catch_group)){
    tdf = tdf %>% mutate(Catch_group = Catch_group)
  }
    
  return(list(tdf = tdf, percent_missing =  missing_entries))
  
  }



# Examples:
#group_func(CL_2014_NSEA, var = OfficialLandingValue,  groupBy=quos(FlagCountry), func = sum, type_of_threshold = 'none')
#group_func(CL_2014_NSEA, var = OfficialLandingCatchWeight,  groupBy=quos(FlagCountry, LandingCountry), func = sum, type_of_threshold = 'percent',value_of_threshold = 90)
#group_func(CL_2014_NSEA, var = FlagCountry,  groupBy=quos(LandingCountry), func = n_distinct, type_of_threshold = 'top_n', value_of_threshold = 10)
#group_func(CL_2014_NSEA, var = OfficialLandingCatchWeight,  groupBy=quos(FlagCountry, LandingCountry), func = sum, type_of_threshold = 'percent', value_of_threshold =90)
