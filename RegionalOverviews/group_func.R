# Working version

group_func = function(df,
                      var,
                      groupBy,
                      func,
                      threshold_type = 'none',
                      threshold = NA) {
  # df - a data frame
  # var -  a column to be summmarised e.g. var = OfficialLandingCatchWeight
  # groupBy - names of columns, by which the grouping should be carried out. IMPORTANT to write it as groupBy = quos(...) e.g. groupBy = quos(Harbours, HarboursDesc)
  # func - function summarising the data: sum,  n_distinct,  e.g. func = sum
  # threshold_type - default set to 'none', other options: 'top_n', 'percent'
  # threshold - set it, if you defined any threshold_type
  
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
  }
  
  # Inform about missing values
  df %>% filter(is.na(!!var)) -> warn
  if (nrow(warn) > 0) {
    message(paste('There are', nrow(warn), 'rows with missing', var_name, sep = " "))
  }
  # To do: check if everything is ok with NAs now
  
  # Check if the threshold is defined properly
  if(threshold_type %in% c('top_n', 'percent')){
    if(is.na(threshold)){
      stop('threshold is missing')
    } else if(!is.numeric(threshold)){
      stop('threshold must be a number')
    }
  }else if(threshold_type!='none'){
    stop(paste('No method  defined for threshold type = ', threshold_type))
  }
  ################################################################################
  # SUMMARISE
  
  df  %>% group_by(!!!groupBy) %>%  summarise(!!var := func(!!var, na.rm = TRUE),
                                              analysis_type = func_name) -> gdf
  
  # save info about missing groupBy entries
    gdf %>% ungroup() %>% mutate(pr = !!var/sum(!!var, na.rm = TRUE)) %>% filter(is.na(!!groupBy[[1]]))->missing_entries


  # apply the threshold
  if (threshold_type == 'percent') {
    tdf = gdf %>% ungroup() %>%
      arrange(desc(!!var)) %>%
      mutate(pr := !!var / sum(!!var) * 100) %>%
      mutate(cum_pr = cumsum(pr)) %>%
      filter(cum_pr <= threshold) %>% 
      mutate(threshold_type = threshold_type,
             threshold = threshold)
    
  } else if (threshold_type == 'top_n') {
    tdf = gdf %>% ungroup() %>% arrange(desc(!!var)) %>% top_n(threshold,!!var)%>% 
      mutate(threshold_type = threshold_type,
             threshold = threshold)
  } else if (threshold_type == 'none') {
    tdf = gdf %>%  ungroup()%>% arrange(desc(!!var))
  } else{
    message(paste('No method  defined for threshold type = ', threshold_type))
    tdf = NULL
  }
  
  
  return(list(tdf = tdf, percent_missing =  missing_entries))
  
  }



# Examples:
#group_func(CL_2014_NSEA, var = OfficialLandingValue,  groupBy=quos(FlagCountry), func = sum, threshold_type = 'none')
#group_func(CL_2014_NSEA, var = OfficialLandingCatchWeight,  groupBy=quos(FlagCountry, LandingCountry), func = sum, threshold_type = 'percent',threshold = 90)
#group_func(CL_2014_NSEA, var = FlagCountry,  groupBy=quos(LandingCountry), func = n_distinct, threshold_type = 'top_n', threshold = 10)
#group_func(CL_2014_NSEA, var = OfficialLandingCatchWeight,  groupBy=quos(FlagCountry, LandingCountry), func = sum, threshold_type = 'percent', threshold =90)
