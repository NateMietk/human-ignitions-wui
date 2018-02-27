library(tidyverse)

raw <- read_csv("rawdata.csv") # Table 3 in the supplements

td <- raw %>%
  tidyr::gather(class, value, wui:wild) %>%
  transform(class = factor(class, levels= c("wui", 'vld', "wild"))) %>%
  tidyr::spread(var, value) 

get_expected <- function(df) {
  
  # Takes a 3x2 table of observed values and calculates expected
  # Pull out the variable name we are using
  varname <- if_else(df$variable[1] == 'aer', 'aerial',
                     if_else(df$variable[1] == 'ba', 'burn_area',
                             if_else(df$variable[1] == 'cba', 'class_burn_area',
                                     if_else(df$variable[1] == 'cost', 'costs',
                                             if_else(df$variable[1] == 'des', 'home_destroyed',
                                                     if_else(df$variable[1] == 'fat', 'fatalities',
                                                             if_else(df$variable[1] == 'ff', 'fire_frequency',
                                                                     if_else(df$variable[1] == 'fsl', 'fire_season_length',
                                                                             if_else(df$variable[1] == 'thr', 'home_threat', 'unk')))))))))
  
  
  # Create new column names for the output dataframe
  colname1 <- paste0("human_expected")
  colname2 <- paste0("lightning_expected")
  colname3 <- paste0("human_observed")
  colname4 <- paste0("lightning_observed")
  
  
  # Reshape the dataframe to be entered into the for loop
  df_class <- df %>%
    tidyr::spread(cause, value) %>%
    dplyr::select(-variable)
  
  d <- df_class %>%
    tibble::remove_rownames() %>%
    tibble::column_to_rownames(var = 'class')
  
  # Empty dataframe
  expected_data <- tibble()
  
  # Calculate the expected values
  for (i in 1:length(levels(df_class$class))){
    expected_data[i,1] <- (sum(d[i,]) * sum(d[,1])) / sum(d)
    expected_data[i,2] <- (sum(d[i,]) * sum(d[,2])) / sum(d)
  }
  
  # Write out the expected values to the dataframe and clean 
  expected_data <- expected_data %>%
    dplyr::mutate(!!colname1 := V1,
                  !!colname2 := V2,
                  !!colname3 := d$Human,
                  !!colname4 := d$Lightning,
                  class = df_class$class,
                  variable = as.factor(varname)) %>%
    dplyr::select(-V1, -V2)
  
}

multi_model <- td %>%
  gather(variable, value, -class, -cause) %>%
  split(.$variable) %>%
  map(~ get_expected(.)) %>%
  bind_rows() %>%
  mutate_if(is.list, simplify_all) %>%
  gather(tmp, expected, human_expected:lightning_expected) %>%
  gather(cause, observed, human_observed:lightning_observed) %>%
  mutate(pct_diff = (expected/observed)*100) %>%
  separate(cause, c('cause', 'other'), extra = 'drop') %>%
  dplyr::select(class, cause, variable, observed, expected, pct_diff) %>%
  mutate(row_id = row_number())

chi_model <- multi_model %>%
  dplyr::select(row_id, observed, expected) %>%
  rowwise() %>% 
  mutate(chi_sq_p_val = chisq.test(., observed, expected, simulate.p.value = TRUE, B = 10)$p.value)


,
chi_sq_stat = chisq.test(x = observed, y = expected, simulate.p.value = TRUE, B = 100)$statistic))


