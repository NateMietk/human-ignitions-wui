library(tidyverse)

raw <- read_csv("rawdata.csv")

td <- raw %>%
  gather(class, value, wui:wild) %>%
  spread(var, value) %>%
  select(class, cause, ff, ba) #eventaully not need to select these but run on all variables

get_expected <- function(df) {
  # Takes a 3x2 table of observed values and calculates expected
  # Pull out the variable name we are using
  varname <- df$variable[1]
  
  # Create new column names for the output dataframe
  colname1 <- paste0(varname, "_human_expected")
  colname2 <- paste0(varname, "_lightning_expected")
  
  # Reshape the dataframe to be entered into the for loop
  d <- df  %>%
    spread(cause, value) %>%
    select(-variable) %>%
    remove_rownames() %>%
    column_to_rownames(var = 'class')
  
  # Empty dataframe
  expected_data <- tibble()
  
  # Calculate the expected values
  for (i in 1:3){
    expected_data[i,1] <- (sum(d[i,]) * sum(d[,1])) / sum(d)
    expected_data[i,2] <- (sum(d[i,]) * sum(d[,2])) / sum(d)
  }
 
  # Write out the expected values to the dataframe and clean 
  expected_data <- expected_data %>%
    mutate(!!colname1 := V1,
           !!colname2 := V2) %>%
    select(-V1, -V2) 
}

multi_model <- td %>%
  transform(class = factor(class, levels= c("wui", "vld", "wild"))) %>%
  gather(variable, value, -class, -cause) %>%
  split(.$variable) %>%
  do.call(get_expected(.))
