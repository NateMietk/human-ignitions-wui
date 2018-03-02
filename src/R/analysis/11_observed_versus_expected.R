
if (!file.exists(file.path(summary_dir, 'class_cause_summary.csv'))) {

  stats_fpa <- fpa %>%
    mutate(cause = ignition) %>%
    group_by(cause, class) %>%
    summarise(fire_frequency = n(),
              burn_area = sum(fire_size_km2),
              class_burn_area = sum(clarea_km2),
              fire_season_length = IQR(discovery_doy)) %>%
    as.data.frame() %>%
    dplyr::select(-geom) %>%
    as.tibble()

  stats_209 <- wui_209 %>%
    group_by(cause, class) %>%
    summarise(aerial = sum(tot.aerial),
              costs = sum(costs),
              strucutres_destroyed = sum(home.destroyed) + sum(comm.destroyed),
              fatalities = sum(fatalities),
              strucutres_threatened = sum(home.threat) + sum(comm.threat)) %>%
    as.data.frame() %>%
    dplyr::select(-geom) %>%
    as.tibble()

  stats_df <- stats_fpa %>%
    left_join(., stats_209, by = (c('class', 'cause'))) %>%
    transform(class = factor(class, levels= c("WUI", 'VLD', "Wildlands"))) %>%
    mutate(cause = as.factor(cause)) %>%
    mutate_if(is.integer, as.numeric) %>%
    as.tibble()

  write_csv(stats_df, file.path(summary_dir, 'class_cause_summary.csv'))

} else {

  stats_df <- read_csv(file.path(summary_dir, 'class_cause_summary.csv'))  %>%
    transform(class = factor(class, levels= c("WUI", 'VLD', "Wildlands"))) %>%
    mutate(cause = as.factor(cause)) %>%
    mutate_if(is.integer, as.numeric) %>%
    as.tibble()
}

get_expected <- function(df) {

  # Takes a 3x2 table of observed values and calculates expected
  # Pull out the variable name we are using
  varname <- df$variable[1]

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

multi_model <- stats_df %>%
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

chi_input <- stats_df %>%
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


# previous attempt that worked only when the simulation was not present
chi_model <- chi_input %>%
  filter(variable != 'costs') %>%
  mutate(observed = if_else(observed == 0, 1, observed)) %>%
  dplyr::select(row_id, observed, expected) %>%
  group_by(row_id) %>%
  do(data_frame(row_id = first(.$row_id),
                data = list(matrix(c(.$observed, .$expected), ncol = 2)))) %>%
  mutate(chi_test = map(data, chisq.test, correct = FALSE, simulate.p.value = TRUE, B = 100)) %>%
  mutate(p.value = map_dbl(chi_test, "p.value")) %>%
  mutate(statistic = map_dbl(chi_test, "statistic")) %>%
  ungroup() %>%
  dplyr::select(row_id, p.value, statistic) %>%
  left_join(chi_input, ., by = 'row_id') %>%
  dplyr::select(-row_id) %>%
  mutate(row_id = row_number())

chi_name <- file.path(summary_dir, paste0('chi_model_statistics.csv'))
write_csv(chi_model, chi_name)


system('aws s3 sync data s3://earthlab-natem/human-ignitions-wui')
