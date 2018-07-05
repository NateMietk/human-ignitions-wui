bu_cleaned <- sum_fpa_bu %>%
  gather(variable, built_up, -ID_sp, -fpa_id) %>%
  separate(variable,
           into = c("statistic", 'tmp', "year"),
           sep = "_") %>%
  dplyr::select(-tmp) %>%
  left_join(., as.data.frame(fpa_wui),
            by = "fpa_id") %>%
  filter(fire_size_km2 > 0.00025)


if (!file.exists(file.path(bu_out, 'bu_wui_blks.csv'))) {
  bu_wui_cleaned <- sum_wui_bu %>%
    setNames(tolower(names(.))) %>%
    gather(variable, built_up, -id_sp, -blk10) %>%
    separate(variable,
             into = c("statistic", 'tmp', "year"),
             sep = "_") %>%
    dplyr::select(-tmp, -id_sp, -statistic) %>%
    left_join(., as.data.frame(wui %>% 
                                 setNames(tolower(names(.)))),
              by = "blk10") %>%
    dplyr::select(-matches('(geom|flag|wuiclass|veg|water|shape)')) %>%
    filter(!(year %in% c('1995', '2005', '2015'))) %>%
    left_join(., cleaned_wui_decades, by = c("blk10", 'year')) %>%
    mutate(year = as.integer(year),
           class = case_when(year == 1990 ~ as.character(class90),
                             year == 2000 ~ as.character(class00),
                             year == 2010 ~ as.character(class10), TRUE ~ NA_character_),
           number_of_persons = case_when(year == 1990 ~ pop1990,
                                         year == 2000 ~ pop2000,
                                         year == 2010 ~ pop2010, TRUE ~ NA_integer_),
           pop_den = case_when(year == 1990 ~ popden1990,
                               year == 2000 ~ popden2000,
                               year == 2010 ~ popden2010, TRUE ~ NA_real_),
           house_den = case_when(year == 1990 ~ huden1990,
                                 year == 2000 ~ huden2000,
                                 year == 2010 ~ huden2010, TRUE ~ NA_real_),
           house_units = case_when(year == 1990 ~ hhu1990,
                                   year == 2000 ~ hhu2000,
                                   year == 2010 ~ hhu2010, TRUE ~ NA_integer_)) %>%
    dplyr::select(blk10, year, class, built_up, residential_built_up, house_units, pop_den, house_den)
  
  write_csv(bu_wui_cleaned, file.path(bu_out, 'bu_wui_blks.csv'))
  system(paste0("aws s3 sync ", prefix, " ", s3_base))
  
} else {
  
  bu_wui_cleaned <- read_csv(file.path(bu_out, 'bu_wui_blks.csv'))
  
}
