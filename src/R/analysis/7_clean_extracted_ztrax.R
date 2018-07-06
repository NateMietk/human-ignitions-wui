
bu_ics_cleaned <- read_rds(file.path(dir_cleaned_ics_ztrax_rds, 'all_cleaned_ics_built_up.rds')) %>%
  mutate(start_year = year) %>%
  left_join(wui_209, ., by = c('start_year', 'incident_unique_id')) %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
  mutate(built_class = ifelse(is.na(built_class), 'No Structures', built_class)) %>%
  dplyr::select(-year)

if (!file.exists(file.path(dir_cleaned_wui_ztrax_rds, 'bu_wui_cleaned.csv'))) {
  
  bu_wui_cleaned <- read_rds(file.path(dir_cleaned_wui_ztrax_rds, 'all_cleaned_wui_built_up.rds')) %>%
    left_join(., wui_df, by = "blk10") %>%
    filter((year %in% c('1990', '2000', '2010'))) %>%
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
    dplyr::select(blk10, year, class, built_class, build_up_count, build_up_intensity_sqm, number_of_persons, house_units, pop_den, house_den) %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
    mutate(built_class = ifelse(is.na(built_class), 'No Structures', built_class))
  
    write_csv(bu_wui_cleaned, file.path(dir_cleaned_wui_ztrax_rds, 'bu_wui_cleaned.csv'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
    
  } else {
  
  bu_wui_cleaned <- read_csv(file.path(dir_cleaned_wui_ztrax_rds, 'bu_wui_cleaned.csv'))
  }
  

bu_fpa_cleaned <- read_rds(file.path(dir_cleaned_fpa_ztrax_rds, 'all_cleaned_fpa_built_up.rds')) %>%
  mutate(discovery_year = year) %>%
  left_join(fpa_wui, ., by = c('discovery_year', 'fpa_id')) %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
  mutate(built_class = ifelse(is.na(built_class), 'No Structures', built_class)) %>%
  dplyr::select(-year)

as.data.frame(bu_fpa_cleaned) %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  filter(built_class != 'No Structures') %>%
  group_by(class, discovery_year, built_class) %>%
  summarise(build_up_count = sum(build_up_count)) %>%
  ggplot(aes(x = discovery_year, y = build_up_count, group = built_class, fill = built_class)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_smooth(method = 'glm', method.args = list(family = "poisson"), 
              se = TRUE)+
  theme_pub() +
  facet_wrap(~ class)
  