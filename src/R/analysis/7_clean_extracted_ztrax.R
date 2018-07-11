
if (!file.exists(file.path(rmarkdown_files, 'bu_ics_cleaned.rds'))) {
  
  bu_ics_cleaned <- read_rds(file.path(dir_cleaned_ics_ztrax_rds, 'all_cleaned_ics_built_up.rds')) %>%
    mutate(start_year = year) %>%
    left_join(wui_209, ., by = c('start_year', 'incident_unique_id')) %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
    mutate(built_class = ifelse(is.na(built_class), 'No Structures', built_class)) %>%
    dplyr::select(-year)
  
  write_rds(bu_ics_cleaned, file.path(rmarkdown_files, 'bu_ics_cleaned.rds'))
  system(paste0("aws s3 sync ", prefix, " ", s3_base))
  
} else {
  
  bu_ics_cleaned <- read_rds(file.path(rmarkdown_files, 'bu_ics_cleaned.rds'))
}

if (!file.exists(file.path(rmarkdown_files, 'bu_wui_cleaned.rds'))) {
  
  bu_wui_cleaned <- ungroup(read_rds(file.path(dir_cleaned_wui_ztrax_rds, 'all_cleaned_wui_built_up.rds'))) %>%
    full_join(., wui_df, by = "blk10") %>%
    filter((year %in% c('1990', '2000', '2010'))) %>%
    ungroup() %>%
    mutate(year = as.integer(year),
           class = case_when(year == 1990 ~ as.character(class90),
                             year == 2000 ~ as.character(class00),
                             year == 2010 ~ as.character(class10), TRUE ~ NA_character_),
           house_units = case_when(year == 1990 ~ house_units_1990,
                                   year == 2000 ~ house_units_2000,
                                   year == 2010 ~ house_units_2010, TRUE ~ NA_integer_)) %>%
    dplyr::select(blk10, year, class, built_class, build_up_count, build_up_intensity_sqm, house_units) %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
    mutate(built_class = ifelse(is.na(built_class), 'No Structures', built_class))
  
  bu_wui_cleaned <- bu_wui_cleaned %>%
    dplyr::select(blk10, year, class, house_units) %>%
    mutate(SILVIS = 'SILVIS') %>%
    gather(key = built_class, value = build_up_count, -blk10, -year, -class) %>%
    filter(built_class != 'SILVIS') %>%
    mutate(built_class = 'SILVIS',
           build_up_count = as.numeric(build_up_count)) %>%
    full_join(., bu_wui_cleaned, 
              by = c('blk10', 'year', 'class', 'built_class', 'build_up_count')) %>%
    dplyr::select(-house_units) %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
    distinct(blk10, year, class, built_class, .keep_all = TRUE) %>%
    arrange(desc(blk10, class, year, built_class))
  
  write_rds(bu_wui_cleaned, file.path(rmarkdown_files, 'bu_wui_cleaned.rds'))
  system(paste0("aws s3 sync ", prefix, " ", s3_base))
  
} else {
  
  bu_wui_cleaned <- read_rds(file.path(rmarkdown_files, 'bu_wui_cleaned.rds'))
}

if (!file.exists(file.path(rmarkdown_files, 'bu_fpa_cleaned.rds'))) {
  
  bu_fpa_cleaned <- read_rds(file.path(dir_cleaned_fpa_ztrax_rds, 'all_cleaned_fpa_built_up.rds')) %>%
    mutate(discovery_year = year) %>%
    left_join(fpa_wui, ., by = c('discovery_year', 'fpa_id')) %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
    mutate(built_class = ifelse(is.na(built_class), 'No Structures', built_class)) %>%
    dplyr::select(-year)
  
  write_rds(bu_fpa_cleaned, file.path(rmarkdown_files, 'bu_fpa_cleaned.rds'))
  system(paste0("aws s3 sync ", prefix, " ", s3_base))
  
} else {
  
  bu_fpa_cleaned <- read_rds(file.path(rmarkdown_files, 'bu_fpa_cleaned.rds'))
}

if (!file.exists(file.path(rmarkdown_files, 'bu_fpa_250m_cleaned.rds'))) {
  
  bu_fpa_250m_cleaned <- read_rds(file.path(dir_cleaned_fpa_250m_ztrax_rds, 'all_cleaned_fpa_250m_built_up.rds')) %>%
    mutate(discovery_year = year) %>%
    left_join(fpa_wui, ., by = c('discovery_year', 'fpa_id')) %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
    mutate(built_class = ifelse(is.na(built_class), 'No Structures', built_class)) %>%
    dplyr::select(-year)
  
  write_rds(bu_fpa_250m_cleaned, file.path(rmarkdown_files, 'bu_fpa_250m_cleaned.rds'))
  system(paste0("aws s3 sync ", prefix, " ", s3_base))
  
} else {
  
  bu_fpa_250m_cleaned <- read_rds(file.path(rmarkdown_files, 'bu_fpa_250m_cleaned.rds'))
}

as.data.frame(wui_df) %>%
  # filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  # transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  # filter(built_class != 'No Structures') %>%
  group_by(year) %>%
  summarise(build_up_count = sum(build_up_count)) %>%
  ggplot(aes(x = year, y = build_up_count, group = built_class, fill = built_class)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  geom_smooth(method = 'glm', method.args = list(family = "poisson"), 
              se = TRUE)+
  theme_pub() 

as.data.frame(bu_ics_cleaned) %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  filter(built_class != 'No Structures') %>%
  group_by(built_class, cause) %>%
  summarise(costs = sum(costs),
            build_up_count = sum(build_up_count))

p1 <- as.data.frame(bu_fpa_cleaned) %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  filter(built_class != 'No Structures') %>%
  group_by(class, discovery_year, built_class) %>%
  summarise(build_up_count = sum(build_up_count)) 

p1 <- as.data.frame(bu_fpa_cleaned) %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  filter(built_class != 'No Structures') %>%
  group_by(class, built_class) %>%
  summarise(build_up_count = sum(build_up_count)) %>%
  ggplot(aes(x = class, y = build_up_count, group = built_class, fill = built_class)) +
  geom_bar(stat = 'identity',              position='stack') +
  theme_pub() +
  theme(legend.position = 'none') +
  facet_wrap( ~ ignition)

wui_synth <- bu_wui_cleaned %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  filter(built_class == 'Residential') %>%
  group_by(class) %>%
  summarise(total_wui_built_up = sum(build_up_count)) 

as.data.frame(bu_fpa_cleaned) %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  filter(built_class == 'Residential') %>%
  group_by(class) %>%
  summarise(build_up_count = sum(build_up_count)) %>%
  ungroup() %>%
  left_join(., wui_synth, by = 'class') %>%
  mutate(pct = build_up_count/total_wui_built_up*100)

wui_synth <- bu_wui_cleaned %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  filter(built_class == 'Non-Residential') %>%
  group_by(class) %>%
  summarise(total_wui_built_up = sum(build_up_count)) 

as.data.frame(bu_fpa_cleaned) %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  filter(built_class == 'Non-Residential') %>%
  group_by(class) %>%
  summarise(build_up_count = sum(build_up_count)) %>%
  ungroup() %>%
  left_join(., wui_synth, by = 'class') %>%
  mutate(pct = build_up_count/total_wui_built_up*100)




