# ICS 209
if (!file.exists(file.path(rmarkdown_files, 'bu_ics_cleaned.rds'))) {
  
  bu_ics_cleaned <- read_rds(file.path(dir_cleaned_ics_ztrax_rds, 'all_cleaned_ics_built_up.rds')) %>%
    mutate(start_year = year) %>%
    group_by(incident_unique_id, built_class) %>%
    arrange(desc(incident_unique_id, built_class, year)) %>%
    mutate(build_up_count_no_zero = build_up_count - first(build_up_count),
           build_up_intensity_sqm_no_zero = build_up_intensity_sqm - first(build_up_intensity_sqm)) %>%
    ungroup() %>%
    left_join(wui_209, ., by = c('start_year', 'incident_unique_id')) %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
    mutate(built_class = ifelse(is.na(built_class), 'No Structures', as.character(built_class))) %>%
    dplyr::select(-year)
  
  write_rds(bu_ics_cleaned, file.path(rmarkdown_files, 'bu_ics_cleaned.rds'))
  system(paste0("aws s3 sync ", rmarkdown_files, " ", s3_rmarkdown))
  
} else {
  
  bu_ics_cleaned <- read_rds(file.path(rmarkdown_files, 'bu_ics_cleaned.rds'))
}

# ICS 209 250m buffer
if (!file.exists(file.path(rmarkdown_files, 'bu_ics_250m_cleaned.rds'))) {
  
  bu_ics_250m_cleaned <- read_rds(file.path(dir_cleaned_ics_250m_ztrax_rds, 'all_cleaned_ics_250m_built_up.rds')) %>%
    mutate(start_year = year) %>%
    group_by(incident_unique_id, built_class) %>%
    arrange(desc(incident_unique_id, built_class, year)) %>%
    mutate(build_up_count_no_zero = build_up_count - first(build_up_count),
           build_up_intensity_sqm_no_zero = build_up_intensity_sqm - first(build_up_intensity_sqm)) %>%
    ungroup() %>%
    left_join(wui_209, ., by = c('start_year', 'incident_unique_id')) %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
    mutate(built_class = ifelse(is.na(built_class), 'No Structures', as.character(built_class))) %>%
    dplyr::select(-year)
  
  write_rds(bu_ics_250m_cleaned, file.path(rmarkdown_files, 'bu_ics_250m_cleaned.rds'))
  system(paste0("aws s3 sync ", rmarkdown_files, " ", s3_rmarkdown))
  
} else {
  
  bu_ics_250m_cleaned <- read_rds(file.path(rmarkdown_files, 'bu_ics_250m_cleaned.rds'))
}

# WUI all years
if (!file.exists(file.path(rmarkdown_files, 'bu_wui_cleaned_validation.rds'))) {
  # This cleaning needs a 240 GB RAM machine and takes ~24 hours
  bu_wui_cleaned_validation <- ungroup(read_rds(file.path(dir_cleaned_wui_ztrax_rds, 'all_cleaned_wui_built_up.rds'))) %>%
    group_by(blk10, built_class) %>%
    arrange(desc(blk10, built_class, year)) %>%
    mutate(build_up_count_no_zero = build_up_count - first(build_up_count),
           build_up_intensity_sqm_no_zero = build_up_intensity_sqm - first(build_up_intensity_sqm)) %>%
    ungroup() %>%
    mutate(blk10 = as.factor(blk10),
           year = as.integer(year))
  
  wui_slim <- wui_df  %>%
    dplyr::select(blk10, class90, class00, class10, house_units_1990, house_units_2000, house_units_2010) 
  
  bu_wui_cleaned_validation <- bu_wui_cleaned_validation %>%
    left_join(., wui_slim, by = "blk10") %>%
    mutate(class = case_when(
      year == 0 ~ as.character(class90),
      year == 1990 & year <= 1999 ~ as.character(class90),
      year == 2000 & year <= 2009~ as.character(class00),
      year == 2010 & year <= 2015 ~ as.character(class10), TRUE ~ NA_character_),
      house_units = case_when(
        year == 0 ~ house_units_1990,
        year == 1990 & year <= 1999 ~ house_units_1990,
        year == 2000 & year <= 2009 ~ house_units_2000,
        year == 2010 & year <= 2015 ~ house_units_2010, TRUE ~ NA_integer_)) %>%
    dplyr::select(blk10, year, class, built_class, build_up_count, build_up_intensity_sqm, 
                  build_up_count_no_zero, build_up_intensity_sqm_no_zero, house_units) %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
    group_by(blk10, built_class) %>%
    fill(class)
  
  bu_wui_cleaned_slim <- ungroup(bu_wui_cleaned_validation)  %>%
    mutate(built_class = ifelse(is.na(built_class), 'No Structures', as.character(built_class))) %>%
    dplyr::select(blk10, year, class, house_units) %>%
    mutate(SILVIS = 'SILVIS') %>%
    gather(key = built_class, value = build_up_count, -blk10, -year, -class) %>%
    filter(built_class != 'SILVIS') %>%
    mutate(built_class = 'SILVIS',
           build_up_count = as.numeric(build_up_count),
           build_up_intensity_sqm = 0,
           build_up_count_no_zero = 0,
           build_up_intensity_sqm_no_zero = 0) 
  
  bu_wui_cleaned_validation <- ungroup(bu_wui_cleaned_validation) %>%
    dplyr::select(-house_units) %>%
    bind_rows(list(., bu_wui_cleaned_slim)) %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
    arrange(desc(blk10, class, year, built_class)) %>%
    mutate(blk10 = as.factor(blk10),
           class = as.factor(class),
           built_class = as.factor(built_class))
  
  write_rds(bu_wui_cleaned_validation, file.path(rmarkdown_files, 'bu_wui_cleaned_validation.rds'))
  system(paste0("aws s3 sync ", rmarkdown_files, " ", s3_rmarkdown))
  
} else {
  
  bu_wui_cleaned_validation <- read_rds(file.path(rmarkdown_files, 'bu_wui_cleaned_validation.rds'))
}

# FPA fire perimeters
if (!file.exists(file.path(rmarkdown_files, 'bu_fpa_cleaned.rds'))) {
  
  bu_fpa_cleaned <- read_rds(file.path(dir_cleaned_fpa_ztrax_rds, 'all_cleaned_fpa_built_up.rds')) %>%
    ungroup() %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
    mutate(discovery_year = year,
           build_up_count_0 = build_up_count,
           build_up_intensity_sqm_0 = build_up_intensity_sqm,
           built_class = ifelse(is.na(built_class), 'No Structures', as.character(built_class))) %>%
    dplyr::select(-year, -build_up_count, -build_up_intensity_sqm)
  
  write_rds(bu_fpa_cleaned, file.path(rmarkdown_files, 'bu_fpa_cleaned.rds'))
  system(paste0("aws s3 sync ", rmarkdown_files, " ", s3_rmarkdown))
  
} else {
  
  bu_fpa_cleaned <- read_rds(file.path(rmarkdown_files, 'bu_fpa_cleaned.rds'))
}

# FPA 250m buffer
if (!file.exists(file.path(rmarkdown_files, 'bu_fpa_250m_cleaned.rds'))) {
  
  bu_fpa_250m_cleaned <- read_rds(file.path(dir_cleaned_fpa_250m_ztrax_rds, 'all_cleaned_fpa_250m_built_up.rds')) %>%
    ungroup() %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
    mutate(discovery_year = year,
           build_up_count_250 = build_up_count,
           build_up_intensity_sqm_250 = build_up_intensity_sqm,
           built_class = ifelse(is.na(built_class), 'No Structures', as.character(built_class))) %>%
    dplyr::select(-year, -build_up_count, -build_up_intensity_sqm)
  
  write_rds(bu_fpa_250m_cleaned, file.path(rmarkdown_files, 'bu_fpa_250m_cleaned.rds'))
  system(paste0("aws s3 sync ", rmarkdown_files, " ", s3_rmarkdown))
  
} else {
  
  bu_fpa_250m_cleaned <- read_rds(file.path(rmarkdown_files, 'bu_fpa_250m_cleaned.rds'))
}

# FPA 500m buffer
if (!file.exists(file.path(rmarkdown_files, 'bu_fpa_500m_cleaned.rds'))) {
  
  bu_fpa_500m_cleaned <- read_rds(file.path(dir_cleaned_fpa_500m_ztrax_rds, 'all_cleaned_fpa_500m_built_up.rds')) %>%
    ungroup() %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
    mutate(discovery_year = year,
           build_up_count_500 = build_up_count,
           build_up_intensity_sqm_500 = build_up_intensity_sqm,
           built_class = ifelse(is.na(built_class), 'No Structures', as.character(built_class))) %>%
    dplyr::select(-year, -build_up_count, -build_up_intensity_sqm)
  
  write_rds(bu_fpa_500m_cleaned, file.path(rmarkdown_files, 'bu_fpa_500m_cleaned.rds'))
  system(paste0("aws s3 sync ", rmarkdown_files, " ", s3_rmarkdown))
  
} else {
  
  bu_fpa_500m_cleaned <- read_rds(file.path(rmarkdown_files, 'bu_fpa_500m_cleaned.rds'))
}

# FPA 1000m buffer
if (!file.exists(file.path(rmarkdown_files, 'bu_fpa_1000m_cleaned.rds'))) {
  
  bu_fpa_1000m_cleaned <- read_rds(file.path(dir_cleaned_fpa_1000m_ztrax_rds, 'all_cleaned_fpa_1000m_built_up.rds')) %>%
    ungroup() %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
    mutate(discovery_year = year,
           build_up_count_1000 = build_up_count,
           build_up_intensity_sqm_1000 = build_up_intensity_sqm,
           built_class = ifelse(is.na(built_class), 'No Structures', as.character(built_class))) %>%
    dplyr::select(-year, -build_up_count, -build_up_intensity_sqm)
  
  write_rds(bu_fpa_1000m_cleaned, file.path(rmarkdown_files, 'bu_fpa_1000m_cleaned.rds'))
  system(paste0("aws s3 sync ", rmarkdown_files, " ", s3_rmarkdown))
  
} else {
  
  bu_fpa_1000m_cleaned <- read_rds(file.path(rmarkdown_files, 'bu_fpa_1000m_cleaned.rds'))
}

# FPA complete joined and cleaned
if (!file.exists(file.path(rmarkdown_files, 'bu_complete_cleaned.rds'))) {
  
  fpa_wui_slim <- as.data.frame(fpa_wui) %>%
    dplyr::select(fpa_id, discovery_year) 
  
  bu_complete_cleaned <- bu_fpa_1000m_cleaned %>%
    left_join(., bu_fpa_500m_cleaned, by = c('fpa_id', 'built_class', 'discovery_year')) %>%
    left_join(., bu_fpa_250m_cleaned, by = c('fpa_id', 'built_class', 'discovery_year')) %>%
    left_join(., bu_fpa_cleaned, by = c('fpa_id', 'built_class', 'discovery_year')) %>%
    as_tibble()  %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(build_up_count_250 = build_up_count_250 - build_up_count_0,
           build_up_intensity_sqm_250 = build_up_intensity_sqm_250 - build_up_intensity_sqm_0,
           build_up_count_500 = build_up_count_500 - (build_up_count_0 + build_up_count_250),
           build_up_intensity_sqm_500 = build_up_intensity_sqm_500 - (build_up_intensity_sqm_0 + build_up_intensity_sqm_250),
           build_up_count_1000 = build_up_count_1000 - (build_up_count_0 + build_up_count_250 + build_up_count_500),
           build_up_intensity_sqm_1000 = build_up_intensity_sqm_1000 - (build_up_count_0 + build_up_count_250 + build_up_count_500))
  zero_df <- bu_complete_cleaned  %>%
    filter(discovery_year == 0)
  tmp <- bu_complete_cleaned  %>%
    left_join(fpa_wui_slim, ., by = c('fpa_id', 'discovery_year')) %>%
    filter(!is.na(built_class)) %>%
    bind_rows(zero_df)
  
  tmp2 <- tmp %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.numeric, funs(ifelse(. < 0, 0, .))) %>%
    group_by(fpa_id, built_class) %>%
    arrange(fpa_id, built_class, discovery_year) %>%
    mutate(build_up_count_no_zero_0 = build_up_count_0 - first(build_up_count_0),
           build_up_count_no_zero_250 = build_up_count_250 - first(build_up_count_250),
           build_up_count_no_zero_500 = build_up_count_500 - first(build_up_count_500),
           build_up_count_no_zero_1000 = build_up_count_1000 - first(build_up_count_1000),
           build_up_intensity_sqm_no_zero_0 = build_up_intensity_sqm_0 - first(build_up_intensity_sqm_0),
           build_up_intensity_sqm_no_zero_250 = build_up_intensity_sqm_250 - first(build_up_intensity_sqm_250),
           build_up_intensity_sqm_no_zero_500 = build_up_intensity_sqm_500 - first(build_up_intensity_sqm_500),
           build_up_intensity_sqm_no_zero_1000 = build_up_intensity_sqm_1000 - first(build_up_intensity_sqm_1000)) %>%
    group_by(fpa_id, built_class, discovery_year) %>%
    summarise_at(vars(build_up_count_0, build_up_count_250, build_up_count_500, build_up_count_1000, 
                      build_up_intensity_sqm_0, build_up_intensity_sqm_250, build_up_intensity_sqm_500, 
                      build_up_intensity_sqm_1000, build_up_count_no_zero_0, build_up_count_no_zero_250, 
                      build_up_count_no_zero_500, build_up_count_no_zero_1000, build_up_intensity_sqm_no_zero_0,
                      build_up_intensity_sqm_no_zero_250, build_up_intensity_sqm_no_zero_500, build_up_intensity_sqm_no_zero_1000), first) %>%
    ungroup() %>%
    left_join(fpa_wui, ., by = c('fpa_id', 'discovery_year')) %>%
    filter(is.na(built_class))
  
  
  write_rds(bu_complete_cleaned, file.path(rmarkdown_files, 'bu_complete_cleaned.rds'))
  system(paste0("aws s3 sync ", rmarkdown_files, " ", s3_rmarkdown))
  
} else {
  
  bu_complete_cleaned <- read_rds(file.path(rmarkdown_files, 'bu_complete_cleaned.rds'))
}
