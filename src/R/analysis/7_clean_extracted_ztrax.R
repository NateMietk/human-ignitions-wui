
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

as.data.frame(bu_wui_cleaned) %>%
  # filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other', 'VLD', 'Wildlands'))) %>%
  transform(class = factor(class, levels=c("High Urban", "Med Urban", "Low Urban",
                                           'Interface WUI', 'Intermix WUI', 'VLD', 'Wildlands', 'Other'))) %>%
  filter(built_class != 'Non-Residential') %>%
  group_by(year, built_class, class) %>%
  summarise(build_up_count = sum(build_up_count, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = build_up_count, group = built_class, fill = built_class)) +
  geom_bar(stat = 'identity', position=position_dodge(.7)) +
  theme_pub() +
  facet_wrap(~class)

fpa_250_bu <- bu_fpa_250m_cleaned %>%
  mutate(fpa_class = '250m buffer')

fpa_bu <- bu_fpa_cleaned %>%
  mutate(fpa_class = '0m buffer')

fpa_list <- rbind(fpa_bu, fpa_250_bu)

as.data.frame(fpa_list) %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  filter(built_class != 'No Structures') %>%
  group_by(class, discovery_year, fpa_class) %>%
  summarise(build_up_count = sum(build_up_count)) %>%
  ggplot(aes(x = discovery_year, y = build_up_count, group = fpa_class, fill = fpa_class)) +
  geom_bar(stat = 'identity', position='stack') +
  # geom_smooth(method = 'glm', method.args = list(family = "poisson"), size = 1) +
  theme_pub() +
  theme(legend.position = 'none') +
  facet_wrap(~class)

as.data.frame(bu_fpa_cleaned) %>%
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
