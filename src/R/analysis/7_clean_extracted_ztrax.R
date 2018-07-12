
if (!file.exists(file.path(rmarkdown_files, 'bu_ics_cleaned.rds'))) {
  
  bu_ics_cleaned <- read_rds(file.path(dir_cleaned_ics_ztrax_rds, 'all_cleaned_ics_built_up.rds')) %>%
    mutate(start_year = year) %>%
    left_join(wui_209, ., by = c('start_year', 'incident_unique_id')) %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
    mutate(built_class = ifelse(is.na(built_class), 'No Structures', built_class)) %>%
    dplyr::select(-year)
  
  write_rds(bu_ics_cleaned, file.path(rmarkdown_files, 'bu_ics_cleaned.rds'))
  system(paste0("aws s3 sync ", rmarkdown_files, " ", s3_rmarkdown))
  
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
  system(paste0("aws s3 sync ", rmarkdown_files, " ", s3_rmarkdown))
  
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
  system(paste0("aws s3 sync ", rmarkdown_files, " ", s3_rmarkdown))
  
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
  system(paste0("aws s3 sync ", rmarkdown_files, " ", s3_rmarkdown))
  
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
  # filter(fpa_class != '250m buffer') %>%
  group_by(class, fpa_class, ignition) %>%
  summarise(build_up_count = sum(build_up_count)) %>%
  ggplot(aes(x = class, y = build_up_count, group = fpa_class, fill = fpa_class)) +
  geom_bar(stat = 'identity', position='stack') +
  theme_pub() +
  # theme(legend.position = 'none') +
  facet_wrap(~ignition)

as.data.frame(bu_fpa_cleaned) %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  filter(built_class != 'No Structures') %>%
  group_by(class, built_class, ignition) %>%
  summarise(build_up_count = sum(build_up_count)) %>%
  ggplot(aes(x = class, y = build_up_count, group = built_class, fill = built_class)) +
  geom_bar(stat = 'identity', position='stack') +
  theme_pub() +
  # theme(legend.position = 'none') +
  facet_wrap(~ignition)



fpa_cause_bu_yearly <- as.data.frame(fpa_list) %>%
  filter(fpa_class != '250m buffer') %>% 
  #filter(fpa_class == '250m buffer') %>% 
  group_by(ignition) %>%
  summarise(bu_total = sum(build_up_count)) %>%
  ungroup() %>%
  na.omit()

lt_fpa_cause_bu_yearly <- as.data.frame(fpa_list) %>%
  filter(fpa_class != '250m buffer') %>% 
  #filter(fpa_class == '250m buffer') %>% 
  group_by(discovery_year, ignition) %>%
  summarise(bu_total = sum(build_up_count)) %>%
  ungroup() %>%
  group_by(ignition) %>%
  summarise(lt_mean = mean(bu_total)) %>%
  ungroup() %>%
  na.omit()

fpa_cause_bu_df <- as.data.frame(fpa_list) %>%
  filter(fpa_class != '250m buffer') %>% 
  #filter(fpa_class == '250m buffer') %>% 
  group_by(ignition, discovery_year) %>%
  summarise(f_cnt = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(build_up_count)) %>%
  left_join(., fpa_cause_bu_yearly, by = 'ignition') %>%
  left_join(., lt_fpa_cause_bu_yearly, by = 'ignition') %>%
  mutate(pct_change = (bu - lag(bu))/lag(bu) * 100,
         home_per_fire = bu/f_cnt,
         home_per_areaburned = bu/burned_area,
         anomalies = bu - lt_mean,
         colour = ifelse(pct_change <= 0, "negative","positive"), 
         colourlt = ifelse(anomalies <= 0, "negative","positive")) %>%
  ungroup() %>%
  filter(!is.na(discovery_year)) %>%
  mutate(buff_zone = 0)

fpa_cause_bu_p <- fpa_cause_bu_df %>%
  ggplot(aes(x = discovery_year, y = bu*0.000001, group = ignition, color = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1, se = FALSE
  ) +
  theme_pub() +
  xlab("Year") + ylab("Building unit count (in 100,000 units)") +
  scale_color_manual(values = c("#D62728","#1F77B4")) +
  # ggtitle('(a) Number of structures \nwithin all wildfires') +
  #scale_y_continuous(limits = c(0, 6)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')

doy_ignition <- as.data.frame(fpa_list) %>%
  #filter(fpa_class != '250m buffer') %>% 
  filter(fpa_class == '250m buffer') %>% 
  group_by(discovery_doy, ignition) %>%
  summarise(doy_total = sum(build_up_count)) %>%
  ungroup()

fpa_cause_bu_doy <- ggplot() +
  geom_bar(data = filter(doy_ignition, ignition == 'Human'), aes(x =  discovery_doy, y = doy_total*0.0001, colour = ignition, fill = ignition), stat = "identity") +
  geom_bar(data = filter(doy_ignition, ignition == 'Lightning'), aes(x =  discovery_doy, y = doy_total*0.0001, fill = ignition, colour = ignition), stat = "identity") +
  theme_pub()  +
  scale_color_manual(values = c("#D62728","#1F77B4")) +
  scale_fill_manual(values = c("#D62728","#1F77B4")) +
  xlab("Discovery day of year") + ylab("Number of homes threatened (in 1,000 units)") +
  # ggtitle('(b) Number of structures \nwithin all wildfires') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')

fpa_cause_bu_anom <- fpa_cause_bu_df %>%
  ggplot(aes(x =  discovery_year)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), aes(y = anomalies*0.000001, fill = interaction(ignition, colourlt), group = interaction(ignition, colourlt))) +
  scale_fill_manual(values = c("#d1e5f0", '#2166ac',
                               '#fddbc7', '#b2182b')) +
  theme_pub() +
  xlab("Year") + ylab("Homes threatened by wildfire anomalies (in 100,000 units)") +
  # ggtitle('(b) Proportion of structures contained \nwithin wildfire to total') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0"))

