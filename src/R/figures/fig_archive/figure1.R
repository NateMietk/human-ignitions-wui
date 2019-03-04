bu_complete_long_no_zero <- as_tibble(as.data.frame(read_rds(file.path(rmarkdown_files, 'bu_complete_cleaned.rds')))) %>%
  dplyr::select(fpa_id, class, ignition, discovery_year, built_class, build_up_count_no_zero_0) %>%
  gather(key = 'buffer_class', value = 'built_count', -fpa_id, -class, -ignition, -discovery_year, -built_class) %>%
  mutate(buffer_class = case_when(
    buffer_class == 'build_up_count_no_zero_0' ~ 'Fire perimeter',
    TRUE ~ NA_character_)) %>% filter(buffer_class == 'Fire perimeter') %>% 
  mutate(fpa_id = as.factor(fpa_id)) %>%
  left_join(., fpa_wui_df, by = c('fpa_id', 'discovery_year', 'ignition', 'class')) %>%
  filter((class_coarse %in% c('WUI', 'VLD', 'Wildlands'))) %>%
  filter(built_class == 'Residential')

# Time series of burned area per ignition
p1_df <- bu_complete_long_no_zero %>%
  transform(class_coarse = factor(class_coarse, levels=c('WUI', 'Wildlands'))) %>%
  group_by(discovery_year, ignition) %>%
  summarise(burn_area = sum(fire_size_km2)) 

human_p1_df <- p1_df %>%
  filter(ignition == 'Human') %>%
  dplyr::select(-ignition)
h_costs_mblm <- mblm(burn_area ~ discovery_year, data = human_p1_df, repeated = FALSE)

lightning_p1_df <- p1_df %>%
  filter(ignition == 'Lightning') %>%
  dplyr::select(-ignition)
l_costs_mblm <- mblm(burn_area ~ discovery_year, data = lightning_p1_df, repeated = FALSE)

p1 <- p1_df %>%
  ggplot(aes(x = discovery_year, y = burn_area/1000, group = ignition, color = ignition)) +
  geom_point() +
  geom_line() +
  # geom_bar(aes(fill = ignition), stat= 'identity', position = 'dodge', alpha = 0.3) +
  geom_smooth(method = sen) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,14)) +
  scale_color_manual(values = c("#D62728","#1F77B4")) +
  scale_fill_manual(values = c("#D62728","#1F77B4")) +
  xlab('Discovery year') + ylab('Burned area (km2; in thousands)') +
  theme_pub() +
  theme(legend.position = "none") 

# Time series of threatened homes per ignition
p2_df <- bu_complete_long_no_zero %>%
  transform(class_coarse = factor(class_coarse, levels=c('WUI', 'Wildlands'))) %>%
  group_by(discovery_year, ignition) %>%
  summarise(built_count = sum(built_count, na.rm = TRUE)) 

human_p2_df <- p2_df %>%
  filter(ignition == 'Human') %>%
  dplyr::select(-ignition)
h_costs_mblm <- mblm(built_count ~ discovery_year, data = human_p2_df, repeated = FALSE)
summary(h_costs_mblm)

lightning_p2_df <- p2_df %>%
  filter(ignition == 'Lightning') %>%
  dplyr::select(-ignition)
l_costs_mblm <- mblm(built_count ~ discovery_year, data = lightning_p2_df, repeated = FALSE)
summary(l_costs_mblm)

p2 <- p2_df %>%
  ggplot(aes(x = discovery_year, y = built_count/10000, group = ignition, color = ignition)) +
  geom_point() +
  geom_line() +
  # geom_bar(aes(fill = ignition), stat= 'identity', position = 'dodge', alpha = 0.3) +
  geom_smooth(method = sen) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,14)) +
  scale_color_manual(values = c("#D62728","#1F77B4")) +
  scale_fill_manual(values = c("#D62728","#1F77B4")) +
  xlab('Discovery year') + ylab('Threatened homes (in tens of thousands)') +
  theme_pub() +
  theme(legend.position = "none") 

# Time series of costs per ignition
nifc <- read_csv(file.path(ics_outtbls, 'nifc_v_209s.csv')) %>%
  mutate(start_year = Year, 
         cause = 'NIFC',
         costs = NIFC_Costs) %>%
  dplyr::select(start_year, cause, costs)

p3_df <- wui_209_df %>%
  filter(incident_name != 'BTU Lightning Complex') %>%
  mutate(costs = ifelse(incident_unique_id %in% c('ID-SCF-P46024|2000|1', 'ID-PAF-008|2000|1', 'MT-BDF-129|2000|1'), costs/10, costs)) %>%
  # filter(region != 'Central') %>%
  filter(cause != 'Unk') %>%
  filter((class_coarse %in% c('WUI', 'VLD', 'Wildlands'))) %>%
  group_by(start_year, cause) %>%
  summarise(costs = sum(costs)) %>%
  bind_rows(., nifc)

p3 <- p3_df %>%
  ggplot(aes(x = start_year)) +
  geom_point(aes(y = costs/1000000000, group = cause, color = cause)) +
  geom_line(aes(y = costs/1000000000, group = cause, color = cause)) +
  # geom_bar(aes(fill = cause), stat= 'identity', position = 'dodge', alpha = 0.3) +
  geom_smooth(aes(x = start_year, y = costs/1000000000, group = cause, color = cause), method = sen) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2.5)) +
  scale_color_manual(values = c("#D62728","#1F77B4", 'darkgreen')) +
  scale_fill_manual(values = c("#D62728","#1F77B4", 'darkgreen')) +
  xlab('Discovery year') + ylab('Costs ($; in billions)') +
  theme_pub() +
  theme(legend.position = "none") 

grid.arrange(p1, p2, p3, nrow = 1)
g <- arrangeGrob(p1, p2, p3, nrow = 1) #generates g

ggsave(file.path(main_text_figs, "figure1.tiff"), g, width = 14, height = 5, dpi = 600, scale = 3, units = "cm") #saves g
system(paste0("aws s3 sync ", figs_dir, " ", s3_figs_dir))

