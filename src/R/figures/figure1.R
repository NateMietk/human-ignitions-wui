bu_complete_long_no_zero <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
  dplyr::select(fpa_id, class, ignition, discovery_year, built_class, build_up_count_no_zero_0) %>%
  gather(key = 'buffer_class', value = 'built_count', -fpa_id, -class, -ignition, -discovery_year, -built_class) %>%
  mutate(buffer_class = case_when(
    buffer_class == 'build_up_count_no_zero_0' ~ 'Fire perimeter',
    TRUE ~ NA_character_)) %>% filter(buffer_class == 'Fire perimeter') %>% 
  mutate(fpa_id = as.factor(fpa_id))

sen <- function(..., weights = NULL) {
  mblm::mblm(...)
}

# Time series of burned area per ignition
p1 <- bu_complete_long_no_zero %>%
  left_join(., fpa_wui, by = c('fpa_id', 'discovery_year', 'ignition', 'class')) %>%
  # filter(region != 'Central') %>%
  filter((class_coarse %in% c('WUI', 'VLD', 'Wildlands'))) %>%
  filter(built_class == 'Residential') %>%
  transform(class_coarse = factor(class_coarse, levels=c('WUI', 'Wildlands'))) %>%
  group_by(discovery_year, ignition) %>%
  summarise(burn_area = sum(fire_size_km2)) %>%
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
p2 <- bu_complete_long_no_zero %>%
  left_join(., fpa_wui, by = c('fpa_id', 'discovery_year', 'ignition', 'class')) %>%
  # filter(region != 'Central') %>%
  filter((class_coarse %in% c('WUI', 'VLD', 'Wildlands'))) %>%
  filter(built_class == 'Residential') %>%
  transform(class_coarse = factor(class_coarse, levels=c('WUI', 'Wildlands'))) %>%
  group_by(discovery_year, ignition) %>%
  summarise(built_count = sum(built_count)) %>%
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
  mutate(start_year = Year)

p3 <- wui_209_df %>%
  mutate(costs = ifelse(incident_unique_id %in% c('ID-SCF-P46024|2000|1', 'ID-PAF-008|2000|1', 'MT-BDF-129|2000|1'), costs/10, costs)) %>%
  # filter(region != 'Central') %>%
  filter(cause != 'Unk') %>%
  filter((class_coarse %in% c('WUI', 'VLD', 'Wildlands'))) %>%
  group_by(start_year, cause) %>%
  summarise(costs = sum(costs)) %>%
  ggplot(aes(x = start_year)) +
  geom_point(aes(y = costs/1000000000, group = cause, color = cause)) +
  geom_line(aes(y = costs/1000000000, group = cause, color = cause)) +
  # geom_bar(aes(fill = cause), stat= 'identity', position = 'dodge', alpha = 0.3) +
  geom_smooth(aes(x = start_year, y = costs/1000000000, group = cause, color = cause), method = sen) +
  geom_point(data = nifc, aes(y = NIFC_Costs/1000000000), color = 'darkgreen') +
  geom_line(data = nifc, aes(y = NIFC_Costs/1000000000), color = 'darkgreen') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +
  scale_color_manual(values = c("#D62728","#1F77B4")) +
  scale_fill_manual(values = c("#D62728","#1F77B4")) +
  xlab('Discovery year') + ylab('Costs ($; in billions)') +
  theme_pub() +
  theme(legend.position = "none") 

grid.arrange(p1, p2, p3, nrow = 1)
g <- arrangeGrob(p1, p2, p3, nrow = 1) #generates g

ggsave("figs/figure1.eps", g, width = 10, height = 5, dpi = 600, scale = 3, units = "cm") #saves g
ggsave("figs/figure1.tiff", g, width = 10, height = 5, dpi = 600, scale = 3, units = "cm") #saves g

