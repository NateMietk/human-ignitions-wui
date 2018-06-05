

bu_cleaned <- sum_fpa_bu %>%
  gather(variable, bu, -ID_sp, -fpa_id) %>%
  separate(variable,
           into = c("statistic", 'tmp', "year"),
           sep = "_") %>%
  dplyr::select(-tmp) %>%
  left_join(., as.data.frame(fpa_wui),
            by = "fpa_id") %>%
  filter(fire_size_km2 > 0.00025)

# Prep CONUS and REGIONS ---------------------------------------------

region_bu_df <- sum_ecoregions_bu %>%
  gather(variable, bu, -ID_sp, -us_l3name) %>%
  separate(variable,
           into = c("statistic", 'tmp', "year"),
           sep = "_") %>%
  dplyr::select(-tmp) %>%
  left_join(., as.data.frame(ecoreg), by = "us_l3name") %>%
  mutate(regions = ifelse(regions == 'East', 'North East', as.character(regions))) %>%
  group_by(year, regions) %>%
  summarise(bu_total_regions = sum(bu)) %>%
  ungroup() %>%
  mutate(year = as.numeric(year)) %>%
  dplyr::select(year, regions, bu_total_regions)

region_yearly <- tibble(year = rep(1992:2015, times = 4),
                        regions = rep(c('Central', 'West', 'North East', 'South East'), each = 2016-1992)) %>%
  left_join(., region_bu_df, by = c('regions', 'year')) %>%
  fill(everything(), .direction = 'up') %>%
  mutate(discovery_year = year) %>%
  dplyr::select(-year)

region_bu_p <- region_bu_df %>%
  transform(regions = factor(regions, levels=c('Central', 'West', 'North East', 'South East'))) %>%
  ggplot(aes(x = year, y = bu_total_regions*0.000001)) +
  geom_point() +
  geom_line(group = 1) +
  scale_color_manual(values = c("#D62728","#1F77B4")) +
  scale_y_continuous(limits = c(0, 175)) +
  theme_pub()  +
  xlab("Year") + ylab("Building unit count (in 100,000 units)") +
  ggtitle('(a) Number of structures \nin the CONUS by region') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = "none") +
  facet_wrap(~regions, ncol = 1)

conus_bu_df <- sum_ecoregions_bu %>%
  gather(variable, bu, -ID_sp, -us_l3name) %>%
  separate(variable,
           into = c("statistic", 'tmp', "year"),
           sep = "_") %>%
  dplyr::select(-tmp) %>%
  group_by(year) %>%
  summarise(bu_total_conus = sum(bu)) %>%
  ungroup() %>%
  mutate(year = as.numeric(year))

conus_yearly <- tibble(year = rep(1992:2015)) %>%
  left_join(., conus_bu_df, by = 'year') %>%
  fill(everything(), .direction = 'up') %>%
  mutate(discovery_year = year) %>%
  dplyr::select(-year)

conus_bu_p <- conus_bu_df %>%
  ggplot(aes(x =  year, y = bu_total_conus*0.000001)) +
  geom_point() +
  geom_line(group = 1) +
  scale_color_manual(values = c("#D62728","#1F77B4")) +
  theme_pub()  +
  xlab("Year") + ylab("Building unit count (in 100,000 units)") +
  ggtitle('(a) Number of structures \nin the CONUS') +
  scale_y_continuous(limits = c(0, 45)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = "none")


# Threatened across the CONUS ---------------------------------------------

sum_fpa_bu_yearly <- bu_cleaned %>%
  group_by() %>%
  summarise(bu_total = sum(bu)) %>%
  ungroup()

lt_fpa_bu_yearly <- bu_cleaned %>%
  group_by(discovery_year) %>%
  summarise(bu_total = sum(bu)) %>%
  ungroup() %>%
  group_by() %>%
  summarise(lt_mean = mean(bu_total)) %>%
  ungroup()

fpa_bu_df <- bu_cleaned %>%
  group_by(discovery_year) %>%
  summarise(f_cnt = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(bu)) %>%
  left_join(., conus_yearly, by = 'discovery_year') %>%
  mutate(pct_total = bu/bu_total_conus * 100,
         pct_change = (bu - lag(bu))/lag(bu) * 100,
         home_per_fire = bu/f_cnt,
         home_per_areaburned = bu/burned_area,
         anomalies = bu - lt_fpa_bu_yearly$lt_mean,
         colour = ifelse(pct_change <= 0, "negative","positive"), 
         colourlt = ifelse(anomalies <= 0, "negative","positive")) %>%
  ungroup() %>%
  filter(!is.na(discovery_year)) %>%
  mutate(buff_zone = 0)

fpa_bu_p <- fpa_bu_df %>%
  ggplot(aes(x = discovery_year, y = bu*0.000001, color = 'red')) +
  geom_point(color = 'red') +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    # fullrange = TRUE,
    size = 1
  ) +
  theme_pub() +
  xlab("Year") + ylab("Building unit count (in 100,000 units)") +
  # ggtitle('(b) Number of structures \nwithin all wildfires') +
  # scale_y_continuous(limits = c(0, 6)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')

fpa_bu_doy <- bu_cleaned %>%
  group_by(discovery_doy) %>%
  summarise(doy_total = sum(bu)) %>%
  ungroup() %>%
  ggplot() +
  geom_bar(aes(x =  discovery_doy, y = doy_total*0.0001), stat = "identity", fill = 'red', colour = 'red') +
  theme_pub()  +
  xlab("Discovery day of year") + ylab("Number of homes threatened (in 1,000 units)") +
  # ggtitle('(b) Number of structures \nwithin all wildfires') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')


fpa_bu_pctot <- fpa_bu_df %>%
  ggplot(aes(x = discovery_year, y = pct_total, color = 'red')) +
  #geom_bar(stat = "identity", width = 0.01, fill = 'black', color = 'black') +
  geom_point(color = 'black', size = 3) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  theme_pub() +
  xlab("Year") + ylab("Yearly perecnt change of structures threatened by wildfire (%)") +
  # ggtitle('(c) Proportion of structures contained \nwithin wildfire to total') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')


fpa_bu_anom <- fpa_bu_df %>%
  ggplot(aes(x =  discovery_year)) +
  geom_bar(stat = "identity", position = "identity", aes(y = anomalies*0.000001, fill = colourlt)) +
  scale_fill_manual(values = c(positive = "firebrick1",
                               negative = "steelblue")) +
  theme_pub() +
  xlab("Year") + ylab("Homes threatened by wildfire anomalies (in 100,000 units)") +
  # ggtitle('(c) Proportion of structures contained \nwithin wildfire to total') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')

grid.arrange(fpa_bu_p, fpa_bu_doy, nrow = 1)
g <- arrangeGrob(fpa_bu_p, fpa_bu_doy, nrow = 1)
ggsave(file = "results/figs/draft/bu_fpa.pdf", g,
       width = 10, height = 5, dpi=1200, scale = 4, units = "cm")

grid.arrange(fpa_bu_pctot, fpa_bu_anom, nrow = 1)
g <- arrangeGrob(fpa_bu_pctot, fpa_bu_anom, nrow = 1)
ggsave(file = "results/figs/draft/bu_bar_fpa.pdf", g,
       width = 10, height = 5, dpi=1200, scale = 4, units = "cm")

# Threatened across CONUS stratified by CAUSE ---------------------------------------------

fpa_cause_bu_yearly <- bu_cleaned %>%
  group_by(ignition) %>%
  summarise(bu_total = sum(bu)) %>%
  ungroup() %>%
  na.omit()

lt_fpa_cause_bu_yearly <- bu_cleaned %>%
  group_by(discovery_year, ignition) %>%
  summarise(bu_total = sum(bu)) %>%
  ungroup() %>%
  group_by(ignition) %>%
  summarise(lt_mean = mean(bu_total)) %>%
  ungroup() %>%
  na.omit()

fpa_cause_bu_df <- bu_cleaned %>%
  group_by(ignition, discovery_year) %>%
  summarise(f_cnt = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(bu)) %>%
  left_join(., fpa_cause_bu_yearly, by = 'ignition') %>%
  left_join(., lt_fpa_cause_bu_yearly, by = 'ignition') %>%
  left_join(., conus_yearly, by = 'discovery_year') %>%
  mutate(pct_change = (bu - lag(bu))/lag(bu) * 100,
         pct_total = bu/bu_total_conus * 100,
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
    size = 1
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

doy_ignition <- bu_cleaned %>%
  group_by(discovery_doy, ignition) %>%
  summarise(doy_total = sum(bu)) %>%
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

fpa_cause_bu_anom <- fpa_cause_bu_df %>%
  ggplot(aes(x =  discovery_year, y = anomalies*0.000001, color = ignition, group = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'loess',
    fullrange = TRUE,
    size = 1
  ) +
  scale_color_manual(values = c("#b2182b",
                               '#2166ac')) +
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

fpa_cause_bu_pctot <- fpa_cause_bu_df %>%
  ggplot(aes(y = pct_total, x =  discovery_year, fill = ignition, color = ignition, group = ignition)) +
  geom_point(size = 3) +
  geom_smooth(
    method = 'glm',
    method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  theme_pub() +
  scale_fill_manual(values = c("#D62728","#1F77B4")) +
  scale_color_manual(values = c("#D62728","#1F77B4")) +
  xlab("Year") + ylab("Yearly perecnt change of structures threatened by wildfire (%)") +
  # ggtitle('(b) Proportion of structures contained \nwithin wildfire to total') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0"))

grid.arrange(fpa_cause_bu_p, fpa_cause_bu_doy, nrow = 1)
g <- arrangeGrob(fpa_cause_bu_p, fpa_cause_bu_doy, nrow = 1)
ggsave(file = "results/figs/draft/bu_cause_fpa.pdf", g,
       width = 10, height = 5, dpi=1200, scale = 4, units = "cm")

grid.arrange(fpa_cause_bu_pctot, fpa_cause_bu_anom, nrow = 1)
g <- arrangeGrob(fpa_cause_bu_pctot, fpa_cause_bu_anom, nrow = 1)
ggsave(file = "results/figs/draft/bu_cause_bar_fpa.pdf", g,
       width = 10, height = 5, dpi=1200, scale = 4, units = "cm")

# Threatened across CONUS stratified by CLASS ---------------------------------------------

class_fpa_bu_yearly <- bu_cleaned %>%
  filter(class_coarse != 'Other') %>%
  group_by(class_coarse) %>%
  summarise(bu_total = sum(bu)) %>%
  ungroup() %>%
  na.omit()

lt_class_fpa_bu_yearly <- bu_cleaned %>%
  filter(class_coarse != 'Other') %>%
  group_by(discovery_year, class_coarse) %>%
  summarise(bu_total = sum(bu)) %>%
  ungroup() %>%
  group_by(class_coarse) %>%
  summarise(lt_mean = mean(bu_total)) %>%
  ungroup() %>%
  na.omit()

class_fpa_bu_df <- bu_cleaned %>%
  filter(class_coarse != 'Other') %>%
  group_by(class_coarse, discovery_year) %>%
  summarise(f_cnt = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(bu)) %>%
  left_join(., class_fpa_bu_yearly, by = 'class_coarse') %>%
  left_join(., lt_class_fpa_bu_yearly, by = 'class_coarse') %>%
  left_join(., conus_yearly, by = 'discovery_year') %>%
  mutate(pct_total = bu/bu_total_conus * 100,
         pct_change = (bu - lag(bu))/lag(bu) * 100,
         home_per_fire = bu/f_cnt,
         home_per_areaburned = bu/burned_area,
         anomalies = bu - lt_mean,
         colour = ifelse(pct_change <= 0, "negative","positive"), 
         colourlt = ifelse(anomalies <= 0, "negative","positive")) %>%  ungroup() %>%
  filter(!is.na(discovery_year)) 

fpa_class_bu_p <- class_fpa_bu_df %>%
  transform(class_coarse = factor(class_coarse, levels=c("Urban", 'WUI', 'VLD', 'Wildlands'))) %>%
  ggplot(aes(x = discovery_year, y = bu*0.000001, group = class_coarse, color = class_coarse, fill = class_coarse)) +
  geom_point() +
  geom_smooth(
    method = 'glm',
    #method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  theme_pub() +
  scale_y_continuous(limits = c(0, 2)) +
  scale_color_manual(values = c("#b2182b","#ef8a62", '#67a9cf', '#2166ac')) +
  scale_fill_manual(values = c("#b2182b","#ef8a62", '#67a9cf', '#2166ac')) +
  xlab("Year") + ylab("Building unit count (in 100,000 units)") +
  #ggtitle('(b) Number of structures \nwithin all wildfires') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')

doy_class <- bu_cleaned %>%
  group_by(discovery_doy, class_coarse) %>%
  summarise(doy_total = sum(bu)) %>%
  ungroup() %>%
  transform(class_coarse = factor(class_coarse, levels=c("Urban", 'WUI', 'VLD', 'Wildlands')))

fpa_class_bu_doy <- ggplot() +
  geom_bar(data = filter(doy_class, class_coarse == 'Urban'), aes(x =  discovery_doy, y = doy_total*0.0001, colour = class_coarse, fill = class_coarse), stat = "identity") +
  geom_bar(data = filter(doy_class, class_coarse == 'WUI'), aes(x =  discovery_doy, y = doy_total*0.0001, fill = class_coarse, colour = class_coarse), stat = "identity") +
  geom_bar(data = filter(doy_class, class_coarse == 'VLD'), aes(x =  discovery_doy, y = doy_total*0.0001, fill = class_coarse, colour = class_coarse), stat = "identity") +
  geom_bar(data = filter(doy_class, class_coarse == 'Wildlands'), aes(x =  discovery_doy, y = doy_total*0.0001, fill = class_coarse, colour = class_coarse), stat = "identity") +
  theme_pub()  +
  scale_color_manual(values = c("#b2182b","#67a9cf", '#2166ac', '#ef8a62')) +
  scale_fill_manual(values = c("#b2182b","#67a9cf", '#2166ac', '#ef8a62')) +
  xlab("Discovery day of year") + ylab("Number of homes threatened (in 1,000 units)") +
  # ggtitle('(b) Number of structures \nwithin all wildfires') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')

fpa_class_bu_anom <- class_fpa_bu_df %>%
  transform(class_coarse = factor(class_coarse, levels=c("Urban", 'WUI', 'VLD', 'Wildlands'))) %>%
  ggplot(aes(x =  discovery_year)) +
  geom_bar(stat = "identity", position = "identity", aes(y = anomalies*0.000001, fill = colourlt)) +
  scale_fill_manual(values = c(positive = "firebrick1",
                               negative = "steelblue")) +
  theme_pub() +
  xlab("Year") + ylab("Homes threatened by wildfire anomalies (in 100,000 units)") +
  # ggtitle('(c) Proportion of structures contained \nwithin wildfire to total') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none') +
  facet_wrap(~ class_coarse, ncol = 1)

fpa_class_bu_pctot <- class_fpa_bu_df %>%
  transform(class_coarse = factor(class_coarse, levels=c("Urban", 'WUI', 'VLD', 'Wildlands'))) %>%
  ggplot(aes(y = pct_total, x =  discovery_year, fill = class_coarse, color = class_coarse, group = class_coarse)) +
  geom_point(size = 3) +
  geom_smooth(
    method = 'glm',
    #method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  theme_pub() +
  scale_color_manual(values = c("#b2182b","#67a9cf", '#2166ac', '#ef8a62')) +
  scale_fill_manual(values = c("#b2182b","#67a9cf", '#2166ac', '#ef8a62')) +
  xlab("Year") + ylab("Yearly perecnt change of structures threatened by wildfire (%)") +
  # ggtitle('(c) Proportion of structures contained \nwithin wildfire to total') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none') 

grid.arrange(fpa_class_bu_p, fpa_class_bu_doy, nrow = 1)
g <- arrangeGrob(fpa_class_bu_p, fpa_class_bu_doy, nrow = 1)
ggsave(file = "results/figs/draft/bu_class_fpa.pdf", g,
       width = 10, height = 5, dpi=1200, scale = 4, units = "cm")

grid.arrange(fpa_class_bu_pctot, fpa_class_bu_anom, nrow = 1)
g <- arrangeGrob(fpa_class_bu_pctot, fpa_class_bu_anom, nrow = 1)
ggsave(file = "results/figs/draft/bu_class_bar_fpa.pdf", g,
       width = 10, height = 5, dpi=1200, scale = 4, units = "cm")


# Threatened across CONUS stratified by SIZE ---------------------------------------------

size_fpa_bu_yearly <- bu_cleaned %>%
  group_by(size) %>%
  summarise(bu_total = sum(bu)) %>%
  ungroup() %>%
  na.omit()

lt_size_fpa_bu_yearly <- bu_cleaned %>%
  filter(size != 'Other') %>%
  group_by(discovery_year, size) %>%
  summarise(bu_total = sum(bu)) %>%
  ungroup() %>%
  group_by(size) %>%
  summarise(lt_mean = mean(bu_total)) %>%
  ungroup() %>%
  na.omit()

size_fpa_bu_df <- bu_cleaned %>%
  filter(size != 'Other') %>%
  group_by(size, discovery_year) %>%
  summarise(f_cnt = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(bu)) %>%
  left_join(., size_fpa_bu_yearly, by = 'size') %>%
  left_join(., lt_size_fpa_bu_yearly, by = 'size') %>%
  left_join(., conus_yearly, by = 'discovery_year') %>%
  mutate(pct_total = bu/bu_total_conus * 100,
         pct_change = (bu - lag(bu))/lag(bu) * 100,
         home_per_fire = bu/f_cnt,
         home_per_areaburned = bu/burned_area,
         anomalies = bu - lt_mean,
         colour = ifelse(pct_change <= 0, "negative","positive"), 
         colourlt = ifelse(anomalies <= 0, "negative","positive")) %>%  ungroup() %>%
  filter(!is.na(discovery_year)) %>%
  mutate(buff_zone = 0)

fpa_size_bu_p <- size_fpa_bu_df %>%
  transform(size = factor(size, levels=c('< 10 ha', "10 - 400 ha", '400 - 5000 ha', '5000 - 20000 ha', '> 20000 ha'))) %>%
  ggplot(aes(x = discovery_year, y = bu*0.000001, group = size, color = size)) +
  geom_point() +
  geom_smooth(
    method = 'glm',
    #method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  theme_pub() +
  scale_y_continuous(limits = c(0, 5)) +
  scale_color_manual(values = c('#fec44f',"#fe9929", "#ec7014", "#cc4c02", "#8c2d04")) +
  xlab("Year") + ylab("Building unit count (in 100,000 units)") +
  #ggtitle('(b) Number of structures \nwithin all wildfires') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')

doy_size <- bu_cleaned %>%
  group_by(discovery_doy, size) %>%
  summarise(doy_total = sum(bu)) %>%
  ungroup()

fpa_size_bu_doy <- ggplot() +
  geom_bar(data = filter(doy_size, size == '< 10 ha'), aes(x =  discovery_doy, y = doy_total*0.0001, colour = size, fill = size), stat = "identity") +
  geom_bar(data = filter(doy_size, size == '10 - 400 ha'), aes(x =  discovery_doy, y = doy_total*0.0001, fill = size, colour = size), stat = "identity") +
  geom_bar(data = filter(doy_size, size == '400 - 5000 ha'), aes(x =  discovery_doy, y = doy_total*0.0001, fill = size, colour = size), stat = "identity") +
  geom_bar(data = filter(doy_size, size == '5000 - 20000 ha'), aes(x =  discovery_doy, y = doy_total*0.0001, fill = size, colour = size), stat = "identity") +
  geom_bar(data = filter(doy_size, size == '> 20000 ha'), aes(x =  discovery_doy, y = doy_total*0.0001, fill = size, colour = size), stat = "identity") +
  theme_pub()  +
  scale_color_manual(values = c('#fec44f',"#fe9929", "#ec7014", "#cc4c02", "#8c2d04")) +
  scale_fill_manual(values =  c('#fec44f',"#fe9929", "#ec7014", "#cc4c02", "#8c2d04")) +
  xlab("Discovery day of year") + ylab("Number of homes threatened (in 1,000 units)") +
  # ggtitle('(b) Number of structures \nwithin all wildfires') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')

fpa_size_bu_anom <- size_fpa_bu_df %>%
  transform(size = factor(size, levels=c('< 10 ha', "10 - 400 ha", '400 - 5000 ha', '5000 - 20000 ha', '> 20000 ha'))) %>%
  ggplot(aes(x =  discovery_year)) +
  geom_bar(stat = "identity", position = "identity", aes(y = anomalies*0.000001, fill = colourlt)) +
  scale_fill_manual(values = c(positive = "firebrick1",
                               negative = "steelblue")) +
  theme_pub() +
  xlab("Year") + ylab("Homes threatened by wildfire anomalies (in 100,000 units)") +
  # ggtitle('(c) Proportion of structures contained \nwithin wildfire to total') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none') +
  facet_wrap(~ size, ncol = 1)

fpa_size_bu_pctot <- size_fpa_bu_df %>%
  transform(size = factor(size, levels=c('< 10 ha', "10 - 400 ha", '400 - 5000 ha', '5000 - 20000 ha', '> 20000 ha'))) %>%
  ggplot(aes(y = pct_total, x =  discovery_year, fill = size, color = size, group = size)) +
  geom_point(size = 3) +
  geom_smooth(
    method = 'glm',
    #method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  theme_pub() +
  scale_color_manual(values = c('#fec44f',"#fe9929", "#ec7014", "#cc4c02", "#8c2d04")) +
  scale_fill_manual(values =  c('#fec44f',"#fe9929", "#ec7014", "#cc4c02", "#8c2d04")) +
  xlab("Year") + ylab("Yearly perecnt change of structures threatened by wildfire (%)") +
  # ggtitle('(c) Proportion of structures contained \nwithin wildfire to total') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none') 

grid.arrange(fpa_size_bu_p, fpa_size_bu_doy, nrow = 1)
g <- arrangeGrob(fpa_size_bu_p, fpa_size_bu_doy, nrow = 1)
ggsave(file = "results/figs/draft/bu_size_fpa.pdf", g,
       width = 10, height = 5, dpi=1200, scale = 4, units = "cm")

grid.arrange(fpa_size_bu_pctot, fpa_size_bu_anom, nrow = 1)
g <- arrangeGrob(fpa_size_bu_pctot, fpa_size_bu_anom, nrow = 1)
ggsave(file = "results/figs/draft/bu_size_bar_fpa.pdf", g,
       width = 10, height = 5, dpi=1200, scale = 4, units = "cm")


# Threatened across CONUS stratified by SIZE EXCLUDING FIRES < 1 ha---------------------------------------------

ssize_fpa_bu_yearly <- bu_cleaned %>%
  filter(fire_size_km2 >= 1) %>%
  group_by(size) %>%
  summarise(bu_total = sum(bu)) %>%
  ungroup() %>%
  na.omit()

lt_ssize_fpa_bu_yearly <- bu_cleaned %>%
  filter(fire_size_km2 >= 1) %>%
  group_by(discovery_year, size) %>%
  summarise(bu_total = sum(bu)) %>%
  ungroup() %>%
  group_by(size) %>%
  summarise(lt_mean = mean(bu_total)) %>%
  ungroup() %>%
  na.omit()

ssize_fpa_bu_df <- bu_cleaned %>%
  filter(fire_size_km2 >= 1) %>%
  group_by(size, discovery_year) %>%
  summarise(f_cnt = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(bu)) %>%
  left_join(., ssize_fpa_bu_yearly, by = 'size') %>%
  left_join(., lt_ssize_fpa_bu_yearly, by = 'size') %>%
  left_join(., conus_yearly, by = 'discovery_year') %>%
  mutate(pct_total = bu/bu_total_conus * 100,
         pct_change = (bu - lag(bu))/lag(bu) * 100,
         home_per_fire = bu/f_cnt,
         home_per_areaburned = bu/burned_area,
         anomalies = bu - lt_mean,
         colour = ifelse(pct_change <= 0, "negative","positive"), 
         colourlt = ifelse(anomalies <= 0, "negative","positive")) %>%  ungroup() %>%
  filter(!is.na(discovery_year)) %>%
  mutate(buff_zone = 0)

fpa_ssize_bu_p <- ssize_fpa_bu_df %>%
  transform(size = factor(size, levels=c('< 10 ha', "10 - 400 ha", '400 - 5000 ha', '5000 - 20000 ha', '> 20000 ha'))) %>%
  ggplot(aes(x = discovery_year, y = bu*0.000001, group = size, color = size)) +
  geom_point() +
  geom_smooth(
    method = 'glm',
    #method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  theme_pub() +
  scale_color_manual(values = c('#fec44f',"#fe9929", "#ec7014", "#cc4c02", "#8c2d04")) +
  xlab("Year") + ylab("Building unit count (in 100,000 units)") +
  #ggtitle('(b) Number of structures \nwithin all wildfires') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')

doy_ssize <- bu_cleaned %>%
  filter(fire_size_km2 >= 1) %>%
  group_by(discovery_doy, size) %>%
  summarise(doy_total = sum(bu)) %>%
  ungroup()

fpa_ssize_bu_doy <- ggplot() +
  geom_bar(data = filter(doy_ssize, size == '< 10 ha'), aes(x =  discovery_doy, y = doy_total*0.0001, colour = size, fill = size), stat = "identity") +
  geom_bar(data = filter(doy_ssize, size == '10 - 400 ha'), aes(x =  discovery_doy, y = doy_total*0.0001, fill = size, colour = size), stat = "identity") +
  geom_bar(data = filter(doy_ssize, size == '400 - 5000 ha'), aes(x =  discovery_doy, y = doy_total*0.0001, fill = size, colour = size), stat = "identity") +
  geom_bar(data = filter(doy_ssize, size == '5000 - 20000 ha'), aes(x =  discovery_doy, y = doy_total*0.0001, fill = size, colour = size), stat = "identity") +
  geom_bar(data = filter(doy_ssize, size == '> 20000 ha'), aes(x =  discovery_doy, y = doy_total*0.0001, fill = size, colour = size), stat = "identity") +
  theme_pub()  +
  scale_color_manual(values = c('#fec44f',"#fe9929", "#ec7014", "#cc4c02", "#8c2d04")) +
  scale_fill_manual(values =  c('#fec44f',"#fe9929", "#ec7014", "#cc4c02", "#8c2d04")) +
  xlab("Year") + ylab("Number of homes threatened per wildfire discovery day of year") +
  xlab("Discovery day of year") + ylab("Number of homes threatened (in 1,000 units)") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')

fpa_ssize_bu_anom <- ssize_fpa_bu_df %>%
  transform(size = factor(size, levels=c('< 10 ha', "10 - 400 ha", '400 - 5000 ha', '5000 - 20000 ha', '> 20000 ha'))) %>%
  ggplot(aes(x =  discovery_year)) +
  geom_bar(stat = "identity", position = "identity", aes(y = anomalies*0.000001, fill = colourlt)) +
  scale_fill_manual(values = c(positive = "firebrick1",
                               negative = "steelblue")) +
  theme_pub() +
  xlab("Year") + ylab("Yearly perecnt change of structures threatened by wildfire (%)") +
  # ggtitle('(c) Proportion of structures contained \nwithin wildfire to total') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none') +
  facet_wrap(~ size, ncol = 1, scales = 'free')

fpa_ssize_bu_pctot <- ssize_fpa_bu_df %>%
  transform(size = factor(size, levels=c('< 10 ha', "10 - 400 ha", '400 - 5000 ha', '5000 - 20000 ha', '> 20000 ha'))) %>%
  ggplot(aes(y = pct_total, x =  discovery_year, fill = size, color = size, group = size)) +
  geom_point(size = 3) +
  geom_smooth(
    method = 'glm',
    #method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  theme_pub() +
  scale_color_manual(values = c('#fec44f',"#fe9929", "#ec7014", "#cc4c02", "#8c2d04")) +
  scale_fill_manual(values =  c('#fec44f',"#fe9929", "#ec7014", "#cc4c02", "#8c2d04")) +
  xlab("Year") + ylab("Yearly perecnt change of structures threatened by wildfire (%)") +
  # ggtitle('(c) Proportion of structures contained \nwithin wildfire to total') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none') 


grid.arrange(fpa_ssize_bu_p, fpa_ssize_bu_doy, nrow = 1)
g <- arrangeGrob(fpa_ssize_bu_p, fpa_ssize_bu_doy, nrow = 1)
ggsave(file = "results/figs/draft/bu_ssize_fpa.pdf", g,
       width = 10, height = 5, dpi=1200, scale = 4, units = "cm")

grid.arrange(fpa_ssize_bu_pctot, fpa_ssize_bu_anom, nrow = 1)
g <- arrangeGrob(fpa_ssize_bu_pctot, fpa_ssize_bu_anom, nrow = 1)
ggsave(file = "results/figs/draft/bu_ssize_bar_fpa.pdf", g,
       width = 10, height = 5, dpi=1200, scale = 4, units = "cm")

# Threatened across REGIONS ---------------------------------------------

region_fpa_bu_yearly <- bu_cleaned %>%
  group_by(regions) %>%
  summarise(bu_total = sum(bu)) %>%
  ungroup() %>%
  na.omit()

lt_region_fpa_bu_yearly <- bu_cleaned %>%
  group_by(discovery_year, regions) %>%
  summarise(bu_total = sum(bu)) %>%
  ungroup() %>%
  group_by(regions) %>%
  summarise(lt_mean = mean(bu_total)) %>%
  ungroup() %>%
  na.omit()

region_fpa_bu_df <- bu_cleaned %>%
  group_by(regions, discovery_year) %>%
  summarise(f_cnt = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(bu)) %>%
  left_join(., region_fpa_bu_yearly, by = 'regions') %>%
  left_join(., lt_region_fpa_bu_yearly, by = 'regions') %>%
  left_join(., region_yearly, by = c('regions', 'discovery_year')) %>%
  mutate(pct_total = bu/bu_total_regions * 100,
         pct_change = (bu - lag(bu))/lag(bu) * 100,
         home_per_fire = bu/f_cnt,
         home_per_areaburned = bu/burned_area,
         anomalies = bu - lt_mean,
         colour = ifelse(pct_change <= 0, "negative","positive"), 
         colourlt = ifelse(anomalies <= 0, "negative","positive")) %>%  ungroup() %>%
  filter(!is.na(discovery_year)) %>%
  mutate(buff_zone = 0)

region_fpa_bu_p <- region_fpa_bu_df %>%
  transform(regions = factor(regions, levels=c("North East", "South East", 'Central', 'West'))) %>%
  ggplot(aes(x = discovery_year, y = bu*0.000001, group = regions, color = regions, fill = regions)) +
  geom_point() +
  geom_smooth(
    method = 'glm',
    #method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  theme_pub() +
  scale_color_manual(values = c('#b2182b',"#5ab4ac", "#8c510a", '#4575b4')) +
  scale_fill_manual(values = c('#b2182b',"#5ab4ac", "#8c510a", '#4575b4')) +
  xlab("Year") + ylab("Building unit count (in 100,000 units)") +
  # ggtitle('(b) Number of structures \nwithin all wildfires') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')

doy_region <- bu_cleaned %>%
  group_by(discovery_doy, regions) %>%
  summarise(doy_total = sum(bu)) %>%
  ungroup()

fpa_region_bu_doy <- ggplot() +
  geom_bar(data = filter(doy_region, regions == 'North East'), aes(x =  discovery_doy, y = doy_total*0.0001, colour = regions, fill = regions), stat = "identity", alpha = 0.5) +
  geom_bar(data = filter(doy_region, regions == 'South East'), aes(x =  discovery_doy, y = doy_total*0.0001, colour = regions, fill = regions), stat = "identity", alpha = 0.5) +
  geom_bar(data = filter(doy_region, regions == 'West'), aes(x =  discovery_doy, y = doy_total*0.0001, fill = regions, colour = regions), stat = "identity", alpha = 0.5) +
  geom_bar(data = filter(doy_region, regions == 'Central'), aes(x =  discovery_doy, y = doy_total*0.0001, fill = regions, colour = regions), stat = "identity", alpha = 0.5) +
  theme_pub()  +
  scale_color_manual(values = c('#b2182b',"#5ab4ac", "#8c510a", '#4575b4')) +
  scale_fill_manual(values = c('#b2182b',"#5ab4ac", "#8c510a", '#4575b4')) +
  xlab("Discovery day of year") + ylab("Number of homes threatened (in 1,000 units)") +
  # ggtitle('(b) Number of structures \nwithin all wildfires') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')

fpa_region_bu_anom <- region_fpa_bu_df %>%
  transform(regions = factor(regions, levels=c("North East", 'South East', 'Central', 'West'))) %>%
  ggplot(aes(x =  discovery_year)) +
  geom_bar(stat = "identity", position = "identity", aes(y = anomalies*0.000001, fill = colourlt)) +
  scale_fill_manual(values = c(positive = "firebrick1",
                               negative = "steelblue")) +
  theme_pub() +
  xlab("Year") + ylab("Yearly perecnt change of structures threatened by wildfire (%)") +
  # ggtitle('(c) Proportion of structures contained \nwithin wildfire to total') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none') +
  facet_wrap(~ regions, ncol = 1)

fpa_region_bu_pctot <- region_fpa_bu_df %>%
  transform(regions = factor(regions, levels=c("North East", 'South East', 'Central', 'West'))) %>%
  ggplot(aes(y = pct_total, x =  discovery_year, fill = regions, color = regions, group = regions)) +
  geom_point() +
  geom_smooth(
    method = 'glm',
    #method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  theme_pub() +
  scale_color_manual(values = c('#b2182b',"#5ab4ac", "#8c510a", '#4575b4')) +
  scale_fill_manual(values = c('#b2182b',"#5ab4ac", "#8c510a", '#4575b4')) +
  xlab("Year") + ylab("Yearly perecnt change of structures threatened by wildfire (%)") +
  # ggtitle('(c) Proportion of structures contained \nwithin wildfire to total') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')

grid.arrange(region_fpa_bu_p, fpa_region_bu_doy, nrow = 1)
g <- arrangeGrob(region_fpa_bu_p, fpa_region_bu_doy, nrow = 1)
ggsave(file = "results/figs/draft/bu_regions_fpa.pdf", g,
       width = 10, height = 5, dpi=1200, scale = 4, units = "cm")

grid.arrange(fpa_region_bu_pctot, fpa_region_bu_anom, nrow = 1)
g <- arrangeGrob(fpa_region_bu_pctot, fpa_region_bu_anom, nrow = 1)
ggsave(file = "results/figs/draft/bu_regions_bar_fpa.pdf", g,
       width = 10, height = 5, dpi=1200, scale = 4, units = "cm")

# Threatened across REGION stratified by CAUSE ---------------------------------------------

fpa_region_cause_bu_yearly <- bu_cleaned %>%
  group_by(regions, ignition) %>%
  summarise(bu_total = sum(bu)) %>%
  ungroup() %>%
  na.omit()

lt_fpa_region_cause_bu_yearly <- bu_cleaned %>%
  group_by(discovery_year, regions, ignition) %>%
  summarise(bu_total = sum(bu)) %>%
  ungroup() %>%
  group_by(regions, ignition) %>%
  summarise(lt_mean = mean(bu_total)) %>%
  ungroup() %>%
  na.omit()

fpa_region_cause_bu_df <- bu_cleaned %>%
  group_by(regions, ignition, discovery_year) %>%
  summarise(f_cnt = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(bu)) %>%
  left_join(., fpa_region_cause_bu_yearly, by = c('regions', 'ignition')) %>%
  left_join(., lt_fpa_region_cause_bu_yearly, by = c('regions', 'ignition')) %>%
  left_join(., region_yearly, by = c('regions', 'discovery_year')) %>%
  mutate(pct_total = bu/bu_total_regions * 100,
         pct_change = (bu - lag(bu))/lag(bu) * 100,
         home_per_fire = bu/f_cnt,
         home_per_areaburned = bu/burned_area,
         anomalies = bu - lt_mean,
         colour = ifelse(pct_change <= 0, "negative","positive"), 
         colourlt = ifelse(anomalies <= 0, "negative","positive")) %>%
  ungroup() %>%
  filter(!is.na(discovery_year)) %>%
  mutate(buff_zone = 0)

cause_region_fpa_bu_p <- fpa_region_cause_bu_df %>%
  transform(regions = factor(regions, levels=c("North East", "South East", 'Central', 'West'))) %>%
  ggplot(aes(x = discovery_year, y = bu*0.000001, group = regions, color = regions, fill = regions)) +
  geom_point() +
  geom_smooth(
    method = 'glm',
    #method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  theme_pub() +
  scale_color_manual(values = c('#b2182b',"#5ab4ac", "#8c510a", '#4575b4')) +
  scale_fill_manual(values = c('#b2182b',"#5ab4ac", "#8c510a", '#4575b4')) +
  xlab("Year") + ylab("Building unit count (in 100,000 units)") +
  # ggtitle('(b) Number of structures \nwithin all wildfires') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none') +
  facet_wrap(~ ignition, ncol = 1, scales = 'free_y')

doy_region_ignition <- bu_cleaned %>%
  group_by(discovery_doy, ignition, regions) %>%
  summarise(doy_total = sum(bu)) %>%
  ungroup()

fpa_region_ignition_bu_doy <- ggplot() +
  geom_bar(data = filter(doy_region_ignition, regions == 'North East'), aes(x =  discovery_doy, y = doy_total*0.0001, colour = regions, fill = regions), stat = "identity", alpha = 0.5) +
  geom_bar(data = filter(doy_region_ignition, regions == 'South East'), aes(x =  discovery_doy, y = doy_total*0.0001, colour = regions, fill = regions), stat = "identity", alpha = 0.5) +
  geom_bar(data = filter(doy_region_ignition, regions == 'West'), aes(x =  discovery_doy, y = doy_total*0.0001, fill = regions, colour = regions), stat = "identity", alpha = 0.5) +
  geom_bar(data = filter(doy_region_ignition, regions == 'Central'), aes(x =  discovery_doy, y = doy_total*0.0001, fill = regions, colour = regions), stat = "identity", alpha = 0.5) +
  theme_pub()  +
  scale_color_manual(values = c('#b2182b',"#5ab4ac", "#8c510a", '#4575b4')) +
  scale_fill_manual(values = c('#b2182b',"#5ab4ac", "#8c510a", '#4575b4')) +
  xlab("Discovery day of year") + ylab("Number of homes threatened (in 1,000 units)") +
  # ggtitle('(b) Number of structures \nwithin all wildfires') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none') +
  facet_wrap(~ ignition, ncol = 1, scales = 'free_y')


fpa_region_ignition_bu_anom <- fpa_region_cause_bu_df %>%
  transform(regions = factor(regions, levels=c("North East", 'South East', 'Central', 'West'))) %>%
  ggplot(aes(x =  discovery_year)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), aes(y = anomalies*0.000001, fill = interaction(ignition, colourlt), group = interaction(ignition, colourlt))) +
  scale_fill_manual(values = c("#d1e5f0", '#2166ac',
                               '#fddbc7', '#b2182b')) +
  
  theme_pub() +
  xlab("Year") + ylab("Yearly perecnt change of structures threatened by wildfire (%)") +
  # ggtitle('(c) Proportion of structures contained \nwithin wildfire to total') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none') +
  facet_wrap(~ regions, ncol = 1, scales = 'free_y')

fpa_region_ignition_bu_pctot <- fpa_region_cause_bu_df %>%
  transform(regions = factor(regions, levels=c("North East", 'South East', 'Central', 'West'))) %>%
  ggplot(aes(y = pct_total, x =  discovery_year, fill = regions, color = regions, group = regions)) +
  geom_point() +
  geom_smooth(
    method = 'glm',
    #method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  theme_pub() +
  scale_color_manual(values = c('#b2182b',"#5ab4ac", "#8c510a", '#4575b4')) +
  scale_fill_manual(values = c('#b2182b',"#5ab4ac", "#8c510a", '#4575b4')) +
  xlab("Year") + ylab("Yearly perecnt change of structures threatened by wildfire (%)") +
  # ggtitle('(c) Proportion of structures contained \nwithin wildfire to total') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none') +
  facet_wrap(~ ignition, ncol = 1, scales = 'free_y')

grid.arrange(cause_region_fpa_bu_p, fpa_region_ignition_bu_doy, nrow = 1)
g <- arrangeGrob(cause_region_fpa_bu_p, fpa_region_ignition_bu_doy, nrow = 1)
ggsave(file = "results/figs/draft/bu_cause_region_fpa.pdf", g,
       width = 10, height = 5, dpi=1200, scale = 4, units = "cm")

grid.arrange(fpa_region_ignition_bu_pctot, fpa_region_ignition_bu_anom, nrow = 1)
g <- arrangeGrob(fpa_region_ignition_bu_pctot, fpa_region_ignition_bu_anom, nrow = 1)
ggsave(file = "results/figs/draft/bu_cause_region_bar_fpa.pdf", g,
       width = 10, height = 5, dpi=1200, scale = 4, units = "cm")

system('aws s3 sync results s3://earthlab-natem/human-ignitions-wui/results')
