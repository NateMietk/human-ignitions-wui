
fpa_wui <- fpa_wui %>%
  mutate(bidecadal = ifelse(DISCOVERY_YEAR >= 1991 & DISCOVERY_YEAR <= 1995, 1995,
                            ifelse(DISCOVERY_YEAR >= 1996 & DISCOVERY_YEAR <= 2000, 2000,
                                   ifelse(DISCOVERY_YEAR >= 2001 & DISCOVERY_YEAR <= 2005, 2005,
                                          ifelse(DISCOVERY_YEAR >= 2006 & DISCOVERY_YEAR <= 2010, 2010,
                                                 ifelse(DISCOVERY_YEAR >= 2011 & DISCOVERY_YEAR <= 2015, 2015,
                                                        DISCOVERY_YEAR ))))),
         pop_den = ifelse( DISCOVERY_YEAR >= 1992 | DISCOVERY_YEAR < 2000, POPDEN1990,
                           ifelse( DISCOVERY_YEAR >= 2000 | DISCOVERY_YEAR < 2009, POPDEN2000,
                                   ifelse( DISCOVERY_YEAR >= 2010 | DISCOVERY_YEAR < 2016, POPDEN2010, NA ))),
         house_den = ifelse( DISCOVERY_YEAR >= 1992 | DISCOVERY_YEAR < 2000, HUDEN1990,
                             ifelse( DISCOVERY_YEAR >= 2000 | DISCOVERY_YEAR < 2009, HUDEN2000,
                                     ifelse( DISCOVERY_YEAR >= 2010 | DISCOVERY_YEAR < 2016, HUDEN2010, NA ))),
         class_coarse =  ifelse( Class == 'High Urban' | Class == 'Med Urban' | Class == 'Low Urban', 'Urban',
                                 ifelse( Class == 'Intermix WUI' | Class == 'Interface WUI', 'WUI', as.character(Class))),
         size = classify_fire_size_cl(FIRE_SIZE_km2))

bu_cleaned <- sum_fpa_bu %>%
  gather(variable, bu, -ID_sp, -fpa_id) %>%
  separate(variable,
           into = c("statistic", 'tmp', "year"),
           sep = "_") %>%
  dplyr::select(-tmp) %>%
  left_join(., as.data.frame(fpa_wui) %>%
              setNames(tolower(names(.))),
            by = "fpa_id")


# Prep CONUS and REGIONS ---------------------------------------------

region_bu_df <- sum_ecoregions_bu %>%
  gather(variable, bu, -ID_sp, -us_l3name) %>%
  separate(variable,
           into = c("statistic", 'tmp', "bidecadal"),
           sep = "_") %>%
  dplyr::select(-tmp) %>%
  left_join(., as.data.frame(ecoreg), by = "us_l3name") %>%
  group_by(bidecadal, region) %>%
  summarise(bu_total = sum(bu)) %>%
  ungroup() %>%
  mutate(bidecadal = as.numeric(bidecadal)) %>%
  dplyr::select(bidecadal, region, bu_total)

region_bu_p <- region_bu_df %>%
  transform(region = factor(region, levels=c("East", 'Central', 'West'))) %>%
  ggplot(aes(x = bidecadal, y = bu_total*0.000001)) +
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
  facet_wrap(~region, ncol = 1)

conus_bu_df <- sum_ecoregions_bu %>%
  gather(variable, bu, -ID_sp, -us_l3name) %>%
  separate(variable,
           into = c("statistic", 'tmp', "bidecadal"),
           sep = "_") %>%
  dplyr::select(-tmp) %>%
  #
  group_by(bidecadal) %>%
  summarise(bu_total = sum(bu)) %>%
  ungroup() %>%
  mutate(bidecadal = as.numeric(bidecadal))

conus_bu_p <- conus_bu_df %>%
  ggplot(aes(x =  bidecadal, y = bu_total*0.000001)) +
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

fpa_bu_df <- bu_cleaned %>%
  group_by(discovery_year) %>%
  summarise(f_cnt = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(bu)) %>%
  mutate(pct_change = (bu - lag(bu))/lag(bu) * 100,
         pct_total = bu/sum_fpa_bu_yearly$bu_total * 100,
         home_per_fire = bu/f_cnt,
         home_per_areaburned = bu/burned_area,
         colour = ifelse(pct_change <= 0, "negative","positive")) %>%
  ungroup() %>%
  filter(!is.na(discovery_year)) %>%
  mutate(buff_zone = 0)

fpa_bu_p <- fpa_bu_df %>%
  ggplot(aes(x = discovery_year, y = bu*0.000001, color = 'red')) +
  geom_point(color = 'red') +
  geom_smooth(
    method = 'glm',
    #method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  theme_pub() +
  xlab("Year") + ylab("Building unit count (in 100,000 units)") +
  # ggtitle('(b) Number of structures \nwithin all wildfires') +
  scale_y_continuous(limits = c(0, 5)) +
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
  geom_bar(aes(x =  discovery_doy, y = doy_total), stat = "identity", fill = 'red', colour = 'red') +
  theme_pub()  +
  xlab("Year") + ylab("Number of homes threatened per wildfire discovery day of year") +
  # ggtitle('(b) Number of structures \nwithin all wildfires') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')


fpa_bu_pctot <- fpa_bu_df %>%
  ggplot(aes(x =  discovery_year)) +
  geom_bar(stat = "identity", position = "identity", aes(y = pct_change, fill = colour)) +
  scale_fill_manual(values = c(positive = "firebrick1",
                               negative = "steelblue")) +
  theme_pub() +
  xlab("Year") + ylab("Yearly perecnt change of structures threatened by wildfire (%)") +
  # ggtitle('(c) Proportion of structures contained \nwithin wildfire to total') +
  scale_y_continuous(limits = c(-40, 65)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')

grid.arrange(fpa_bu_p, fpa_bu_pctot, fpa_bu_doy, nrow = 1)
g <- arrangeGrob(fpa_bu_p, fpa_bu_pctot, fpa_bu_doy, nrow = 1)
ggsave(file = "results/figs/draft/bu_fpa.pdf", g,
       width = 10, height = 6, dpi=1200, scale = 4, units = "cm")

# Threatened across CONUS stratified by CAUSE ---------------------------------------------

fpa_cause_bu_yearly <- bu_cleaned %>%
  group_by(ignition) %>%
  summarise(bu_total = sum(bu)) %>%
  ungroup() %>%
  na.omit()

fpa_cause_bu_df <- bu_cleaned %>%
  group_by(ignition, discovery_year) %>%
  summarise(f_cnt = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(bu)) %>%
  left_join(., fpa_cause_bu_yearly, by = 'ignition') %>%
  mutate(pct_change = (bu - lag(bu))/lag(bu) * 100,
         pct_total = bu/bu_total * 100,
         home_per_fire = bu/f_cnt,
         home_per_areaburned = bu/burned_area,
         colour = ifelse(pct_change <= 0, "negative","positive")) %>%
  ungroup() %>%
  filter(!is.na(discovery_year)) %>%
  mutate(buff_zone = 0)

fpa_cause_bu_p <- fpa_cause_bu_df %>%
  ggplot(aes(x = discovery_year, y = bu*0.000001, group = ignition, color = ignition)) +
  geom_point() +
  geom_smooth(
    method = 'glm',
    #method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  theme_pub() +
  xlab("Year") + ylab("Building unit count (in 100,000 units)") +
  scale_color_manual(values = c("#D62728","#1F77B4")) +
  # ggtitle('(a) Number of structures \nwithin all wildfires') +
  scale_y_continuous(limits = c(0, 5)) +
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
  geom_bar(data = filter(doy_ignition, ignition == 'Human'), aes(x =  discovery_doy, y = doy_total, colour = ignition, fill = ignition), stat = "identity") +
  geom_bar(data = filter(doy_ignition, ignition == 'Lightning'), aes(x =  discovery_doy, y = doy_total, fill = ignition, colour = ignition), stat = "identity") +
  theme_pub()  +
  scale_color_manual(values = c("#D62728","#1F77B4")) +
  scale_fill_manual(values = c("#D62728","#1F77B4")) +
  xlab("Year") + ylab("Number of homes threatened per wildfire discovery day of year") +
  # ggtitle('(b) Number of structures \nwithin all wildfires') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')

fpa_cause_bu_pctot <- fpa_cause_bu_df %>%
  ggplot(aes(x =  discovery_year)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), aes(y = pct_change, fill = interaction(ignition, colour), group = interaction(ignition, colour))) +
  scale_fill_manual(values = c("#d1e5f0", '#2166ac',
                               '#fddbc7', '#b2182b')) +
  theme_pub() +
  xlab("Year") + ylab("Yearly perecnt change of structures threatened by wildfire (%)") +
  # ggtitle('(b) Proportion of structures contained \nwithin wildfire to total') +
  scale_y_continuous(limits = c(-50, 125)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none',
        panel.grid.minor = element_line(colour="#f0f0f0"))

grid.arrange(fpa_cause_bu_p, fpa_cause_bu_pctot, fpa_cause_bu_doy, nrow = 1)
g <- arrangeGrob(fpa_cause_bu_p, fpa_cause_bu_pctot, fpa_cause_bu_doy, nrow = 1)
ggsave(file = "results/figs/draft/bu_cause_fpa.pdf", g,
       width = 10, height = 6, dpi=1200, scale = 4, units = "cm")


# Threatened across CONUS stratified by CLASS ---------------------------------------------

class_fpa_bu_yearly <- bu_cleaned %>%
  filter(class_coarse != 'Other') %>%
  group_by(class_coarse) %>%
  summarise(bu_total = sum(bu)) %>%
  ungroup() %>%
  na.omit()

class_fpa_bu_df <- bu_cleaned %>%
  filter(class_coarse != 'Other') %>%
  group_by(class_coarse, discovery_year) %>%
  summarise(f_cnt = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(bu)) %>%
  left_join(., class_fpa_bu_yearly, by = 'class_coarse') %>%
  mutate(pct_change = (bu - lag(bu))/lag(bu) * 100,
         pct_total = bu/sum_fpa_bu_yearly$bu_total * 100,
         home_per_fire = bu/f_cnt,
         home_per_areaburned = bu/burned_area,
         colour = ifelse(pct_change <= 0, "negative","positive")) %>%
  ungroup() %>%
  filter(!is.na(discovery_year)) %>%
  mutate(buff_zone = 0)

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
  ggtitle('(b) Number of structures \nwithin all wildfires') +
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
  geom_bar(data = filter(doy_class, class_coarse == 'Urban'), aes(x =  discovery_doy, y = doy_total, colour = class_coarse, fill = class_coarse), stat = "identity") +
  geom_bar(data = filter(doy_class, class_coarse == 'WUI'), aes(x =  discovery_doy, y = doy_total, fill = class_coarse, colour = class_coarse), stat = "identity") +
  geom_bar(data = filter(doy_class, class_coarse == 'VLD'), aes(x =  discovery_doy, y = doy_total, fill = class_coarse, colour = class_coarse), stat = "identity") +
  geom_bar(data = filter(doy_class, class_coarse == 'Wildlands'), aes(x =  discovery_doy, y = doy_total, fill = class_coarse, colour = class_coarse), stat = "identity") +
  theme_pub()  +
  scale_color_manual(values = c("#b2182b","#67a9cf", '#2166ac', '#ef8a62')) +
  scale_fill_manual(values = c("#b2182b","#67a9cf", '#2166ac', '#ef8a62')) +
  xlab("Year") + ylab("Number of homes threatened per wildfire discovery day of year") +
  # ggtitle('(b) Number of structures \nwithin all wildfires') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')


fpa_class_bu_pctot <- class_fpa_bu_df %>%
  transform(class_coarse = factor(class_coarse, levels=c("Urban", 'WUI', 'VLD', 'Wildlands'))) %>%
  ggplot(aes(x =  discovery_year)) +
  geom_bar(stat = "identity", position = "identity", aes(y = pct_change, fill = colour)) +
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
  facet_wrap(~ class_coarse, ncol = 1)

grid.arrange(fpa_class_bu_p, fpa_class_bu_pctot, fpa_class_bu_doy, nrow = 1)
g <- arrangeGrob(fpa_class_bu_p, fpa_class_bu_pctot, fpa_class_bu_doy, nrow = 1)
ggsave(file = "results/figs/draft/bu_class_fpa.pdf", g,
       width = 10, height = 6, dpi=1200, scale = 4, units = "cm")


# Threatened across CONUS stratified by SIZE ---------------------------------------------

size_fpa_bu_yearly <- bu_cleaned %>%
  group_by(size) %>%
  summarise(bu_total = sum(bu)) %>%
  ungroup() %>%
  na.omit()

size_fpa_bu_df <- bu_cleaned %>%
  group_by(size, discovery_year) %>%
  summarise(f_cnt = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(bu)) %>%
  left_join(., size_fpa_bu_yearly, by = 'size') %>%
  mutate(pct_change = (bu - lag(bu))/lag(bu) * 100,
         pct_total = bu/sum_fpa_bu_yearly$bu_total * 100,
         home_per_fire = bu/f_cnt,
         home_per_areaburned = bu/burned_area,
         colour = ifelse(pct_change <= 0, "negative","positive")) %>%
  ungroup() %>%
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
  xlab("Year") + ylab("") +
  ggtitle('(b) Number of structures \nwithin all wildfires') +
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
  geom_bar(data = filter(doy_size, size == '< 10 ha'), aes(x =  discovery_doy, y = doy_total, colour = size, fill = size), stat = "identity") +
  geom_bar(data = filter(doy_size, size == '10 - 400 ha'), aes(x =  discovery_doy, y = doy_total, fill = size, colour = size), stat = "identity") +
  geom_bar(data = filter(doy_size, size == '400 - 5000 ha'), aes(x =  discovery_doy, y = doy_total, fill = size, colour = size), stat = "identity") +
  geom_bar(data = filter(doy_size, size == '5000 - 20000 ha'), aes(x =  discovery_doy, y = doy_total, fill = size, colour = size), stat = "identity") +
  geom_bar(data = filter(doy_size, size == '> 20000 ha'), aes(x =  discovery_doy, y = doy_total, fill = size, colour = size), stat = "identity") +
  theme_pub()  +
  scale_color_manual(values = c('#fec44f',"#fe9929", "#ec7014", "#cc4c02", "#8c2d04")) +
  scale_fill_manual(values =  c('#fec44f',"#fe9929", "#ec7014", "#cc4c02", "#8c2d04")) +
  xlab("Year") + ylab("Number of homes threatened per wildfire discovery day of year") +
  # ggtitle('(b) Number of structures \nwithin all wildfires') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')


fpa_size_bu_pctot <- size_fpa_bu_df %>%
  transform(size = factor(size, levels=c('< 10 ha', "10 - 400 ha", '400 - 5000 ha', '5000 - 20000 ha', '> 20000 ha'))) %>%
  ggplot(aes(x =  discovery_year)) +
  geom_bar(stat = "identity", position = "identity", aes(y = pct_change, fill = colour)) +
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

grid.arrange(fpa_size_bu_p, fpa_size_bu_pctot, fpa_size_bu_doy, nrow = 1)
g <- arrangeGrob(fpa_size_bu_p, fpa_size_bu_pctot, fpa_size_bu_doy, nrow = 1)
ggsave(file = "results/figs/draft/bu_size_fpa.pdf", g,
       width = 10, height = 6, dpi=1200, scale = 4, units = "cm")


# Threatened across CONUS stratified by SIZE EXCLUDING FIRES < 1 ha---------------------------------------------

ssize_fpa_bu_yearly <- bu_cleaned %>%
  filter(fire_size_km2 >= 1) %>%
  group_by(size) %>%
  summarise(bu_total = sum(bu)) %>%
  ungroup() %>%
  na.omit()

ssize_fpa_bu_df <- bu_cleaned %>%
  filter(fire_size_km2 >= 1) %>%
  group_by(size, discovery_year) %>%
  summarise(f_cnt = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(bu)) %>%
  left_join(., ssize_fpa_bu_yearly, by = 'size') %>%
  mutate(pct_change = (bu - lag(bu))/lag(bu) * 100,
         pct_total = bu/sum_fpa_bu_yearly$bu_total * 100,
         home_per_fire = bu/f_cnt,
         home_per_areaburned = bu/burned_area,
         colour = ifelse(pct_change <= 0, "negative","positive")) %>%
  ungroup() %>%
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
  xlab("Year") + ylab("") +
  ggtitle('(b) Number of structures \nwithin all wildfires') +
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
  geom_bar(data = filter(doy_ssize, size == '< 10 ha'), aes(x =  discovery_doy, y = doy_total, colour = size, fill = size), stat = "identity") +
  geom_bar(data = filter(doy_ssize, size == '10 - 400 ha'), aes(x =  discovery_doy, y = doy_total, fill = size, colour = size), stat = "identity") +
  geom_bar(data = filter(doy_ssize, size == '400 - 5000 ha'), aes(x =  discovery_doy, y = doy_total, fill = size, colour = size), stat = "identity") +
  geom_bar(data = filter(doy_ssize, size == '5000 - 20000 ha'), aes(x =  discovery_doy, y = doy_total, fill = size, colour = size), stat = "identity") +
  geom_bar(data = filter(doy_ssize, size == '> 20000 ha'), aes(x =  discovery_doy, y = doy_total, fill = size, colour = size), stat = "identity") +
  theme_pub()  +
  scale_color_manual(values = c('#fec44f',"#fe9929", "#ec7014", "#cc4c02", "#8c2d04")) +
  scale_fill_manual(values =  c('#fec44f',"#fe9929", "#ec7014", "#cc4c02", "#8c2d04")) +
  xlab("Year") + ylab("Number of homes threatened per wildfire discovery day of year") +
  # ggtitle('(b) Number of structures \nwithin all wildfires') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')


fpa_ssize_bu_pctot <- ssize_fpa_bu_df %>%
  transform(size = factor(size, levels=c('< 10 ha', "10 - 400 ha", '400 - 5000 ha', '5000 - 20000 ha', '> 20000 ha'))) %>%
  ggplot(aes(x =  discovery_year)) +
  geom_bar(stat = "identity", position = "identity", aes(y = pct_change, fill = colour)) +
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

grid.arrange(fpa_ssize_bu_p, fpa_ssize_bu_pctot, fpa_ssize_bu_doy, nrow = 1)
g <- arrangeGrob(fpa_ssize_bu_p, fpa_ssize_bu_pctot, fpa_ssize_bu_doy, nrow = 1)
ggsave(file = "results/figs/draft/bu_smallex_size_fpa.pdf", g,
       width = 10, height = 6, dpi=1200, scale = 4, units = "cm")
# Threatened across REGIONS ---------------------------------------------

region_fpa_bu_yearly <- bu_cleaned %>%
  group_by(region) %>%
  summarise(bu_total = sum(bu)) %>%
  ungroup() %>%
  na.omit()

region_fpa_bu_df <- bu_cleaned %>%
  group_by(region, discovery_year) %>%
  summarise(f_cnt = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(bu)) %>%
  left_join(., region_fpa_bu_yearly, by = 'region') %>%
  mutate(pct_change = (bu - lag(bu))/lag(bu) * 100,
         pct_total = bu/sum_fpa_bu_yearly$bu_total * 100,
         home_per_fire = bu/f_cnt,
         home_per_areaburned = bu/burned_area,
         colour = ifelse(pct_change <= 0, "negative","positive")) %>%
  ungroup() %>%
  filter(!is.na(discovery_year)) %>%
  mutate(buff_zone = 0)

region_fpa_bu_p <- region_fpa_bu_df %>%
  transform(region = factor(region, levels=c("East", 'Central', 'West'))) %>%
  ggplot(aes(x = discovery_year, y = bu*0.000001, group = region, color = region, fill = region)) +
  geom_point() +
  geom_smooth(
    method = 'glm',
    #method.args = list(family = "poisson"),
    fullrange = TRUE,
    size = 1
  ) +
  theme_pub() +
  scale_color_manual(values = c('#b2182b',"#5ab4ac", "#8c510a")) +
  scale_fill_manual(values = c('#b2182b',"#5ab4ac", "#8c510a")) +
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
  group_by(discovery_doy, region) %>%
  summarise(doy_total = sum(bu)) %>%
  ungroup()

fpa_region_bu_doy <- ggplot() +
  geom_bar(data = filter(doy_region, region == 'East'), aes(x =  discovery_doy, y = doy_total, colour = region, fill = region), stat = "identity") +
  geom_bar(data = filter(doy_region, region == 'West'), aes(x =  discovery_doy, y = doy_total, fill = region, colour = region), stat = "identity") +
  geom_bar(data = filter(doy_region, region == 'Central'), aes(x =  discovery_doy, y = doy_total, fill = region, colour = region), stat = "identity") +
  theme_pub()  +
  scale_color_manual(values = c('#b2182b',"#5ab4ac", "#8c510a")) +
  scale_fill_manual(values = c('#b2182b',"#5ab4ac", "#8c510a")) +
  xlab("Year") + ylab("Number of homes threatened per wildfire discovery day of year") +
  # ggtitle('(b) Number of structures \nwithin all wildfires') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')


fpa_region_bu_pctot <- region_fpa_bu_df %>%
  transform(region = factor(region, levels=c("East", 'Central', 'West'))) %>%
  ggplot(aes(x =  discovery_year)) +
  geom_bar(stat = "identity", position = "identity", aes(y = pct_change, fill = colour)) +
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
  facet_wrap(~ region, ncol = 1)

grid.arrange(region_fpa_bu_p, fpa_region_bu_pctot, fpa_region_bu_doy, nrow = 1)
g <- arrangeGrob(region_fpa_bu_p, fpa_region_bu_pctot, fpa_region_bu_doy, nrow = 1)
ggsave(file = "results/figs/draft/bu_region_fpa.pdf", g,
       width = 10, height = 6, dpi=1200, scale = 4, units = "cm")


# Threatened across REGION stratified by CAUSE ---------------------------------------------

fpa_cause_region_bu_df <- sum_fpa_bu %>%
  gather(variable, bu, -ID_sp, -fpa_id) %>%
  separate(variable,
           into = c("statistic", 'tmp', "year"),
           sep = "_") %>%
  dplyr::select(-tmp) %>%
  left_join(., as.data.frame(fpa_wui) %>%
              setNames(tolower(names(.))),
            by = "fpa_id") %>%
  group_by(ignition, region, bidecadal) %>%
  summarise(f_cnt = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(bu)) %>%
  left_join(., region_bu_df, by = c('bidecadal', 'region')) %>%
  mutate(pct_change = (bu - lag(bu))/lag(bu) * 100,
         pct_total = bu/bu_total * 100) %>%
  ungroup() %>%
  filter(!is.na(bidecadal)) %>%
  mutate(buff_zone = 0)

fpa_cause_region_bu_p <- fpa_cause_region_bu_df %>%
  transform(region = factor(region, levels=c("East", 'Central', 'West'))) %>%
  ggplot(aes(x =  bidecadal, group = ignition, color = ignition)) +
  geom_point(aes( y = bu*0.000001)) +
  geom_line(aes( y = bu*0.000001)) +
  theme_pub() +
  scale_y_continuous(limits = c(0, 10)) +
  scale_color_manual(values = c("#D62728","#1F77B4")) +
  xlab("Year") + ylab("") +
  ggtitle('(b) Number of structures \nwithin all wildfires') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none') +
  facet_wrap(~region, ncol = 1)

fpa_cause_bu_pctot <- fpa_cause_region_bu_df %>%
  transform(region = factor(region, levels=c("East", 'Central', 'West'))) %>%
  ggplot(aes(x =  bidecadal, group = ignition, color = ignition)) +
  geom_point(aes( y = pct_total)) +
  geom_line(aes( y = pct_total)) +
  theme_pub() +
  scale_color_manual(values = c("#D62728","#1F77B4")) +
  xlab("Year") + ylab("% of total structures contained within wildfire") +
  ggtitle('(c) Proportion of structures contained \nwithin wildfire to total') +
  scale_y_continuous(limits = c(0, 20)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none') +
  facet_wrap(~region, ncol = 1)

grid.arrange(region_bu_p, fpa_cause_region_bu_p, fpa_cause_bu_pctot, nrow = 1)
g <- arrangeGrob(region_bu_p, fpa_cause_region_bu_p, fpa_cause_bu_pctot, nrow = 1)
ggsave(file = "results/figs/draft/bu_cause_region_fpa.pdf", g,
       width = 10, height = 8, dpi=1200, scale = 3, units = "cm")

# Threatened across REGION stratified by CLASS ---------------------------------------------

fpa_region_class_bu_df <- sum_fpa_bu %>%
  gather(variable, bu, -ID_sp, -fpa_id) %>%
  separate(variable,
           into = c("statistic", 'tmp', "year"),
           sep = "_") %>%
  dplyr::select(-tmp) %>%
  left_join(., as.data.frame(fpa_wui) %>%
              setNames(tolower(names(.))),
            by = "fpa_id") %>%
  filter(class_coarse != 'Other') %>%
  group_by(class_coarse, region, bidecadal) %>%
  summarise(f_cnt = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(bu)) %>%
  left_join(., region_bu_df, by = c('bidecadal', 'region')) %>%
  mutate(pct_change = (bu - lag(bu))/lag(bu) * 100,
         pct_total = bu/bu_total * 100) %>%
  ungroup() %>%
  filter(!is.na(bidecadal)) %>%
  mutate(buff_zone = 0)

fpa_class_region_bu_p <- fpa_region_class_bu_df %>%
  transform(class_coarse = factor(class_coarse, levels=c("Urban", 'WUI', 'VLD', 'Wildlands'))) %>%
  transform(region = factor(region, levels=c("East", 'Central', 'West'))) %>%
  ggplot(aes(x =  bidecadal, group = class_coarse, color = class_coarse)) +
  geom_point(aes( y = bu*0.000001)) +
  geom_line(aes( y = bu*0.000001)) +
  theme_pub() +
  #scale_y_continuous(limits = c(0, 45)) +
  # scale_color_manual(values = c("#D62728","#1F77B4")) +
  xlab("Year") + ylab("") +
  ggtitle('(b) Number of structures \nwithin all wildfires') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none') +
  facet_wrap(~region , ncol = 1)

fpa_class_region_bu_pctot <- fpa_region_class_bu_df %>%
  transform(class_coarse = factor(class_coarse, levels=c("Urban", 'WUI', 'VLD', 'Wildlands'))) %>%
  transform(region = factor(region, levels=c("East", 'Central', 'West'))) %>%
  ggplot(aes(x =  bidecadal, group = class_coarse, color = class_coarse)) +
  geom_point(aes( y = pct_total)) +
  geom_line(aes( y = pct_total)) +
  theme_pub() +
  # scale_color_manual(values = c("#D62728","#1F77B4")) +
  xlab("Year") + ylab("% of total structures contained within wildfire") +
  ggtitle('(c) Proportion of structures contained \nwithin wildfire to total') +
  scale_y_continuous(limits = c(0, 10)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none') +
  facet_wrap(~region , ncol = 1)

grid.arrange(region_bu_p, fpa_class_region_bu_p, fpa_class_region_bu_pctot, nrow = 1)
g <- arrangeGrob(region_bu_p, fpa_class_region_bu_p, fpa_class_region_bu_pctot, nrow = 1)
ggsave(file = "results/figs/draft/bu_class_region_fpa.pdf", g,
       width = 10, height = 8, dpi=1200, scale = 3, units = "cm")


# Threatened across REGION stratified by SIZE ---------------------------------------------

fpa_region_size_bu_df <- sum_fpa_bu %>%
  gather(variable, bu, -ID_sp, -fpa_id) %>%
  separate(variable,
           into = c("statistic", 'tmp', "year"),
           sep = "_") %>%
  dplyr::select(-tmp) %>%
  left_join(., as.data.frame(fpa_wui) %>%
              setNames(tolower(names(.))),
            by = "fpa_id") %>%
  group_by(region, size, bidecadal) %>%
  summarise(f_cnt = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(bu)) %>%
  left_join(., region_bu_df, by = c('bidecadal', 'region')) %>%
  mutate(pct_change = (bu - lag(bu))/lag(bu) * 100,
         pct_total = bu/bu_total * 100,
         home_per_fire = bu/f_cnt,
         home_per_areaburned = bu/burned_area) %>%
  ungroup() %>%
  filter(!is.na(bidecadal)) %>%
  mutate(buff_zone = 0)


fpa_region_size_bu_p <- fpa_region_size_bu_df %>%
  transform(region = factor(region, levels=c("East", 'Central', 'West'))) %>%
  transform(size = factor(size, levels=c('< 10 ha', "10 - 400 ha", '400 - 5000 ha', '5000 - 20000 ha', '> 20000 ha'))) %>%
  ggplot(aes(x =  bidecadal, group = size, color = size)) +
  geom_point(aes( y = bu*0.000001)) +
  geom_line(aes( y = bu*0.000001)) +
  theme_pub() +
  scale_y_continuous(limits = c(0, 10)) +
  # scale_color_manual(values = c("#D62728","#1F77B4")) +
  xlab("Year") + ylab("") +
  ggtitle('(b) Number of structures \nwithin all wildfires') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none') +
  facet_wrap(~ region, ncol = 1)

fpa_region_size_bu_pctot <- fpa_region_size_bu_df %>%
  transform(region = factor(region, levels=c("East", 'Central', 'West'))) %>%
  transform(size = factor(size, levels=c('< 10 ha', "10 - 400 ha", '400 - 5000 ha', '5000 - 20000 ha', '> 20000 ha'))) %>%
  ggplot(aes(x =  bidecadal, group = size, color = size)) +
  geom_point(aes( y = pct_total)) +
  geom_line(aes( y = pct_total)) +
  theme_pub() +
  # scale_color_manual(values = c("#D62728","#1F77B4")) +
  xlab("Year") + ylab("% of total structures contained within wildfire") +
  ggtitle('(c) Proportion of structures contained \nwithin wildfire to total') +
  scale_y_continuous(limits = c(0, 20)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none') +
  facet_wrap(~ region, ncol = 1)

grid.arrange(region_bu_p, fpa_region_size_bu_p, fpa_region_size_bu_pctot, nrow = 1)
g <- arrangeGrob(region_bu_p, fpa_region_size_bu_p, fpa_region_size_bu_pctot, nrow = 1)
ggsave(file = "results/figs/draft/bu_region_size_fpa.pdf", g,
       width = 10, height = 5, dpi=1200, scale = 3, units = "cm")
