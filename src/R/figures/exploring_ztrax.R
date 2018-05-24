
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

fpa_bu_df <- sum_fpa_bu %>%
  gather(variable, bu, -ID_sp, -fpa_id) %>%
  separate(variable,
           into = c("statistic", 'tmp', "year"),
           sep = "_") %>%
  dplyr::select(-tmp) %>%
  left_join(., as.data.frame(fpa_wui) %>%
              setNames(tolower(names(.))),
            by = "fpa_id") %>%
  group_by(bidecadal) %>%
  summarise(f_cnt = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(bu)) %>%
  left_join(., conus_bu_df, by = 'bidecadal') %>%
  mutate(pct_change = (bu - lag(bu))/lag(bu) * 100,
         pct_total = bu/bu_total * 100,
         home_per_fire = bu/f_cnt,
         home_per_areaburned = bu/burned_area) %>%
  ungroup() %>%
  filter(!is.na(bidecadal)) %>%
  mutate(buff_zone = 0)

fpa_bu_p <- fpa_bu_df %>%
  ggplot(aes(x =  bidecadal)) +
  geom_point(aes( y = bu*0.000001), color = 'red') +
  geom_line(aes( y = bu*0.000001), color = 'red') +
  theme_pub() +
  xlab("Year") + ylab("") +
  ggtitle('(b) Number of structures \nwithin all wildfires') +
  scale_y_continuous(limits = c(0, 45)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank())

fpa_bu_pctot <- fpa_bu_df %>%
  ggplot(aes(x =  bidecadal)) +
  geom_point(aes( y = pct_total), color = 'blue') +
  geom_line(aes( y = pct_total), color = 'blue') +
  theme_pub() +
  xlab("Year") + ylab("% of total structures contained within wildfire") +
  ggtitle('(c) Proportion of structures contained \nwithin wildfire to total') +
  scale_y_continuous(limits = c(10, 35)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank())

grid.arrange(conus_bu_p, fpa_bu_p, fpa_bu_pctot, nrow = 1)
g <- arrangeGrob(conus_bu_p, fpa_bu_p, fpa_bu_pctot, nrow = 1)
ggsave(file = "results/figs/draft/bu_fpa.pdf", g,
       width = 10, height = 5, dpi=1200, scale = 3, units = "cm")

# Threatened across CONUS stratified by CAUSE ---------------------------------------------

fpa_cause_bu_df <- sum_fpa_bu %>%
  gather(variable, bu, -ID_sp, -fpa_id) %>%
  separate(variable,
           into = c("statistic", 'tmp', "year"),
           sep = "_") %>%
  dplyr::select(-tmp) %>%
  left_join(., as.data.frame(fpa_wui) %>%
              setNames(tolower(names(.))),
            by = "fpa_id") %>%
  filter( fire_size_km2 >= 0.025) %>%
  group_by(ignition, bidecadal) %>%
  summarise(f_cnt = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(bu)) %>%
  left_join(., conus_bu_df, by = 'bidecadal') %>%
  mutate(pct_change = (bu - lag(bu))/lag(bu) * 100,
         pct_total = bu/bu_total * 100,
         home_per_fire = bu/f_cnt,
         home_per_areaburned = bu/burned_area) %>%
  ungroup() %>%
  filter(!is.na(bidecadal)) %>%
  mutate(buff_zone = 0)

fpa_cause_bu_p <- fpa_cause_bu_df %>%
  ggplot(aes(x =  bidecadal, group = ignition, color = ignition)) +
  geom_point(aes( y = bu*0.000001)) +
  geom_line(aes( y = bu*0.000001)) +
  theme_pub() +
  scale_color_manual(values = c("#D62728","#1F77B4")) +
  xlab("Year") + ylab("") +
  ggtitle('(a) Number of structures \nwithin all wildfires') +
  scale_y_continuous(limits = c(0, 1)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')

fpa_cause_bu_pctot <- fpa_cause_bu_df %>%
  ggplot(aes(x =  bidecadal, group = ignition, color = ignition)) +
  geom_point(aes( y = pct_total)) +
  geom_line(aes( y = pct_total)) +
  theme_pub() +
  scale_color_manual(values = c("#D62728","#1F77B4")) +
  xlab("Year") + ylab("% of total structures contained within wildfire") +
  ggtitle('(b) Proportion of structures contained \nwithin wildfire to total') +
  scale_y_continuous(limits = c(0, 35)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')

grid.arrange(conus_bu_p, fpa_cause_bu_p, fpa_cause_bu_pctot, nrow = 1)
g <- arrangeGrob(conus_bu_p, fpa_cause_bu_p, fpa_cause_bu_pctot, nrow = 1)
ggsave(file = "results/figs/draft/bu_cause_fpa.pdf", g,
       width = 10, height = 5, dpi=1200, scale = 3, units = "cm")


# Threatened across CONUS stratified by CLASS ---------------------------------------------

fpa_class_bu_df <- sum_fpa_bu %>%
  gather(variable, bu, -ID_sp, -fpa_id) %>%
  separate(variable,
           into = c("statistic", 'tmp', "year"),
           sep = "_") %>%
  dplyr::select(-tmp) %>%
  left_join(., as.data.frame(fpa_wui) %>%
              setNames(tolower(names(.))),
            by = "fpa_id") %>%
  filter(class_coarse != 'Other') %>%
  filter( fire_size_km2 >= 0.025) %>%
  group_by(class_coarse, bidecadal) %>%
  summarise(f_cnt = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(bu)) %>%
  left_join(., conus_bu_df, by = c('bidecadal')) %>%
  mutate(pct_change = (bu - lag(bu))/lag(bu) * 100,
         pct_total = bu/bu_total * 100,
         home_per_fire = bu/f_cnt,
         home_per_areaburned = bu/burned_area) %>%
  ungroup() %>%
  filter(!is.na(bidecadal)) %>%
  mutate(buff_zone = 0)

fpa_class_bu_p <- fpa_class_bu_df %>%
  transform(class_coarse = factor(class_coarse, levels=c("Urban", 'WUI', 'VLD', 'Wildlands'))) %>%
  ggplot(aes(x =  bidecadal, group = class_coarse, color = class_coarse)) +
  geom_point(aes( y = bu*0.000001)) +
  geom_line(aes( y = bu*0.000001)) +
  theme_pub() +
  scale_y_continuous(limits = c(0, 45)) +
  # scale_color_manual(values = c("#D62728","#1F77B4")) +
  xlab("Year") + ylab("") +
  ggtitle('(b) Number of structures \nwithin all wildfires') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')

fpa_class_bu_pctot <- fpa_class_bu_df %>%
  transform(class_coarse = factor(class_coarse, levels=c("Urban", 'WUI', 'VLD', 'Wildlands'))) %>%
  ggplot(aes(x =  bidecadal, group = class_coarse, color = class_coarse)) +
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
        legend.position = 'none')

grid.arrange(conus_bu_p, fpa_class_bu_p, fpa_class_bu_pctot, nrow = 1)
g <- arrangeGrob(conus_bu_p, fpa_class_bu_p, fpa_class_bu_pctot, nrow = 1)
ggsave(file = "results/figs/draft/bu_class_fpa.pdf", g,
       width = 10, height = 5, dpi=1200, scale = 3, units = "cm")


# Threatened across CONUS stratified by SIZE ---------------------------------------------

fpa_size_bu_df <- sum_fpa_bu %>%
  gather(variable, bu, -ID_sp, -fpa_id) %>%
  separate(variable,
           into = c("statistic", 'tmp', "year"),
           sep = "_") %>%
  dplyr::select(-tmp) %>%
  left_join(., as.data.frame(fpa_wui) %>%
              setNames(tolower(names(.))),
            by = "fpa_id") %>%
  group_by(size, bidecadal) %>%
  summarise(f_cnt = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(bu)) %>%
  left_join(., conus_bu_df, by = c('bidecadal')) %>%
  mutate(pct_change = (bu - lag(bu))/lag(bu) * 100,
         pct_total = bu/bu_total * 100,
         home_per_fire = bu/f_cnt,
         home_per_areaburned = bu/burned_area) %>%
  ungroup() %>%
  filter(!is.na(bidecadal)) %>%
  mutate(buff_zone = 0)


fpa_size_bu_p <- fpa_size_bu_df %>%
  transform(size = factor(size, levels=c('< 10 ha', "10 - 400 ha", '400 - 5000 ha', '5000 - 20000 ha', '> 20000 ha'))) %>%
  ggplot(aes(x =  bidecadal, group = size, color = size)) +
  geom_point(aes( y = bu*0.000001)) +
  geom_line(aes( y = bu*0.000001)) +
  theme_pub() +
  scale_y_continuous(limits = c(0, 40)) +
  # scale_color_manual(values = c("#D62728","#1F77B4")) +
  xlab("Year") + ylab("") +
  ggtitle('(b) Number of structures \nwithin all wildfires') +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')

fpa_size_bu_pctot <- fpa_size_bu_df %>%
  transform(size = factor(size, levels=c('< 10 ha', "10 - 400 ha", '400 - 5000 ha', '5000 - 20000 ha', '> 20000 ha'))) %>%
  ggplot(aes(x =  bidecadal, group = size, color = size)) +
  geom_point(aes( y = pct_total)) +
  geom_line(aes( y = pct_total)) +
  theme_pub() +
  # scale_color_manual(values = c("#D62728","#1F77B4")) +
  xlab("Year") + ylab("% of total structures contained within wildfire") +
  ggtitle('(c) Proportion of structures contained \nwithin wildfire to total') +
  scale_y_continuous(limits = c(0, 35)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none')

grid.arrange(conus_bu_p, fpa_size_bu_p, fpa_size_bu_pctot, nrow = 1)
g <- arrangeGrob(conus_bu_p, fpa_size_bu_p, fpa_size_bu_pctot, nrow = 1)
ggsave(file = "results/figs/draft/bu_size_fpa.pdf", g,
       width = 10, height = 5, dpi=1200, scale = 3, units = "cm")


# Threatened across REGIONS ---------------------------------------------

fpa_region_bu_df <- sum_fpa_bu %>%
  gather(variable, bu, -ID_sp, -fpa_id) %>%
  separate(variable,
           into = c("statistic", 'tmp', "year"),
           sep = "_") %>%
  dplyr::select(-tmp) %>%
  left_join(., as.data.frame(fpa_wui) %>%
              setNames(tolower(names(.))),
            by = "fpa_id") %>%
  filter(class_coarse != 'Other') %>%
  group_by(region, bidecadal) %>%
  summarise(f_cnt = n(),
            burned_area = sum(fire_size_km2),
            bu = sum(bu)) %>%
  left_join(., region_bu_df, by = c('bidecadal', 'region')) %>%
  mutate(pct_change = (bu - lag(bu))/lag(bu) * 100,
         pct_total = bu/bu_total * 100) %>%
  ungroup() %>%
  filter(!is.na(bidecadal)) %>%
  mutate(buff_zone = 0)

fpa_region_bu_p <- fpa_region_bu_df %>%
  transform(region = factor(region, levels=c("East", 'Central', 'West'))) %>%
  ggplot(aes(x =  bidecadal)) +
  geom_point(aes( y = bu*0.000001), color = 'red') +
  geom_line(aes( y = bu*0.000001), color = 'red') +
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

fpa_region_bu_pctot <- fpa_region_bu_df %>%
  transform(region = factor(region, levels=c("East", 'Central', 'West'))) %>%
  ggplot(aes(x =  bidecadal)) +
  geom_point(aes( y = pct_total), color = 'blue') +
  geom_line(aes( y = pct_total), color = 'blue') +
  theme_pub() +
  # scale_color_manual(values = c("#D62728","#1F77B4")) +
  xlab("Year") + ylab("% of total structures contained within wildfire") +
  ggtitle('(c) Proportion of structures contained \nwithin wildfire to total') +
  scale_y_continuous(limits = c(0, 15)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none') +
  facet_wrap(~region , ncol = 1)

grid.arrange(region_bu_p, fpa_region_bu_p, fpa_region_bu_pctot, nrow = 1)
g <- arrangeGrob(region_bu_p, fpa_region_bu_p, fpa_region_bu_pctot, nrow = 1)
ggsave(file = "results/figs/draft/bu_region_fpa.pdf", g,
       width = 10, height = 8, dpi=1200, scale = 3, units = "cm")

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
