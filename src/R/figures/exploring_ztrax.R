
conus_bu_df <- sum_ecoregions_bu %>%
  gather(variable, bu, -ID_sp, -us_l3name) %>%
  separate(variable,
           into = c("statistic", 'tmp', "year"),
           sep = "_") %>%
  dplyr::select(-tmp) %>%
  left_join(., as.data.frame(ecoreg), by = "us_l3name") %>%
  group_by(year) %>%
  summarise(bu = sum(bu))

conus_bu_p <- conus_bu_df %>%
  group_by(year) %>%
  summarise(bu = sum(bu)) %>%
  ggplot(aes(x =  year, y = bu*0.000001)) +
  geom_point() +
  geom_line(group = 1) +
  scale_color_manual(values = c("#D62728","#1F77B4")) +
  theme_pub()  +
  xlab("Year") + ylab("Building unit count (in 100,000 units)") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = "none")
ggsave(file = "results/figs/draft/bu_conus.pdf", conus_bu_p, 
       width = 4, height = 5, dpi=1200, scale = 3, units = "cm")

fpa <- as.data.frame(fpa_wui) %>%
  mutate(year = ifelse(DISCOVERY_YEAR >= 1991 & DISCOVERY_YEAR <= 1995, 1995,
                        ifelse(DISCOVERY_YEAR >= 1996 & DISCOVERY_YEAR <= 2000, 2000,
                               ifelse(DISCOVERY_YEAR >= 2001 & DISCOVERY_YEAR <= 2005, 2005,
                                      ifelse(DISCOVERY_YEAR >= 2006 & DISCOVERY_YEAR <= 2010, 2010,
                                             ifelse(DISCOVERY_YEAR >= 2011 & DISCOVERY_YEAR <= 2015, 2015,
                                                    DISCOVERY_YEAR )))))),
         pop_den = ifelse( DISCOVERY_YEAR >= 1992 | DISCOVERY_YEAR < 2000, POPDEN1990,
                          ifelse( DISCOVERY_YEAR >= 2000 | DISCOVERY_YEAR < 2009, POPDEN2000,
                                   ifelse( DISCOVERY_YEAR >= 2010 | DISCOVERY_YEAR < 2016, POPDEN2010, NA ))),
         house_den = ifelse( DISCOVERY_YEAR >= 1992 | DISCOVERY_YEAR < 2000, HUDEN1990,
                             ifelse( DISCOVERY_YEAR >= 2000 | DISCOVERY_YEAR < 2009, HUDEN2000,
                                     ifelse( DISCOVERY_YEAR >= 2010 | DISCOVERY_YEAR < 2016, HUDEN2010, NA ))))


fpa_bu_df <- sum_fpa_bu %>%
  gather(variable, bu, -ID_sp, -fpa_id) %>%
  separate(variable,
           into = c("statistic", 'tmp', "year"),
           sep = "_") %>%
  dplyr::select(-tmp) %>%
  left_join(., as.data.frame(fpa_wui) %>%
              setNames(tolower(names(.))), 
            by = "fpa_id") %>%
  mutate(buff_zone = 0) %>%
  group_by(class, ignition, year, buff_zone) %>%
  summarise(bu = sum(bu)) %>%
  ungroup() %>%
  na.omit()  

fpa_bu_1k_df <- sum_fpa_1k_bu %>%
  gather(variable, bu, -ID_sp, -fpa_id) %>%
  separate(variable,
           into = c("statistic", 'tmp', "year"),
           sep = "_") %>%
  dplyr::select(-tmp) %>%
  left_join(., as.data.frame(fpa_wui) %>%
              setNames(tolower(names(.))), 
            by = "fpa_id") %>%
  mutate(buff_zone = 1) %>%
  group_by(class, ignition, year, buff_zone) %>%
  summarise(bu = sum(bu)) %>%
  na.omit()

fpa_bu_2k_df <- sum_fpa_2k_bu %>%
  gather(variable, bu, -ID_sp, -fpa_id) %>%
  separate(variable,
           into = c("statistic", 'tmp', "year"),
           sep = "_") %>%
  dplyr::select(-tmp) %>%
  left_join(., as.data.frame(fpa_wui) %>%
              setNames(tolower(names(.))), 
            by = "fpa_id") %>%
  mutate(buff_zone = 2) %>%
  group_by(class, ignition, year, buff_zone) %>%
  summarise(bu = sum(bu)) %>%
  na.omit()

fpa_bu_3k_df <- sum_fpa_3k_bu %>%
  gather(variable, bu, -ID_sp, -fpa_id) %>%
  separate(variable,
           into = c("statistic", 'tmp', "year"),
           sep = "_") %>%
  dplyr::select(-tmp) %>%
  left_join(., as.data.frame(fpa_wui) %>%
              setNames(tolower(names(.))), 
            by = "fpa_id") %>%
  mutate(buff_zone = 3) %>%
  group_by(class, ignition, year, buff_zone) %>%
  summarise(bu = sum(bu)) %>%
  na.omit()
  
fpa_long <- fpa_bu_df %>%
  bind_cols(., fpa_bu_1k_df, fpa_bu_2k_df, fpa_bu_3k_df) %>%
  ungroup() %>%
  mutate(year = as.numeric(year),
         bu_c = bu,
         bu1_c = bu1 - bu,
         bu2_c = bu2 - bu1,
         bu3_c = bu3 - bu2) %>%
  dplyr::select(class, ignition, year, bu_c, bu1_c, bu2_c, bu3_c) 

fpa_cause_bu_p <- fpa_long %>%
  na.omit() %>%  
  ggplot(aes(x =  year, y = bu_c*0.000001, fill = ignition, color = ignition, group = ignition)) +
  geom_point() +
  geom_line() +
  theme_pub() +
  xlab("Year") + ylab("Building unit count (in 100,000 units)") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = "none") 

fpa_cause_bu_p <- fpa_long %>%
  na.omit() %>%  
  ggplot(aes(x =  year, y = bu_c*0.000001, fill = ignition, color = ignition, group = ignition)) +
  geom_point() +
  geom_line() +
  theme_pub() +
  xlab("Year") + ylab("Building unit count (in 100,000 units)") +
  scale_y_continuous(limits = c(0, 900)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = "none")

fpa_1k_cause_bu_p <- fpa_long %>%
  na.omit() %>%  
  ggplot(aes(x =  year, y = bu1_c*0.000001, fill = ignition, color = ignition, group = ignition)) +
  geom_point() +
  geom_line() +
  theme_pub() +
  xlab("Year") + ylab("Building unit count (in 100,000 units)") +
  scale_y_continuous(limits = c(0, 900)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = "none")

fpa_2k_cause_bu_p <- fpa_long %>%
  na.omit() %>%  
  ggplot(aes(x =  year, y = bu2_c*0.000001, fill = ignition, color = ignition, group = ignition)) +
  geom_point() +
  geom_line() +
  theme_pub() +
  xlab("Year") + ylab("Building unit count (in 100,000 units)") +
  scale_y_continuous(limits = c(0, 900)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = "none")

fpa_3k_cause_bu_p <- fpa_long %>%
  na.omit() %>%  
  ggplot(aes(x =  year, y = bu3_c*0.000001, fill = ignition, color = ignition, group = ignition)) +
  geom_point() +
  geom_line() +
  theme_pub() +
  xlab("Year") + ylab("Building unit count (in 100,000 units)") +
  scale_y_continuous(limits = c(0, 900)) +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = "none")

grid.arrange(fpa_cause_bu_p, fpa_1k_cause_bu_p, 
             fpa_2k_cause_bu_p, fpa_3k_cause_bu_p, ncol =4)
g <- arrangeGrob(fpa_cause_bu_p, fpa_1k_cause_bu_p, 
                 fpa_2k_cause_bu_p, fpa_3k_cause_bu_p, ncol = 4)
ggsave(file = "results/figs/draft/bu_fpa_buf03.pdf", g, 
       width = 9, height = 5, dpi=1200, scale = 3, units = "cm")

fpa_bu_p <- fpa_long %>%
  group_by(year) %>%
  summarise(bu_c = sum(bu_c)) %>%
  na.omit() %>%  
  ggplot(aes(x =  year, y = bu_c*0.000001)) +
  geom_point() +
  geom_line(group = 1) +
  theme_pub() +
  xlab("Year") + ylab("Building unit count (in 100,000 units)") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = "none") 
ggsave(file = "results/figs/draft/bu_fpa.pdf", fpa_bu_p, 
       width = 4, height = 5, dpi=1200, scale = 3, units = "cm")

fpa_class_bu_p <- fpa_long %>%
  filter(class != 'Other') %>%
  mutate(class =  ifelse( class == 'High Urban' | class == 'Med Urban' | class == 'Low Urban', 'Urban',
                          ifelse( class == 'Intermix WUI' | class == 'Interface WUI', 'WUI', as.character(class)))) %>%
  transform(class = factor(class, levels = c("Urban", "WUI", "VLD", 'Wildlands'))) %>% 
  group_by(year, class) %>%
  summarise(bu_c = sum(bu_c)) %>%
  na.omit() %>%  
  ggplot(aes(x =  year, y = bu_c*0.000001)) +
  geom_point() +
  geom_line(group = 1) +
  theme_pub() +
  xlab("Year") + ylab("Building unit count (in 100,000 units)") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = "none") +
  facet_wrap(~ class)
ggsave(file = "results/figs/draft/bu_fpa.pdf", fpa_bu_p, 
       width = 4, height = 5, dpi=1200, scale = 3, units = "cm")
