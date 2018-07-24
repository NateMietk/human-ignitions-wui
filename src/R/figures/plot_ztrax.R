
#
p1 <- bu_wui_cleaned %>%
  filter(year != 0) %>%
  filter(built_class != 'Non-Residential') %>%
  # mutate(built_class = ifelse(built_class == 'Residential', 'Ztrax', built_class)) %>%
  # filter(built_class == 'SILVIS') %>%
  group_by(year, built_class) %>%
  summarise(build_up_count_no_zero = sum(build_up_count_no_zero),
            build_up_count = sum(build_up_count)) %>%
  mutate(build_up_count_no_zero = ifelse(build_up_count_no_zero == 0, build_up_count, build_up_count_no_zero)) %>%
  ggplot(aes(x = year)) +
  geom_bar(aes(y = build_up_count_no_zero/1000000, fill = built_class), stat = 'identity', position = position_dodge(width = 2.5)) +
  xlab('') + ylab('Threatened homes (in millions)') +
  ggtitle('Ztrax with 0 built year removed') +
  theme_pub()

p2 <- bu_wui_cleaned %>%
  filter(year != 0) %>%
  filter(built_class != 'Non-Residential') %>%
  group_by(year, built_class) %>%
  summarise(build_up_count = sum(build_up_count)) %>%
  ggplot(aes(x = year)) +
  geom_bar(aes(y = build_up_count/1000000, fill = built_class), stat = 'identity', position = position_dodge(width = 2.5)) +
  xlab('Discovery year') + ylab('Threatened homes (in millions)') +
  ggtitle('Ztrax with 0 built year assumed to be prior to 1990') +
  theme_pub()

grid.arrange(p1, p2)

#
bu_complete_long_no_zero <- as_tibble(as.data.frame(bu_complete_cleaned)) %>%
  dplyr::select(fpa_id, class, ignition, discovery_year, built_class, 
                build_up_count_no_zero_0, build_up_count_no_zero_250, build_up_count_no_zero_500, 
                build_up_count_no_zero_1000) %>%
  gather(key = 'buffer_class', value = 'built_count', -fpa_id, -class, -ignition, -discovery_year, -built_class) %>%
  mutate(buffer_class = case_when(
    buffer_class == 'build_up_count_no_zero_0' ~ 'Fire perimeter',
    buffer_class == 'build_up_count_no_zero_250' ~ 'Buffer zone - 250m',
    buffer_class == 'build_up_count_no_zero_500' ~ 'Buffer zone - 500m',
    buffer_class == 'build_up_count_no_zero_1000' ~ 'Buffer zone - 1000m',
    TRUE ~ NA_character_))

bu_complete_long_no_zero %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(buffer_class = factor(buffer_class, levels=c('Fire perimeter', 'Buffer zone - 250m', 
                                                         'Buffer zone - 500m', 'Buffer zone - 1000m'))) %>%
  filter(built_class != 'No Structures') %>%
  group_by(discovery_year, buffer_class) %>%
  summarise(built_count = sum(built_count)) %>%
  ggplot(aes(x = discovery_year, y = built_count/1000000)) +
  geom_bar(stat = 'identity') +
  xlab('Discovery year') + ylab('Threatened homes (in millions)') +
  theme_pub() +
  theme(legend.position = 'none') +
  facet_wrap(~ buffer_class, nrow = 1)

#
bu_complete_long_no_zero %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  transform(buffer_class = factor(buffer_class, levels=c('Buffer zone - 1000m', 'Buffer zone - 500m',
                                                         'Buffer zone - 250m', 'Fire perimeter'))) %>%
  filter(built_class != 'No Structures') %>%
  group_by(class, buffer_class) %>%
  summarise(built_count = sum(built_count)) %>%
  ggplot(aes(x = class, y = built_count/1000000, fill = buffer_class)) +
  geom_bar(stat = 'identity', position='stack') +
  scale_fill_manual(values = c("Fire perimeter" = "#D62728", 
                               "Buffer zone - 250m" = "#FF7F0E", 
                               "Buffer zone - 500m" =  "#2CA02C", 
                               "Buffer zone - 1000m" = "#1F77B4"), 
                    name="Threatened homes") +
  xlab('Discovery year') + ylab('Threatened homes (in millions)') +
  theme_pub() +
  theme(legend.position = 'none')

#
bu_complete_long_no_zero %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  transform(buffer_class = factor(buffer_class, levels=c('Buffer zone - 1000m', 'Buffer zone - 500m',
                                                         'Buffer zone - 250m', 'Fire perimeter'))) %>%
  filter(built_class != 'No Structures') %>%
  group_by(ignition, buffer_class) %>%
  summarise(built_count = sum(built_count)) %>%
  ggplot(aes(x = ignition, y = built_count/1000000, fill = buffer_class)) +
  geom_bar(stat = 'identity', position='stack') +
  scale_fill_manual(values = c("Fire perimeter" = "#D62728", 
                               "Buffer zone - 250m" = "#FF7F0E", 
                               "Buffer zone - 500m" =  "#2CA02C", 
                               "Buffer zone - 1000m" = "#1F77B4"), 
                    name="Threatened homes") +
  xlab('Discovery year') + ylab('Threatened homes (in millions)') +
  theme_pub() +
  theme(legend.position = 'none')
#
bu_complete_long_no_zero %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  transform(buffer_class = factor(buffer_class, levels=c('Buffer zone - 1000m', 'Buffer zone - 500m',
                                                         'Buffer zone - 250m', 'Fire perimeter'))) %>%
  filter(built_class == 'Residential') %>%
  group_by(discovery_year, buffer_class) %>%
  summarise(built_count = sum(built_count)) %>%
  ggplot(aes(x = discovery_year, y = built_count/1000000, fill = buffer_class)) +
  geom_bar(stat = 'identity', position='stack') +
  scale_fill_manual(values = c("Fire perimeter" = "#D62728", 
                               "Buffer zone - 250m" = "#FF7F0E", 
                               "Buffer zone - 500m" =  "#2CA02C", 
                               "Buffer zone - 1000m" = "#1F77B4"), 
                    name="Threatened homes") +
  xlab('Discovery year') + ylab('Threatened homes (in millions)') +
  theme_pub() +
  theme(legend.position = 'none')
#

bu_complete_long_no_zero %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  transform(buffer_class = factor(buffer_class, levels=c('Buffer zone - 1000m', 'Buffer zone - 500m',
                                                         'Buffer zone - 250m', 'Fire perimeter'))) %>%
  filter(built_class == 'Residential') %>%
  group_by(class, discovery_year, buffer_class) %>%
  summarise(built_count = sum(built_count)) %>%
  ggplot(aes(x = discovery_year, y = built_count/1000000, fill = buffer_class)) +
  geom_bar(stat = 'identity', position='stack') +
  scale_fill_manual(values = c("Fire perimeter" = "#D62728", 
                               "Buffer zone - 250m" = "#FF7F0E", 
                               "Buffer zone - 500m" =  "#2CA02C", 
                               "Buffer zone - 1000m" = "#1F77B4"), 
                    name="Threatened homes") +
  xlab('Discovery year') + ylab('Threatened homes (in millions)') +
  theme_pub() +
  theme(legend.position = 'none') +
  facet_wrap(~class, nrow = 2)
#

mtbs_fitted_0 <- bu_complete_long_no_zero %>%
  filter(buffer_class == 'Fire perimeter') %>%
  filter(built_class == 'Residential') %>%
  droplevels() %>%
  group_by(discovery_year) %>%
  summarise(sum_built_count = sum(built_count)) %>%
  mblm(sum_built_count ~ discovery_year, data = ., repeated = FALSE)
pct_increase_0 <- (max(mtbs_fitted_0$fitted.values)/min(mtbs_fitted_0$fitted.values))*100

mtbs_fitted_250 <- bu_complete_long_no_zero %>%
  filter(buffer_class == "Buffer zone - 250m") %>%
  filter(built_class == 'Residential') %>%
  droplevels() %>%
  group_by(discovery_year) %>%
  summarise(sum_built_count = sum(built_count)) %>%
  mblm(sum_built_count ~ discovery_year, data = ., repeated = FALSE)
pct_increase_250 <- (max(mtbs_fitted_250$fitted.values)/min(mtbs_fitted_250$fitted.values))*100

mtbs_fitted_500 <- bu_complete_long_no_zero %>%
  filter(buffer_class == "Buffer zone - 500m") %>%
  filter(built_class == 'Residential') %>%
  droplevels() %>%
  group_by(discovery_year) %>%
  summarise(sum_built_count = sum(built_count)) %>%
  mblm(sum_built_count ~ discovery_year, data = ., repeated = FALSE)
pct_increase_500 <- (max(mtbs_fitted_500$fitted.values)/min(mtbs_fitted_500$fitted.values))*100

mtbs_fitted_1000 <- bu_complete_long_no_zero %>%
  filter(buffer_class == "Buffer zone - 1000m") %>%
  filter(built_class == 'Residential') %>%
  droplevels() %>%
  group_by(discovery_year) %>%
  summarise(sum_built_count = sum(built_count)) %>%
  mblm(sum_built_count ~ discovery_year, data = ., repeated = FALSE)
pct_increase_1000 <- (max(mtbs_fitted_1000$fitted.values)/min(mtbs_fitted_1000$fitted.values))*100

bu_complete_long_no_zero %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  transform(buffer_class = factor(buffer_class, levels=c('Buffer zone - 1000m', 'Buffer zone - 500m',
                                                         'Buffer zone - 250m', 'Fire perimeter'))) %>%
  filter(built_class == 'Residential') %>%
  group_by(discovery_year, buffer_class) %>%
  summarise(built_count = sum(built_count)) %>%
  ggplot(aes(x = discovery_year, y = built_count, fill = buffer_class)) +
  geom_bar(stat= 'identity', position = 'stack', alpha = 0.3) +
  scale_fill_manual(values = c("Fire perimeter" = "#D62728", 
                               "Buffer zone - 250m" = "#FF7F0E", 
                               "Buffer zone - 500m" =  "#2CA02C", 
                               "Buffer zone - 1000m" = "#1F77B4"), 
                    name="Threatened homes") +
  geom_abline(intercept = coef(mtbs_fitted_0)[1], 
              slope = coef(mtbs_fitted_0)[2], 
              col = '#D62728', linetype = "dashed", size = 1.25) +
  geom_abline(intercept = coef(mtbs_fitted_250)[1], 
              slope = coef(mtbs_fitted_250)[2], 
              col = '#FF7F0E', linetype = "dashed", size = 1.25) +
  geom_abline(intercept = coef(mtbs_fitted_500)[1], 
              slope = coef(mtbs_fitted_500)[2], 
              col = '#2CA02C', linetype = "dashed", size = 1.25) +
  geom_abline(intercept = coef(mtbs_fitted_1000)[1], 
              slope = coef(mtbs_fitted_1000)[2], 
              col = '#1F77B4', linetype = "dashed", size = 1.25) +
  geom_text(aes(label=paste('Within perimeter % change = ', round(pct_increase_0[1],1), '%'),
                x = 1997, y = 12), col = '#D62728', size = 4, inherit.aes = FALSE) +
  geom_text(aes(label=paste('Within 250m buffer % change = ', round(pct_increase_250[1],1), '%'),
                x = 1997.35, y = 11.5), col = '#FF7F0E', size = 4, inherit.aes = FALSE) +
  geom_text(aes(label=paste('Within 500m buffer % change = ', round(pct_increase_500[1],1), '%'),
                x = 1997.35, y = 11), col = '#2CA02C', size = 4, inherit.aes = FALSE) +
  geom_text(aes(label=paste('Within 1000m buffer % change = ', round(pct_increase_1000[1],1), '%'),
                x = 1997.5, y = 10.5), col = '#1F77B4', size = 4, inherit.aes = FALSE) +
  # scale_y_continuous(limits = c(0, 12)) +
  scale_x_continuous(limits = c(1992, 2015)) +
  xlab('Discovery year') + ylab('Threatened homes (in millions)') +
  ggtitle('TheilSen trend estimates') +
  theme_pub() +
  theme(legend.position = 'none')

p3 <- bu_complete_long_no_zero %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  transform(buffer_class = factor(buffer_class, levels=c('Buffer zone - 1000m', 'Buffer zone - 500m',
                                                         'Buffer zone - 250m', 'Fire perimeter'))) %>%
  filter(built_class == 'Residential') %>%
  group_by(discovery_year, buffer_class) %>%
  summarise(built_count = sum(built_count)/1000000) %>%
  ggplot(aes(x = discovery_year, y = built_count, fill = buffer_class)) +
  geom_bar(stat= 'identity', position = 'stack', alpha = 0.3) +
  geom_smooth(
    method = 'glm'
    # method.args = list(family = "poisson")
    ) +
  scale_fill_manual(values = c("Fire perimeter" = "#D62728", 
                               "Buffer zone - 250m" = "#FF7F0E", 
                               "Buffer zone - 500m" =  "#2CA02C", 
                               "Buffer zone - 1000m" = "#1F77B4"), 
                    name="Threatened homes") +
  theme_pub() +
  theme(legend.position = 'none')

  
ggplot_build(p3)$data[[2]] %>%
  group_by(group) %>%
  summarise(ymin = min(y),
            ymax = max(y)) %>%
  mutate(pct_change = (ymax/ymin) *100)


class_fpa_bu_yearly <- bu_complete_long_no_zero %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  filter(built_class == 'Residential') %>%
  droplevels() %>%
  group_by(class) %>%
  summarise(bu_total = sum(built_count)) %>%
  ungroup() %>%
  na.omit()

lt_class_fpa_bu_yearly <- bu_complete_long_no_zero %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  filter(built_class == 'Residential') %>%
  group_by(discovery_year, class) %>%
  summarise(bu_total = sum(built_count)) %>%
  ungroup() %>%
  group_by(class) %>%
  summarise(lt_mean = mean(bu_total)) %>%
  ungroup() %>%
  na.omit()

class_fpa_bu_df <- bu_complete_long_no_zero %>%
  filter(!(class %in% c("High Urban", "Med Urban", "Low Urban", 'Other'))) %>%
  filter(built_class == 'Residential') %>%
  group_by(class, discovery_year) %>%
  summarise(f_cnt = n(),
            bu = sum(built_count)) %>%
  left_join(., class_fpa_bu_yearly, by = 'class') %>%
  left_join(., lt_class_fpa_bu_yearly, by = 'class') %>%
  mutate(pct_change = (bu - lag(bu))/lag(bu) * 100,
         home_per_fire = bu/f_cnt,
         anomalies = bu - lt_mean,
         colour = ifelse(pct_change <= 0, "negative","positive"), 
         colourlt = ifelse(anomalies <= 0, "negative","positive")) %>%  ungroup() %>%
  filter(!is.na(discovery_year)) 

class_fpa_bu_df %>%
  transform(class = factor(class, levels=c('Intermix WUI', 'Interface WUI', 'VLD', 'Wildlands'))) %>%
  ggplot(aes(x =  discovery_year)) +
  geom_bar(stat = "identity", position = "identity", aes(y = anomalies*0.000001, fill = colourlt)) +
  scale_fill_manual(values = c(positive = "firebrick1",
                               negative = "steelblue")) +
  theme_pub() +
  xlab("Year") + ylab("Homes threatened by wildfire anomalies (in 100,000 units)") +
  theme(axis.title = element_text(face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = 'none') +
  facet_wrap(~ class, ncol = 1)
