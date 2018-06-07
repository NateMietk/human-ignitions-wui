
# FPA database from 1992-2015
# Overall totals CLASS AND CAUSE AND FISHID25K
fpa_wui_class_cause_fishnet25k <- fpa_wui %>%
  as.data.frame(.) %>%
  group_by(fishid25k, ignition, class) %>%
  summarise(fire_freq = n())

# Overall totals
totals_fpa <- fpa_wui %>%
  as.data.frame(.) %>%
  group_by() %>%
  summarise(totfirefreq = n(),
            totfirearea = sum(fire_size_km2),
            totseasonlength = IQR(discovery_doy),
            totfire_mean = mean(fire_size_km2),
            totfire_sd = sd(fire_size_km2)) %>%
  mutate(totfiresize_se = totfire_sd / sqrt(totfirefreq),
         lower_ci_totfiresize = totfire_mean - qt(1 - (0.05 / 2), totfirefreq - 1) * totfiresize_se,
         upper_ci_totfiresize = totfire_mean + qt(1 - (0.05 / 2), totfirefreq - 1) * totfiresize_se)

# Overall totals by CLASS
totals_by_class <- fpa_wui %>%
  as.data.frame(.) %>%
  group_by(class_coarse) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            seasonlength = IQR(discovery_doy),
            firesize_mean = mean(fire_size_km2),
            firesize_sd = sd(fire_size_km2)) %>%
  mutate(firesize_se = firesize_sd / sqrt(firefreq),
         lower_ci_firesize = firesize_mean - qt(1 - (0.05 / 2), firefreq - 1) * firesize_se,
         upper_ci_firesize = firesize_mean + qt(1 - (0.05 / 2), firefreq - 1) * firesize_se,
         pct_firefreq = (firefreq/totals_fpa$totfirefreq)*100,
         pct_firearea = (firearea/totals_fpa$totfirearea)*100,
         pct_allfreq = (firefreq/totals_fpa$totfirefreq)*100)

totals_by_class_slim <- totals_by_class %>%
  mutate(totfirefreq = firefreq,
         totfirearea = firearea,
         totseasonlength = seasonlength) %>%
  dplyr::select(class_coarse, totfirefreq, totfirearea, totseasonlength)

# Overall totals by CAUSE
totals_by_cause <- fpa_wui %>%
  as.data.frame(.) %>%
  group_by(ignition) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            seasonlength = IQR(discovery_doy),
            firesize_mean = mean(fire_size_km2),
            firesize_sd = sd(fire_size_km2)) %>%
  mutate(firesize_se = firesize_sd / sqrt(firefreq),
         lower_ci_firesize = firesize_mean - qt(1 - (0.05 / 2), firefreq - 1) * firesize_se,
         upper_ci_firesize = firesize_mean + qt(1 - (0.05 / 2), firefreq - 1) * firesize_se,
         pct_firefreq = (firefreq/totals_fpa$totfirefreq)*100,
         pct_firearea = (firearea/totals_fpa$totfirearea)*100,
         pct_allfreq = (firefreq/totals_fpa$totfirefreq)*100)

totals_by_cause_slim <- totals_by_cause %>%
  mutate(totfirefreq = firefreq,
         totfirearea = firearea,
         totseasonlength = seasonlength) %>%
  dplyr::select(ignition, totfirefreq, totfirearea, totseasonlength)

# Overall totals by CAUSE AND CLASS
totals_by_cause_class <- fpa_wui %>%
  as.data.frame(.) %>%
  group_by(class_coarse, ignition) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            seasonlength = IQR(discovery_doy),
            firesize_mean = mean(fire_size_km2),
            firesize_sd = sd(fire_size_km2)) %>%
  left_join(., totals_by_class_slim, by = 'class_coarse') %>%
  mutate(firesize_se = firesize_sd / sqrt(firefreq),
         lower_ci_firesize = firesize_mean - qt(1 - (0.05 / 2), firefreq - 1) * firesize_se,
         upper_ci_firesize = firesize_mean + qt(1 - (0.05 / 2), firefreq - 1) * firesize_se,
         pct_firefreq = (firefreq/totfirefreq)*100,
         pct_firearea = (firearea/totfirearea)*100,
         pct_allfreq = (firefreq/totfirefreq)*100)

totals_by_cause_class_slim <- totals_by_cause_class %>%
  mutate(totfirefreq = firefreq,
         totfirearea = firearea,
         totseasonlength = seasonlength) %>%
  dplyr::select(class, ignition, totfirefreq, totfirearea, totseasonlength)

# Overall totals by CAUSE AND YEAR
totals_by_year <- fpa_wui %>%
  as.data.frame(.) %>%
  group_by(discovery_year) %>%
  summarise(totfirefreq = n(),
            totfirearea = sum(fire_size_km2),
            totseasonlength = IQR(discovery_doy))

totals_by_cause_year <- fpa_wui %>%
  as.data.frame(.) %>%
  group_by(discovery_year, ignition) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            seasonlength = IQR(discovery_doy),
            firesize_mean = mean(fire_size_km2),
            firesize_sd = sd(fire_size_km2), 
            doy_median = median(discovery_doy)) %>%
  left_join(., totals_by_year, by = "discovery_year") %>%
  mutate(firesize_se = firesize_sd / sqrt(firefreq),
         lower_ci_firesize = firesize_mean - qt(1 - (0.05 / 2), firefreq - 1) * firesize_se,
         upper_ci_firesize = firesize_mean + qt(1 - (0.05 / 2), firefreq - 1) * firesize_se,
         pct_firefreq = (firefreq/totfirefreq)*100,
         pct_firearea = (firearea/totfirearea)*100)

totals_by_cause_year %>%
  ggplot(aes(x = discovery_year, y = firesize_mean, color = ignition)) +
  geom_line() +
  geom_ribbon(data=totals_by_cause_year, 
              aes(x = discovery_year, y = firesize_mean, 
                  ymin = lower_ci_firesize, ymax = upper_ci_firesize, 
                  group = ignition), 
              alpha = 0.2) 

# Overall totals by CLASS AND YEAR
totals_by_class_year <- fpa_wui %>%
  as.data.frame(.) %>%
  group_by(discovery_year, class) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            seasonlength = IQR(discovery_doy),
            firesize_mean = mean(fire_size_km2),
            firesize_sd = sd(fire_size_km2), 
            doy_median = median(discovery_doy)) %>%
  left_join(., totals_by_year, by = "discovery_year") %>%
  mutate(firesize_se = firesize_sd / sqrt(firefreq),
         lower_ci_firesize = firesize_mean - qt(1 - (0.05 / 2), firefreq - 1) * firesize_se,
         upper_ci_firesize = firesize_mean + qt(1 - (0.05 / 2), firefreq - 1) * firesize_se,
         pct_firefreq = (firefreq/totfirefreq)*100,
         pct_firearea = (firearea/totfirearea)*100)

totals_by_class_year %>%
  ggplot(aes(x = discovery_year, y = firesize_mean, color = class)) +
  geom_line() +
  geom_ribbon(data = totals_by_class_year, 
              aes(x = discovery_year, y = firesize_mean, 
                  ymin = lower_ci_firesize, ymax = upper_ci_firesize, 
                  group = class), 
              alpha = 0.2) 

# Overall totals by CAUSE AND CLASS AND YEAR
totals_by_cause_class_year <- fpa_wui %>%
  as.data.frame(.) %>%
  group_by(discovery_year, class, ignition) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            seasonlength = IQR(discovery_doy),
            firesize_mean = mean(fire_size_km2),
            firesize_sd = sd(fire_size_km2), 
            doy_median = median(discovery_doy)) %>%
  left_join(., totals_by_year, by = "discovery_year") %>%
  mutate(firesize_se = firesize_sd / sqrt(firefreq),
         lower_ci_firesize = firesize_mean - qt(1 - (0.05 / 2), firefreq - 1) * firesize_se,
         upper_ci_firesize = firesize_mean + qt(1 - (0.05 / 2), firefreq - 1) * firesize_se,
         pct_firefreq = (firefreq/totfirefreq)*100,
         pct_firearea = (firearea/totfirearea)*100)

# Overall totals by SEASON
totals_fpa_seasonal <- fpa_wui %>%
  as.data.frame(.) %>%
  group_by(seasons) %>%
  summarise(totfirefreq = n(),
            totfirearea = sum(fire_size_km2),
            totseasonlength = IQR(discovery_doy))

# Overall totals by SEASON AND CLASS
totals_fpa_class_seasonal <- fpa_wui %>%
  as.data.frame(.) %>%
  group_by(seasons, class) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            seasonlength = IQR(discovery_doy),
            firesize_mean = mean(fire_size_km2),
            firesize_sd = sd(fire_size_km2), 
            doy_median = median(discovery_doy)) %>%
  left_join(., totals_fpa_seasonal, by = "seasons") %>%
  mutate(firesize_se = firesize_sd / sqrt(firefreq),
         lower_ci_firesize = firesize_mean - qt(1 - (0.05 / 2), firefreq - 1) * firesize_se,
         upper_ci_firesize = firesize_mean + qt(1 - (0.05 / 2), firefreq - 1) * firesize_se,
         pct_firefreq = (firefreq/totfirefreq)*100,
         pct_firearea = (firearea/totfirearea)*100)

# Overall totals by SEASON AND CAUSE
totals_fpa_cause_seasonal <- fpa_wui %>%
  as.data.frame(.) %>%
  group_by(seasons, ignition) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            seasonlength = IQR(discovery_doy),
            firesize_mean = mean(fire_size_km2),
            firesize_sd = sd(fire_size_km2), 
            doy_median = median(discovery_doy)) %>%
  left_join(., totals_fpa_seasonal, by = "seasons") %>%
  mutate(firesize_se = firesize_sd / sqrt(firefreq),
         lower_ci_firesize = firesize_mean - qt(1 - (0.05 / 2), firefreq - 1) * firesize_se,
         upper_ci_firesize = firesize_mean + qt(1 - (0.05 / 2), firefreq - 1) * firesize_se,
         pct_firefreq = (firefreq/totfirefreq)*100,
         pct_firearea = (firearea/totfirearea)*100)

# Overall totals by SEASON AND CAUSE and DISCOVERY YEAR
totals_fpa_cause_seasonal_year <- fpa_wui %>%
  as.data.frame(.) %>%
  group_by(discovery_year, seasons, ignition) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            seasonlength = IQR(discovery_doy),
            firesize_mean = mean(fire_size_km2),
            firesize_sd = sd(fire_size_km2), 
            doy_median = median(discovery_doy)) %>%
  mutate(firesize_se = firesize_sd / sqrt(firefreq),
         lower_ci_firesize = firesize_mean - qt(1 - (0.05 / 2), firefreq - 1) * firesize_se,
         upper_ci_firesize = firesize_mean + qt(1 - (0.05 / 2), firefreq - 1) * firesize_se)

totals_fpa_cause_seasonal_year %>%
  ggplot(aes(x = discovery_year, y = firesize_mean, color = ignition)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_ci_firesize, ymax = upper_ci_firesize, 
                  group = ignition), 
              alpha = 0.2) +
  facet_wrap(~ seasons)

# Overall totals by CAUSE AND CLASS AND SEASON
totals_by_cause_class_season <- fpa_wui %>%
  as.data.frame(.) %>%
  group_by(seasons, class, ignition) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            seasonlength = IQR(discovery_doy),
            firesize_mean = mean(fire_size_km2),
            firesize_sd = sd(fire_size_km2), 
            doy_median = median(discovery_doy)) %>%
  left_join(., totals_fpa_seasonal, by = "seasons") %>%
  mutate(firesize_se = firesize_sd / sqrt(firefreq),
         lower_ci_firesize = firesize_mean - qt(1 - (0.05 / 2), firefreq - 1) * firesize_se,
         upper_ci_firesize = firesize_mean + qt(1 - (0.05 / 2), firefreq - 1) * firesize_se,
         pct_firefreq = (firefreq/totfirefreq)*100,
         pct_firearea = (firearea/totfirearea)*100)

# Summarize FPA point data by CAUSE AND CLASS AND DISCOVERY DOY
# Will get at what is the most common discovery doy for human vs lightining ignitions
common_disc_doy <- fpa_wui %>%
  as.data.frame(.) %>%
  group_by(class, ignition, discovery_doy) %>%
  summarise(doyfreq = n())

# Overall totals by SIZE
totals_size <- fpa_wui %>%
  as.data.frame(.) %>%
  mutate(sizeclass = classify_fire_size_cl(fire_size_km2)) %>%
  transform(size = factor(size, levels=c('< 10 ha', "10 - 400 ha", '400 - 5000 ha', '5000 - 20000 ha', '> 20000 ha'))) %>%
  group_by(sizeclass) %>%
  summarise(totfirefreq = n(),
            totfirearea = sum(fire_size_km2))

# Overall totals by SIZE AND CLASS
totals_size_class <- fpa_wui %>%
  as.data.frame(.) %>%
  mutate(sizeclass = classify_fire_size_cl(fire_size_km2)) %>%
  transform(size = factor(size, levels=c('< 10 ha', "10 - 400 ha", '400 - 5000 ha', '5000 - 20000 ha', '> 20000 ha'))) %>%
  group_by(sizeclass, class) %>%
  summarise(totfirefreq = n(),
            totfirearea = sum(fire_size_km2))

# Overall totals by SIZE AND CAUSE
totals_size_cause_slim <- fpa_wui %>%
  as.data.frame(.) %>%
  group_by(size, ignition) %>%
  summarise(totfirefreq = n(),
            totfirearea = sum(fire_size_km2))

totals_by_size_cause <- fpa_wui %>%
  as.data.frame(.) %>%
  group_by(size, ignition, class) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2)) %>%
  left_join(., totals_size_cause, by = c("size", "ignition")) %>%
  mutate(pct_firefreq = (firefreq/totfirefreq)*100,
         pct_firearea = (firearea/totfirearea)*100)

# Overall totals by SIZE AND CAUSE AND CLASS


# Overall totals by IGNITIONS AND CLASS AND REGION
 totals_by_cause_class_slim <- totals_by_cause_class %>%
   mutate(totfirefreq = firefreq,
          totfirearea = firearea ) %>%
   dplyr::select(class, ignition, totfirefreq, totfirearea)

totals_by_sizeclass <- fpa_wui %>%
  as.data.frame(.) %>%
  group_by(region, ignition, class) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            seasonlength = IQR(discovery_doy),
            firesize_mean = mean(fire_size_km2),
            firesize_sd = sd(fire_size_km2), 
            doy_median = median(discovery_doy)) %>%
  left_join(., totals_by_cause_class_slim, by = c("ignition", "class")) %>%
  mutate(firesize_se = firesize_sd / sqrt(firefreq),
         lower_ci_firesize = firesize_mean - qt(1 - (0.05 / 2), firefreq - 1) * firesize_se,
         upper_ci_firesize = firesize_mean + qt(1 - (0.05 / 2), firefreq - 1) * firesize_se,
         pct_firefreq = (firefreq/totfirefreq)*100,
         pct_firearea = (firearea/totfirearea)*100)

# Overall totals by IGNITIONS AND CLASS AND REGION AND SIZE
totals_by_size_cause_class_slim <- totals_by_size_cause_class %>%
  mutate(totfirefreq = firefreq,
         totfirearea = firearea ) %>%
  dplyr::select(class, ignition, totfirefreq, totfirearea)

fpa_wui %>%
  as.data.frame(.) %>%
  mutate(sizeclass = classify_fire_size_cl(fire_size_km2)) %>%
  transform(sizeclass = factor(sizeclass, levels=c("Small", "Large", "Very Large"))) %>%
  group_by(region, sizeclass, ignition, class) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            firesize_sd = sd(fire_size_km2),
            seasonlength = IQR(discovery_doy)) %>%
  left_join(., totals_by_size_cause_class, by = c("sizeclass", "ignition", "class")) %>%
  mutate(pct_firefreq = (firefreq/totfirefreq)*100,
         pct_firearea = (firearea/totfirearea)*100)
