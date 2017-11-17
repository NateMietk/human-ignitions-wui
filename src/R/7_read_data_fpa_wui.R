source("src/functions/ggplot_theme.R")
source("src/functions/plot_theme.R")

# FPA database from 2001-2015
fpa_wui <- st_read(file.path(prefix, "fire/fpa-fod/fpa_wui_conus.gpkg")) %>%
  mutate(Seasons = classify_seasons(DISCOVERY_DOY),
         region = as.factor(if_else(NA_L1NAME %in% c("EASTERN TEMPERATE FORESTS",
                                                     "GREAT PLAINS",
                                                     "TROPICAL WET FORESTS",
                                                     "NORTHERN FORESTS"), "East",
                                    if_else(NA_L1NAME %in% c("NORTH AMERICAN DESERTS",
                                                             "SOUTHERN SEMI-ARID HIGHLANDS",
                                                             "TEMPERATE SIERRAS",
                                                             "MEDITERRANEAN CALIFORNIA",
                                                             "NORTHWESTERN FORESTED MOUNTAINS",
                                                             "MARINE WEST COAST FOREST"), "West", "Central")))) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  filter(DISCOVERY_YEAR > 2000) %>%
  filter(FIRE_SIZE >= 0.1)

names(fpa_wui) %<>% tolower

wuw_area <- data.table(class=c("WUI", "VLD", "Wildlands"), 
                       class_area = c(784320, 2260783, 2598246))

# Overall totals
totals_fpa <- fpa_wui %>% 
  as.data.frame(.) %>%
  group_by() %>%
  summarise(totfirefreq = n(),
            totfirearea = sum(fire_size_km2),
            totseasonlength = IQR(discovery_doy)) 

# Overall totals by CLASS
totals_by_class <- fpa_wui %>% 
  as.data.frame(.) %>%
  group_by(class) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            firesize = mean(fire_size_km2),
            firesize_sd = sd(fire_size_km2),
            firesize_90 = quantile(fire_size_km2, 0.90),
            seasonlength = IQR(discovery_doy)) %>%
  mutate(pct_firefreq = (firefreq/totals_fpa$totfirefreq)*100,
         pct_firearea = (firearea/totals_fpa$totfirearea)*100,
         pct_allfreq = (firefreq/totals_fpa$totfirefreq)*100)

totals_by_class_slim <- totals_by_class %>%
  mutate(totfirefreq = firefreq,
         totfirearea = firearea,
         totseasonlength = seasonlength) %>%
  dplyr::select(class, totfirefreq, totfirearea, totseasonlength)

# Overall totals by CAUSE
totals_by_cause <- fpa_wui %>% 
  as.data.frame(.) %>%
  group_by(ignition) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            seasonlength = IQR(discovery_doy)) %>%
  mutate(pct_firefreq = (firefreq/totals_fpa$totfirefreq)*100,
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
  group_by(class, ignition, na_l1name) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            seasonlength = IQR(discovery_doy),
            mediandoy = median(discovery_doy)) %>%
  left_join(., totals_by_class_slim, by = "class") %>%
  mutate(pct_firefreq = (firefreq/totfirefreq)*100,
         pct_firearea = (firearea/totfirearea)*100,
         pct_allfreq = (firefreq/totals_fpa$totfirefreq)*100)

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
            firesize = mean(fire_size_km2),
            seasonlength = IQR(discovery_doy),
            mediandoy = median(discovery_doy)) %>%
  left_join(., totals_by_year, by = "discovery_year") %>%
  mutate(pct_firefreq = (firefreq/totfirefreq)*100,
         pct_firearea = (firearea/totfirearea)*100)

# Overall totals by CLASS AND YEAR
totals_by_class_year <- fpa_wui %>% 
  as.data.frame(.) %>%
  group_by(discovery_year, class) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            firesize = mean(fire_size_km2),
            seasonlength = IQR(discovery_doy),
            mediandoy = median(discovery_doy)) %>%
  left_join(., totals_by_year, by = "discovery_year") %>%
  mutate(pct_firefreq = (firefreq/totfirefreq)*100,
         pct_firearea = (firearea/totfirearea)*100)

mean(totals_by_class_year$pct_firearea)

# Overall totals by CAUSE AND CLASS AND YEAR
totals_by_cause_class_year <- fpa_wui %>% 
  as.data.frame(.) %>%
  group_by(discovery_year, class, ignition) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            seasonlength = IQR(discovery_doy),
            mediandoy = median(discovery_doy)) %>%
  left_join(., totals_by_year, by = "discovery_year") %>%
  mutate(pct_firefreq = (firefreq/totfirefreq)*100,
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
            seasonlength = IQR(discovery_doy)) %>%
  left_join(., totals_fpa_seasonal, by = "seasons") %>%
  mutate(pct_firefreq = (firefreq/totfirefreq)*100,
         pct_firearea = (firearea/totfirearea)*100) 

# Overall totals by SEASON AND CAUSE
totals_fpa_cause_seasonal <- fpa_wui %>% 
  as.data.frame(.) %>%
  group_by(seasons, ignition) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            seasonlength = IQR(discovery_doy)) %>%
  left_join(., totals_fpa_seasonal, by = "seasons") %>%
  mutate(pct_firefreq = (firefreq/totfirefreq)*100,
         pct_firearea = (firearea/totfirearea)*100)

# Overall totals by CAUSE AND CLASS AND SEASON
totals_by_cause_class_season <- fpa_wui %>% 
  as.data.frame(.) %>%
  group_by(seasons, class, ignition) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            seasonlength = IQR(discovery_doy),
            mediandoy = median(discovery_doy)) %>%
  left_join(., totals_fpa_seasonal, by = "seasons") %>%
  mutate(pct_firefreq = (firefreq/totfirefreq)*100,
         pct_firearea = (firearea/totfirearea)*100) %>%
  filter(class == "Wildlands")

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
  transform(sizeclass = factor(sizeclass, levels=c("Small", "Large", "Very Large"))) %>%
  group_by(sizeclass) %>%
  summarise(totfirefreq = n(),
            totfirearea = sum(fire_size_km2)) 

# Overall totals by SIZE AND CLASS
totals_size_class <- fpa_wui %>% 
  as.data.frame(.) %>%
  mutate(sizeclass = classify_fire_size_cl(fire_size_km2)) %>%
  transform(sizeclass = factor(sizeclass, levels=c("Small", "Large", "Very Large"))) %>%
  group_by(sizeclass, class) %>%
  summarise(totfirefreq = n(),
            totfirearea = sum(fire_size_km2)) 

# Overall totals by SIZE AND CAUSE
totals_size_cause <- fpa_wui %>% 
  as.data.frame(.) %>%
  mutate(sizeclass = classify_fire_size_cl(fire_size_km2)) %>%
  transform(sizeclass = factor(sizeclass, levels=c("Small", "Large", "Very Large"))) %>%
  group_by(sizeclass, ignition) %>%
  summarise(totfirefreq = n(),
            totfirearea = sum(fire_size_km2)) 

# Overall totals by SIZE AND CAUSE AND CLASS
totals_by_size_cause_class <- fpa_wui %>%
  as.data.frame(.) %>%
  mutate(sizeclass = classify_fire_size_cl(fire_size_km2)) %>%
  transform(sizeclass = factor(sizeclass, levels=c("Small", "Large", "Very Large"))) %>%
  group_by(sizeclass, ignition, class) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2)) %>%
  mutate(pct_firefreq = (firefreq/totfirefreq)*100,
         pct_firearea = (firearea/totfirearea)*100) 

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
            firesize_sd = sd(fire_size_km2),
            seasonlength = IQR(discovery_doy)) %>%
  left_join(., totals_by_cause_class_slim, by = c("ignition", "class")) %>%
  mutate(pct_firefreq = (firefreq/totfirefreq)*100,
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
