source("src/functions/ggplot_theme.R")
source("src/functions/plot_theme.R")

# ICS 209 database from 2001-2013
wui_209 <- st_read("data/fire/ics209/ics209_wui_conus.gpkg") %>%
  mutate(Seasons = classify_seasons(sdoy)) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") 

names(wui_209) %<>% tolower

# FPA database from 2001-2015
fpa_wui <- st_read("data/fire/fpa-fod/fpa_wui_conus.gpkg") %>%
  mutate(Seasons = classify_seasons(DISCOVERY_DOY)) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  filter(DISCOVERY_YEAR > 2000) %>%
  filter(FIRE_SIZE >= 0.1)


fpa_wui <- fpa_wui %>%
  mutate(region = as.factor(ifelse(na_l1name %in% c("EASTERN TEMPERATE FORESTS",
                                                    "GREAT PLAINS",
                                                    "TROPICAL WET FORESTS",
                                                    "NORTHERN FORESTS"), "East",
                                   ifelse(na_l1name %in% c("NORTH AMERICAN DESERTS",
                                                           "SOUTHERN SEMI-ARID HIGHLANDS",
                                                           "TEMPERATE SIERRAS",
                                                           "MEDITERRANEAN CALIFORNIA",
                                                           "NORTHWESTERN FORESTED MOUNTAINS",
                                                           "MARINE WEST COAST FOREST"), "West", "Central"))))

names(fpa_wui) %<>% tolower

totals_fpa <- fpa_wui %>% 
  as.data.frame(.) %>%
  group_by() %>%
  summarise(totfirefreq = n(),
            totfirearea = sum(fire_size_km2),
            totseasonlength = IQR(discovery_doy)) 

totals_by_cause <- fpa_wui %>% 
  as.data.frame(.) %>%
  mutate(sizeclass = classify_fire_size_cl(fire_size_km2)) %>%
  transform(sizeclass = factor(sizeclass, levels=c("Very Small", "Small", "Medium", "Large", "Very Large"))) %>%
  group_by(ignition) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            firesize = mean(fire_size_km2),
            firesize_sd = sd(fire_size_km2),
            firesize_min = min(fire_size_km2),
            firesize_max = max(fire_size_km2),
            firesize_90 = quantile(fire_size_km2, 0.90),
            seasonlength = IQR(discovery_doy)) %>%
  mutate(pct_firefreq = (firefreq/totals_fpa$totfirefreq)*100,
         pct_firearea = (firearea/totals_fpa$totfirearea)*100) 
  
totals_by_cause_class_size <- fpa_wui %>% 
  as.data.frame(.) %>%
  # mutate(sizeclass = classify_fire_size_cl(fire_size_km2)) %>%
  # transform(sizeclass = factor(sizeclass, levels=c("Very Small", "Small", "Medium", "Large", "Very Large"))) %>%
  group_by(class, ignition) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            firesize = mean(fire_size_km2),
            seasonlength = IQR(discovery_doy)) %>%
  mutate(pct_firefreq = (firefreq/totals_fpa$totfirefreq)*100,
         pct_firearea = (firearea/totals_fpa$totfirearea)*100)


# What is the contribution of lots of small fires to the total burned area
totals_fpa <- fpa_wui %>% 
  as.data.frame(.) %>%
  mutate(sizeclass = classify_fire_size_cl(fire_size_km2)) %>%
  transform(sizeclass = factor(sizeclass, levels=c("Very Small", "Small", "Medium", "Large", "Very Large"))) %>%
  group_by() %>%
  summarise(totfirefreq = n(),
            totfirearea = sum(fire_size_km2),
            totseasonlength = IQR(discovery_doy)) 

totals_by_sizeclass <- fpa_wui %>% 
  as.data.frame(.) %>%
  mutate(sizeclass = classify_fire_size_cl(fire_size_km2)) %>%
  transform(sizeclass = factor(sizeclass, levels=c("Very Small", "Small", "Medium", "Large", "Very Large"))) %>%
  group_by(sizeclass,ignition) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            firesize = mean(fire_size_km2),
            firesize_sd = sd(fire_size_km2),
            firesize_min = min(fire_size_km2),
            firesize_max = max(fire_size_km2),
            firesize_90 = quantile(fire_size_km2, 0.90),
            seasonlength = IQR(discovery_doy)) %>%
  mutate(pct_firefreq = (firefreq/totals_fpa$totfirefreq)*100,
         pct_firearea = (firearea/totals_fpa$totfirearea)*100) 

# How different are the east and west in terms of ignitons and fire size?
totals_fpa <- fpa_wui %>% 
  as.data.frame(.) %>%
  mutate(sizeclass = classify_fire_size_cl(fire_size_km2)) %>%
  transform(sizeclass = factor(sizeclass, levels=c("Very Small", "Small", "Medium", "Large", "Very Large"))) %>%
  group_by() %>%
  summarise(totfirefreq = n(),
            totfirearea = sum(fire_size_km2),
            totseasonlength = IQR(discovery_doy)) 

totals_by_sizeclass <- fpa_wui %>% 
  as.data.frame(.) %>%
  mutate(sizeclass = classify_fire_size_cl(fire_size_km2)) %>%
  transform(sizeclass = factor(sizeclass, levels=c("Very Small", "Small", "Medium", "Large", "Very Large"))) %>%
  group_by(region, ignition) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            firesize = mean(fire_size_km2),
            firesize_sd = sd(fire_size_km2),
            firesize_min = min(fire_size_km2),
            firesize_max = max(fire_size_km2),
            firesize_90 = quantile(fire_size_km2, 0.90),
            seasonlength = IQR(discovery_doy)) %>%
  mutate(pct_firefreq = (firefreq/totals_fpa$totfirefreq)*100,
         pct_firearea = (firearea/totals_fpa$totfirearea)*100) 


