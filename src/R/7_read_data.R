source("src/functions/ggplot_theme.R")
source("src/functions/plot_theme.R")

# ICS 209 database from 2001-2013
wui_209 <- st_read(file.path(prefix, "fire/ics209/ics209_wui_conus.gpkg")) %>%
  mutate(Seasons = classify_seasons(sdoy)) %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") 

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

# FPA BAE database from 2001-2015
fpa_bae <- st_read(file.path(prefix, "fire/fpa-fod/fpa_mtbs_bae_wui.gpkg")) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  filter(DISCOVERY_YEAR > 2000) %>%
  filter(FIRE_SIZE >= 0.1) %>%
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
                                                           "MARINE WEST COAST FOREST"), "West", "Central"))),
         wui_area_km2 = as.numeric(st_area(geom))/1000000) %>%
  mutate_all(funs(replace(., is.na(.), 0))) 

names(wui_209) %<>% tolower
names(fpa_wui) %<>% tolower
names(fpa_bae) %<>% tolower

wuw_area <- data.table(class=c("WUI", "VLD", "Wildlands"), 
                       class_area = c(784320, 2260783, 2598246))

totals_bae <- fpa_bae %>% 
  as.data.frame(.) %>%
  group_by(class) %>%
  summarise(totfirearea = sum(wui_area_km2)) %>%
  left_join(., wuw_area, by = "class") %>%
  mutate(pct_firewui_tot = (totfirearea/class_area)*100) 

totals_by_cause <- fpa_bae %>% 
  as.data.frame(.) %>%
  mutate(sizeclass = classify_fire_size_cl(fire_size_km2)) %>%
  transform(sizeclass = factor(sizeclass, levels=c( "Small", "Large", "Very Large"))) %>%
  group_by(class, ignition, sizeclass) %>%
  summarise(firearea = sum(wui_area_km2),
            firesize = mean(wui_area_km2),
            firesize_sd = sd(wui_area_km2),
            firesize_90 = quantile(wui_area_km2, 0.90),
            seasonlength = IQR(discovery_doy)) %>%
  left_join(., wuw_area, by = "class") %>%
  left_join(., totals_bae, by = "class") %>%
  mutate(pct_firearea = (firearea/totfirearea)*100,
         pct_firewui = (firearea/class_area)*100) 

# Summarize FPA BAE data by YEAR
totals_by_wuiburn_year <- fpa_bae %>% 
  as.data.frame(.) %>%
  group_by(class, discovery_year) %>%
  summarise(firearea = sum(wui_area_km2)) %>%
  left_join(., wuw_area, by = "class") %>%
  mutate(pct_firearea = (firearea/class_area)*100) %>%
  filter(class == "VLD")

mean(totals_by_wuiburn_year$pct_firearea)

# Summarize FPA BAE data by CLASS
totals_bae_class_cause <- fpa_bae %>% 
  as.data.frame(.) %>%
  group_by(class) %>%
  summarise(firearea = sum(wui_area_km2)) %>%
  left_join(., wuw_area, by = "class") %>%
  mutate(pct_firearea = (firearea/class_area)*100) %>%
  filter(class == "VLD")

# Summarize FPA BAE data by CAUSE AND CLASS
totals_bae_class_cause <- fpa_bae %>% 
  as.data.frame(.) %>%
  group_by(class, ignition) %>%
  summarise(firearea = sum(wui_area_km2)) %>%
  left_join(., wuw_area, by = "class") %>%
  mutate(pct_firearea = (firearea/class_area)*100) %>%
  filter(class == "VLD")

# Summarize FPA BAE data by CAUSE AND CLASS AND SIZE
totals_bae <- fpa_bae %>% 
  as.data.frame(.) %>%
  group_by(class, ignition) %>%
  summarise(totfirearea = sum(wui_area_km2))

totals_bae_class_cause <- fpa_bae %>% 
  as.data.frame(.) %>%
  mutate(sizeclass = classify_fire_size_cl(fire_size_km2)) %>%
  transform(sizeclass = factor(sizeclass, levels = c("Small", "Large", "Very Large"))) %>%
  group_by(class, ignition, sizeclass) %>%
  summarise(firearea = sum(wui_area_km2)) %>%
  left_join(., wuw_area, by = "class") %>%
  left_join(., totals_bae, by = c("class", "ignition")) %>%
  mutate(pct_firearea = (firearea/totfirearea)*100,
         pct_firewui = (firearea/class_area)*100) %>% 
  filter(class == "VLD")

# Summarize FPA point data 
totals_fpa <- fpa_wui %>% 
  as.data.frame(.) %>%
  group_by() %>%
  summarise(totfirefreq = n(),
            totfirearea = sum(fire_size_km2),
            totseasonlength = IQR(discovery_doy)) 

# Summarize FPA point data by CLASS
totals_by_cause <- fpa_wui %>% 
  as.data.frame(.) %>%
  group_by(class) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            firesize = mean(fire_size_km2),
            firesize_sd = sd(fire_size_km2),
            firesize_90 = quantile(fire_size_km2, 0.90),
            seasonlength = IQR(discovery_doy)) %>%
  mutate(pct_firefreq = (firefreq/totals_fpa$totfirefreq)*100,
         pct_firearea = (firearea/totals_fpa$totfirearea)*100) 

# Summarize FPA point data by CAUSE
totals_by_cause <- fpa_wui %>% 
  as.data.frame(.) %>%
  group_by(ignition) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            firesize = mean(fire_size_km2),
            firesize_sd = sd(fire_size_km2),
            firesize_90 = quantile(fire_size_km2, 0.90),
            seasonlength = IQR(discovery_doy)) %>%
  mutate(pct_firefreq = (firefreq/totals_fpa$totfirefreq)*100,
         pct_firearea = (firearea/totals_fpa$totfirearea)*100) 

# Summarize FPA point data by CAUSE AND CLASS
totals_fpa_class <- fpa_wui %>% 
  as.data.frame(.) %>%
  group_by(class) %>%
  summarise(totfirefreq = n(),
            totfirearea = sum(fire_size_km2),
            totseasonlength = IQR(discovery_doy)) 

totals_by_cause_class <- fpa_wui %>% 
  as.data.frame(.) %>%
  group_by(class, ignition) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            firesize = mean(fire_size_km2),
            seasonlength = IQR(discovery_doy),
            mediandoy = median(discovery_doy)) %>%
  left_join(., totals_fpa_class, by = "class") %>%
  mutate(pct_firefreq = (firefreq/totfirefreq)*100,
         pct_firearea = (firearea/totfirearea)*100,
         pct_allfreq = (firefreq/totals_fpa$totfirefreq)*100)



# Summarize FPA point data by seasons
totals_fpa_seasonal <- fpa_wui %>% 
  as.data.frame(.) %>%
  group_by(ignition) %>%
  summarise(totfirefreq = n(),
            totfirearea = sum(fire_size_km2),
            totseasonlength = IQR(discovery_doy)) 

# Summarize FPA point data by CAUSE AND CLASS AND SEASON
totals_by_cause_class_season <- fpa_wui %>% 
  as.data.frame(.) %>%
  group_by(class, ignition, seasons) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            firesize = mean(fire_size_km2),
            seasonlength = IQR(discovery_doy),
            mediandoy = median(discovery_doy))  %>%
  filter(class == "VLD")
  
# Summarize FPA point data by CAUSE AND CLASS AND DISCOVERY DOY
# Will get at what is the most common discovery doy for human vs lightining ignitions
t1 <- fpa_wui %>% 
  as.data.frame(.) %>%
  group_by(class, ignition, discovery_doy) %>%
  summarise(doyfreq = n())


# What is the contribution of lots of small fires to the total burned area
totals_size <- fpa_wui %>% 
  as.data.frame(.) %>%
  mutate(sizeclass = classify_fire_size_cl(fire_size_km2)) %>%
  transform(sizeclass = factor(sizeclass, levels=c("Small", "Large", "Very Large"))) %>%
  group_by(sizeclass, class) %>%
  summarise(totfirefreq = n(),
            totfirearea = sum(fire_size_km2)) 

totals_by_sizeclass <- fpa_wui %>% 
  as.data.frame(.) %>%
  mutate(sizeclass = classify_fire_size_cl(fire_size_km2)) %>%
  transform(sizeclass = factor(sizeclass, levels=c("Small", "Large", "Very Large"))) %>%
  group_by(sizeclass, ignition, class) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            firesize = mean(fire_size_km2),
            firesize_sd = sd(fire_size_km2)) %>%
  left_join(., totals_size, by = c("ignition", "class")) %>%
  mutate(pct_firefreq = (firefreq/totfirefreq)*100,
         pct_firearea = (firearea/totfirearea)*100) %>%
filter(class == "VLD" & ignition == "Human")

# How different are the east and west in terms of ignitons and fire size?
totals_fpa <- fpa_wui %>% 
  as.data.frame(.) %>%
  mutate(sizeclass = classify_fire_size_cl(fire_size_km2)) %>%
  transform(sizeclass = factor(sizeclass, levels=c("Small", "Large", "Very Large"))) %>%
  group_by(sizeclass, ignition, class) %>%
  summarise(totfirefreq = n(),
            totfirearea = sum(fire_size_km2)) 

totals_by_sizeclass <- fpa_wui %>% 
  as.data.frame(.) %>%
  mutate(sizeclass = classify_fire_size_cl(fire_size_km2)) %>%
  transform(sizeclass = factor(sizeclass, levels=c("Small", "Large", "Very Large"))) %>%
  group_by(region, sizeclass, ignition, class) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            firesize_sd = sd(fire_size_km2),
            seasonlength = IQR(discovery_doy)) %>%
  left_join(., totals_fpa, by = c("sizeclass", "ignition", "class")) %>%
  mutate(pct_firefreq = (firefreq/totfirefreq)*100,
         pct_firearea = (firearea/totfirearea)*100)  %>%
  filter(class == "WUI" & ignition == "Human")


# How different are the east and west in terms of ignitons class?
totals_fpa <- fpa_wui %>% 
  as.data.frame(.) %>%
  group_by( ignition, class) %>%
  summarise(totfirefreq = n(),
            totfirearea = sum(fire_size_km2)) 

totals_by_sizeclass <- fpa_wui %>% 
  as.data.frame(.) %>%
  group_by(region, ignition, class) %>%
  summarise(firefreq = n(),
            firearea = sum(fire_size_km2),
            firesize_sd = sd(fire_size_km2),
            seasonlength = IQR(discovery_doy)) %>%
  left_join(., totals_fpa, by = c("ignition", "class")) %>%
  mutate(pct_firefreq = (firefreq/totfirefreq)*100,
         pct_firearea = (firearea/totfirearea)*100)  %>%
  filter(class == "VLD" & ignition == "Human")



classify_fire_size_cl <-  function(x) {
  # break out fires into small, med, large
  # input:
  #   - x: vector of fire sizes
  # output:
  #   - y: vector (same length) of classified fire sizes ----- Km2
  ifelse(x < 4, "Small",
         ifelse(x >= 4 & x < 500, "Large", "Very Large"))
}


fpa_wui %>%
  as.data.frame(.) %>%
  mutate(sizeclass = classify_fire_size_cl(fire_size_km2)) %>%
  transform(sizeclass = factor(sizeclass, levels=c("Small", "Large", "Very Large"))) %>%
  ggplot(aes(x = sizeclass, y = log(fire_size_km2), color = ignition, fill = ignition)) +
  geom_boxplot() + 
  theme_pub() +
  facet_wrap(~ class, ncol = 3)
