
# FPA BAE database from 2001-2015
fpa_bae <- st_read(file.path(fpa_out, "fpa_mtbs_bae_wui.gpkg")) %>%
  filter(Class == "WUI" | Class == "VLD" | Class == "Wildlands") %>%
  filter(DISCOVERY_YEAR > 2000) %>%
  filter(FIRE_SIZE >= 0.1) %>%
  mutate(Seasons = classify_seasons(DISCOVERY_DOY),
         region = as.factor(if_else(NA_L1NAME %in% c("EASTERN TEMPERATE FORESTS",
                                                     "GREAT PLAINS",
                                                     "TROPICAL WET FORESTS",
                                                     "NORTHERN FORESTS"), "East",
                                    if_else(NA_L1NAME %in% c("NORTH AMERICAN DESERTS",
                                                             "SOUTHERN SEMIARID HIGHLANDS",
                                                             "TEMPERATE SIERRAS",
                                                             "MEDITERRANEAN CALIFORNIA",
                                                             "NORTHWESTERN FORESTED MOUNTAINS",
                                                             "MARINE WEST COAST FOREST"), "West", "Central"))),
         wui_area_km2 = as.numeric(st_area(geom))/1000000) %>%
  mutate_all(funs(replace(., is.na(.), 0)))

names(fpa_bae) %<>% tolower

wuw_area <- data.table(class=c("WUI", "VLD", "Wildlands"),
                       class_area = c(784320, 2260783, 2598246))

# Overall totals
totals_bae <- fpa_bae %>%
  as.data.frame(.) %>%
  group_by() %>%
  summarise(totfirearea = sum(wui_area_km2))

# Overall totals by CAUSE
bae_cause <- fpa_bae %>%
  as.data.frame(.) %>%
  group_by(ignition) %>%
  summarise(totfirearea = sum(wui_area_km2))

# Overall totals by CLASS
bae_class <- fpa_bae %>%
  as.data.frame(.) %>%
  group_by(class) %>%
  summarise(totfirearea = sum(wui_area_km2)) %>%
  left_join(., wuw_area, by = "class") %>%
  mutate(pct_firewui_tot = (totfirearea/class_area)*100)

# Overall totals by CLASS AND CAUSE
bae_class <- fpa_bae %>%
  as.data.frame(.) %>%
  group_by(class) %>%
  summarise(totfirearea = sum(wui_area_km2))

totals_by_cause <- fpa_bae %>%
  as.data.frame(.) %>%
  group_by(class, ignition) %>%
  summarise(firearea = sum(wui_area_km2),
            firesize = mean(wui_area_km2),
            firesize_sd = sd(wui_area_km2),
            firesize_90 = quantile(wui_area_km2, 0.90),
            seasonlength = IQR(discovery_doy)) %>%
  left_join(., wuw_area, by = "class") %>%
  left_join(., bae_class, by = "class") %>%
  mutate(pct_firearea = (firearea/totals_bae$totfirearea)*100,
         pct_firewui = (firearea/class_area)*100)

# Summarize FPA BAE data by YEAR
totals_by_wuiburn_year <- fpa_bae %>%
  as.data.frame(.) %>%
  group_by(class, discovery_year) %>%
  summarise(firearea = sum(wui_area_km2)) %>%
  left_join(., wuw_area, by = "class") %>%
  mutate(pct_firearea = (firearea/class_area)*100) %>%
  filter(class == "Wildlands")

mean(totals_by_wuiburn_year$pct_firearea)
mean(totals_by_wuiburn_year$firearea)

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
  filter(class == "WUI")

# Summarize FPA BAE data by CAUSE AND CLASS AND SIZE AND REGION
totals_bae <- fpa_bae %>%
  as.data.frame(.) %>%
  group_by(class, ignition) %>%
  summarise(totfirearea = sum(wui_area_km2))

totals_bae_class_cause <- fpa_bae %>%
  as.data.frame(.) %>%
  mutate(sizeclass = classify_fire_size_cl(fire_size_km2)) %>%
  transform(sizeclass = factor(sizeclass, levels = c("Small", "Large", "Very Large"))) %>%
  group_by(class, ignition, sizeclass, region) %>%
  summarise(n = n(),
            firearea = sum(wui_area_km2)) %>%
  left_join(., wuw_area, by = "class") %>%
  left_join(., totals_bae, by = c("class", "ignition")) %>%
  mutate(pct_firearea = (firearea/totfirearea)*100,
         pct_firewui = (firearea/class_area)*100) %>%
  filter(class == "WUI" & ignition == "Human")
